module Data.GI.CodeGen.Signal
    ( genSignal
    , genCallback
    , signalHaskellName
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, forM_, when, unless)

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Typeable (typeOf)
import Data.Bool (bool)
import qualified Data.Text as T
import Data.Text (Text)

import Text.Show.Pretty (ppShow)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Callable (hOutType, wrapMaybe,
                                 fixupCallerAllocates,
                                 genDynamicCallableWrapper, ExposeClosures(..),
                                 callableHInArgs, callableHOutArgs)
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.SymbolNaming
import Data.GI.CodeGen.Transfer (freeContainerType)
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util (parenthesize, withComment, tshow, terror,
                             ucFirst, prime)

-- The prototype of the callback on the Haskell side (what users of
-- the binding will see)
genHaskellCallbackPrototype :: Text -> Callable -> Text -> ExposeClosures ->
                               ExcCodeGen ()
genHaskellCallbackPrototype subsec cb htype expose = group $ do
    let name' = case expose of
                  WithClosures -> callbackHTypeWithClosures htype
                  WithoutClosures -> htype
        (hInArgs, _) = callableHInArgs cb expose
        hOutArgs = callableHOutArgs cb

    exportSignal subsec name'
    line $ "type " <> name' <> " ="
    indent $ do
      forM_ hInArgs $ \arg -> do
        ht <- haskellType (argType arg)
        wrapMaybe arg >>= bool
                          (line $ tshow ht <> " ->")
                          (line $ tshow (maybeT ht) <> " ->")
      ret <- hOutType cb hOutArgs False
      line $ tshow $ io ret

    -- For optional parameters, in case we want to pass Nothing.
    exportSignal subsec ("no" <> name')
    line $ "no" <> name' <> " :: Maybe " <> name'
    line $ "no" <> name' <> " = Nothing"

-- | Generate the type synonym for the prototype of the callback on
-- the C side. Returns the name given to the type synonym.
genCCallbackPrototype :: Text -> Callable -> Text -> Bool -> CodeGen Text
genCCallbackPrototype subsec cb name' isSignal = group $ do
    let ctypeName = callbackCType name'
    exportSignal subsec ctypeName

    line $ "type " <> ctypeName <> " ="
    indent $ do
      when isSignal $ line $ withComment "Ptr () ->" "object"
      forM_ (args cb) $ \arg -> do
        ht <- foreignType $ argType arg
        let ht' = if direction arg /= DirectionIn
                  then ptr ht
                  else ht
        line $ tshow ht' <> " ->"
      when isSignal $ line $ withComment "Ptr () ->" "user_data"
      ret <- io <$> case returnType cb of
                      Nothing -> return $ typeOf ()
                      Just t -> foreignType t
      line $ tshow ret
    return ctypeName

-- Generator for wrappers callable from C
genCallbackWrapperFactory :: Text -> Text -> CodeGen ()
genCallbackWrapperFactory subsec name' = group $ do
    let factoryName = callbackWrapperAllocator name'
    line "foreign import ccall \"wrapper\""
    indent $ line $ factoryName <> " :: " <> callbackCType name'
               <> " -> IO (FunPtr " <> callbackCType name' <> ")"
    exportSignal subsec factoryName

-- | Wrap the Haskell `cb` callback into a foreign function of the
-- right type. Returns the name of the wrapped value.
genWrappedCallback :: Callable -> Text -> Text -> Bool -> CodeGen Text
genWrappedCallback cb cbArg callback isSignal = do
  drop <- if callableHasClosures cb
          then do
            let arg' = prime cbArg
            line $ "let " <> arg' <> " = "
                     <> callbackDropClosures callback <> " " <> cbArg
            return arg'
          else return cbArg
  line $ "let " <> prime drop <> " = " <> callbackHaskellToForeign callback <>
       if isSignal
       then " " <> drop
       else " Nothing " <> drop
  return (prime drop)

-- | Generator of closures
genClosure :: Text -> Callable -> Text -> Text -> Bool -> CodeGen ()
genClosure subsec cb callback name isSignal = group $ do
  let closure = callbackClosureGenerator name
  exportSignal subsec closure
  group $ do
      line $ closure <> " :: " <> callback <> " -> IO Closure"
      line $ closure <> " cb = do"
      indent $ do
            wrapped <- genWrappedCallback cb "cb" callback isSignal
            line $ callbackWrapperAllocator callback <> " " <> wrapped
                     <> " >>= newCClosure"

-- Wrap a conversion of a nullable object into "Maybe" object, by
-- checking whether the pointer is NULL.
convertNullable :: Text -> BaseCodeGen e Text -> BaseCodeGen e Text
convertNullable aname c = do
  line $ "maybe" <> ucFirst aname <> " <-"
  indent $ do
    line $ "if " <> aname <> " == nullPtr"
    line   "then return Nothing"
    line   "else do"
    indent $ do
             unpacked <- c
             line $ "return $ Just " <> unpacked
    return $ "maybe" <> ucFirst aname

-- Convert a non-zero terminated out array, stored in a variable
-- named "aname", into the corresponding Haskell object.
convertCallbackInCArray :: Callable -> Arg -> Type -> Text -> ExcCodeGen Text
convertCallbackInCArray callable arg t@(TCArray False (-1) length _) aname =
  if length > -1
  then wrapMaybe arg >>= bool convertAndFree
                         (convertNullable aname convertAndFree)
  else
    -- Not much we can do, we just pass the pointer along, and let
    -- the callback deal with it.
    return aname
  where
    lname = escapedArgName $ args callable !! length

    convertAndFree :: ExcCodeGen Text
    convertAndFree = do
      unpacked <- convert aname $ unpackCArray lname t (transfer arg)
      -- Free the memory associated with the array
      freeContainerType (transfer arg) t aname lname
      return unpacked

-- Remove the warning, this should never be reached.
convertCallbackInCArray _ t _ _ =
    terror $ "convertOutCArray : unexpected " <> tshow t

-- Prepare an argument for passing into the Haskell side.
prepareArgForCall :: Callable -> Arg -> ExcCodeGen Text
prepareArgForCall cb arg = case direction arg of
  DirectionIn -> prepareInArg cb arg
  DirectionInout -> prepareInoutArg arg
  DirectionOut -> terror "Unexpected DirectionOut!"

prepareInArg :: Callable -> Arg -> ExcCodeGen Text
prepareInArg cb arg = do
  let name = escapedArgName arg
  case argType arg of
    t@(TCArray False _ _ _) -> convertCallbackInCArray cb arg t name
    _ -> do
      let c = convert name $ fToH (argType arg) (transfer arg)
      wrapMaybe arg >>= bool c (convertNullable name c)

prepareInoutArg :: Arg -> ExcCodeGen Text
prepareInoutArg arg = do
  let name = escapedArgName arg
  name' <- genConversion name $ apply $ M "peek"
  convert name' $ fToH (argType arg) (transfer arg)

saveOutArg :: Arg -> ExcCodeGen ()
saveOutArg arg = do
  let name = escapedArgName arg
      name' = "out" <> name
  when (transfer arg /= TransferEverything) $
       notImplementedError $ "Unexpected transfer type for \"" <> name <> "\""
  isMaybe <- wrapMaybe arg
  name'' <- if isMaybe
            then do
              let name'' = prime name'
              line $ name'' <> " <- case " <> name' <> " of"
              indent $ do
                   line "Nothing -> return nullPtr"
                   line $ "Just " <> name'' <> " -> do"
                   indent $ do
                         converted <- convert name'' $ hToF (argType arg) TransferEverything
                         line $ "return " <> converted
              return name''
            else convert name' $ hToF (argType arg) TransferEverything
  line $ "poke " <> name <> " " <> name''

-- | A simple wrapper that drops every closure argument.
genDropClosures :: Text -> Callable -> Text -> CodeGen ()
genDropClosures subsec cb name' = group $ do
  let dropper = callbackDropClosures name'
      (inWithClosures, _) = callableHInArgs cb WithClosures
      (inWithoutClosures, _) = callableHInArgs cb WithoutClosures
      passOrIgnore = \arg -> if arg `elem` inWithoutClosures
                             then Just (escapedArgName arg)
                             else Nothing
      argNames = map (maybe "_" id . passOrIgnore) inWithClosures

  exportSignal subsec dropper

  line $ dropper <> " :: " <> name' <> " -> " <> callbackHTypeWithClosures name'
  line $ dropper <> " _f " <> T.unwords argNames <> " = _f "
           <> T.unwords (catMaybes (map passOrIgnore inWithClosures))

-- The wrapper itself, marshalling to and from Haskell. The first
-- argument is possibly a pointer to a FunPtr to free (via
-- freeHaskellFunPtr) once the callback is run once, or Nothing if the
-- FunPtr will be freed by someone else (the function registering the
-- callback for ScopeTypeCall, or a destroy notifier for
-- ScopeTypeNotified).
genCallbackWrapper :: Text -> Callable -> Text -> Bool -> ExcCodeGen ()
genCallbackWrapper subsec cb name' isSignal = group $ do
  let wrapperName = callbackHaskellToForeign name'
      (hInArgs, _) = callableHInArgs cb WithClosures
      hOutArgs = callableHOutArgs cb

  exportSignal subsec wrapperName

  group $ do
    line $ wrapperName <> " ::"
    indent $ do
      if isSignal
      then do
        line $ name' <> " ->"
        line "Ptr () ->"
      else do
           line $ "Maybe (Ptr (FunPtr " <> callbackCType name' <> ")) ->"
           let hType = if callableHasClosures cb
                       then callbackHTypeWithClosures name'
                       else name'
           line $ hType <> " ->"

      forM_ (args cb) $ \arg -> do
        ht <- foreignType $ argType arg
        let ht' = if direction arg /= DirectionIn
                  then ptr ht
                  else ht
        line $ tshow ht' <> " ->"
      when isSignal $ line "Ptr () ->"
      ret <- io <$> case returnType cb of
                      Nothing -> return $ typeOf ()
                      Just t -> foreignType t
      line $ tshow ret

    let cArgNames = map escapedArgName (args cb)
        allArgs = if isSignal
                  then T.unwords $ ["_cb", "_"] <> cArgNames <> ["_"]
                  else T.unwords $ ["funptrptr", "_cb"] <> cArgNames
    line $ wrapperName <> " " <> allArgs <> " = do"
    indent $ do
      hInNames <- forM hInArgs (prepareArgForCall cb)

      let maybeReturn = case returnType cb of
                          Nothing -> []
                          _       -> ["result"]
          returnVars = maybeReturn <> map (("out"<>) . escapedArgName) hOutArgs
          returnBind = case returnVars of
                         []  -> ""
                         [r] -> r <> " <- "
                         _   -> parenthesize (T.intercalate ", " returnVars) <> " <- "
      line $ returnBind <> "_cb " <> T.concat (map (" " <>) hInNames)

      forM_ hOutArgs saveOutArg

      unless isSignal $ line "maybeReleaseFunPtr funptrptr"

      case returnType cb of
        Nothing -> return ()
        Just r -> do
           nullableReturnType <- typeIsNullable r
           if returnMayBeNull cb && nullableReturnType
           then do
             line "maybeM nullPtr result $ \\result' -> do"
             indent $ unwrapped "result'"
           else unwrapped "result"
           where
             unwrapped rname = do
               result' <- convert rname $ hToF r (returnTransfer cb)
               line $ "return " <> result'

genCallback :: Name -> Callback -> CodeGen ()
genCallback n (Callback cb) = do
  let name' = upperName n
  line $ "-- callback " <> name'
  line $ "--          -> " <> tshow (fixupCallerAllocates cb)

  if skipReturn cb
  then group $ do
    line $ "-- XXX Skipping callback " <> name'
    line $ "-- Callbacks skipping return unsupported :\n"
             <> T.pack (ppShow n) <> "\n" <> T.pack (ppShow cb)
  else do
    let cb' = fixupCallerAllocates cb

    handleCGExc (\e -> line ("-- XXX Could not generate callback wrapper for "
                             <> name' <>
                             "\n-- Error was : " <> describeCGError e)) $ do
      genClosure name' cb' name' name' False
      typeSynonym <- genCCallbackPrototype name' cb' name' False
      dynamic <- genDynamicCallableWrapper n typeSynonym cb False
      exportSignal name' dynamic
      genCallbackWrapperFactory name' name'
      line $ deprecatedPragma name' (callableDeprecated cb')
      genHaskellCallbackPrototype name' cb' name' WithoutClosures
      when (callableHasClosures cb') $ do
           genHaskellCallbackPrototype name' cb' name' WithClosures
           genDropClosures name' cb' name'
      genCallbackWrapper name' cb' name' False

-- | Return the name for the signal in Haskell CamelCase conventions.
signalHaskellName :: Text -> Text
signalHaskellName sn = let (w:ws) = T.split (== '-') sn
                       in w <> T.concat (map ucFirst ws)

genSignal :: Signal -> Name -> ExcCodeGen ()
genSignal (Signal { sigName = sn, sigCallable = cb }) on = do
  let on' = upperName on

  line $ "-- signal " <> on' <> "::" <> sn

  let sn' = signalHaskellName (sn)
      signalConnectorName = on' <> ucFirst sn'
      cbType = signalConnectorName <> "Callback"

  line $ deprecatedPragma cbType (callableDeprecated cb)
  genHaskellCallbackPrototype (ucFirst sn') cb cbType WithoutClosures

  _ <- genCCallbackPrototype (ucFirst sn') cb cbType True

  genCallbackWrapperFactory (ucFirst sn') cbType

  genClosure (ucFirst sn') cb cbType signalConnectorName True

  genCallbackWrapper (ucFirst sn') cb cbType True

  -- Wrapper for connecting functions to the signal
  -- We can connect to a signal either before the default handler runs
  -- ("on...") or after the default handler runs (after...). We
  -- provide convenient wrappers for both cases.
  group $ do
    let signatureConstraints = "(GObject a, MonadIO m) =>"
        signatureArgs = "a -> " <> cbType <> " -> m SignalHandlerId"
        signature = " :: " <> signatureConstraints <> " " <> signatureArgs
        onName = "on" <> signalConnectorName
        afterName = "after" <> signalConnectorName
    line $ onName <> signature
    line $ onName <> " obj cb = liftIO $ connect"
             <> signalConnectorName <> " obj cb SignalConnectBefore"
    line $ afterName <> signature
    line $ afterName <> " obj cb = connect"
             <> signalConnectorName <> " obj cb SignalConnectAfter"
    exportSignal (ucFirst sn') onName
    exportSignal (ucFirst sn') afterName

  group $ do
    let fullName = "connect" <> signalConnectorName
        signatureConstraints = "(GObject a, MonadIO m) =>"
        signatureArgs = "a -> " <> cbType
                        <> " -> SignalConnectMode -> m SignalHandlerId"
    line $ fullName <> " :: " <> signatureConstraints
    line $ T.replicate (4 + T.length fullName) " " <> signatureArgs
    line $ fullName <> " obj cb after = liftIO $ do"
    indent $ do
        cb' <- genWrappedCallback cb "cb" cbType True
        let cb'' = prime cb'
        line $ cb'' <> " <- " <> callbackWrapperAllocator cbType <> " " <> cb'
        line $ "connectSignalFunPtr obj \"" <> sn <> "\" " <> cb'' <> " after"
