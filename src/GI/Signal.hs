module GI.Signal
    ( genSignal
    , genCallback
    , signalHaskellName
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, forM_, when, unless)

import Data.Typeable (typeOf)
import Data.Bool (bool)
import qualified Data.Text as T
import Data.Text (Text)

import Text.Show.Pretty (ppShow)

import GI.API
import GI.Callable (hOutType, arrayLengths, wrapMaybe, fixupCallerAllocates)
import GI.Code
import GI.Conversions
import GI.SymbolNaming
import GI.Transfer (freeContainerType)
import GI.Type
import GI.Util (parenthesize, withComment, tshow, terror, ucFirst, lcFirst,
                prime)

-- The prototype of the callback on the Haskell side (what users of
-- the binding will see)
genHaskellCallbackPrototype :: Text -> Callable -> Text -> [Arg] -> [Arg] ->
                               ExcCodeGen ()
genHaskellCallbackPrototype subsec cb name' hInArgs hOutArgs = do
  group $ do
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
  group $ do
    exportSignal subsec ("no" <> name')
    line $ "no" <> name' <> " :: Maybe " <> name'
    line $ "no" <> name' <> " = Nothing"

-- Prototype of the callback on the C side
genCCallbackPrototype :: Text -> Callable -> Text -> Bool -> CodeGen ()
genCCallbackPrototype subsec cb name' isSignal =
  group $ do
    let ctypeName = name' <> "C"
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

-- Generator for wrappers callable from C
genCallbackWrapperFactory :: Text -> Text -> CodeGen ()
genCallbackWrapperFactory subsec name' =
  group $ do
    let factoryName = "mk" <> name'
    line "foreign import ccall \"wrapper\""
    indent $ line $ factoryName <> " :: "
               <> name' <> "C -> IO (FunPtr " <> name' <> "C)"
    exportSignal subsec factoryName

-- Generator of closures
genClosure :: Text -> Text -> Text -> Bool -> CodeGen ()
genClosure subsec callback closure isSignal = do
  exportSignal subsec closure
  group $ do
      line $ closure <> " :: " <> callback <> " -> IO Closure"
      line $ closure <> " cb = newCClosure =<< mk" <> callback <> " wrapped"
      indent $
         line $ "where wrapped = " <> lcFirst callback <> "Wrapper " <>
              if isSignal
              then "cb"
              else "Nothing cb"

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

-- The wrapper itself, marshalling to and from Haskell. The first
-- argument is possibly a pointer to a FunPtr to free (via
-- freeHaskellFunPtr) once the callback is run once, or Nothing if the
-- FunPtr will be freed by someone else (the function registering the
-- callback for ScopeTypeCall, or a destroy notifier for
-- ScopeTypeNotified).
genCallbackWrapper :: Text -> Callable -> Text -> [Arg] -> [Arg] -> [Arg] ->
                      Bool -> ExcCodeGen ()
genCallbackWrapper subsec cb name' dataptrs hInArgs hOutArgs isSignal = do
  let cName arg = if arg `elem` dataptrs
                  then "_"
                  else escapedArgName arg
      cArgNames = map cName (args cb)
      wrapperName = lcFirst name' <> "Wrapper"

  exportSignal subsec wrapperName

  group $ do
    line $ wrapperName <> " ::"
    indent $ do
      unless isSignal $
           line $ "Maybe (Ptr (FunPtr (" <> name' <> "C))) ->"
      line $ name' <> " ->"
      when isSignal $ line "Ptr () ->"
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

    let allArgs = if isSignal
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
genCallback n (Callback cb) = submodule "Callbacks" $ do
  name' <- upperName n
  line $ "-- callback " <> name'

  let -- user_data pointers, which we generically omit
      dataptrs = map (args cb !!) . filter (/= -1) . map argClosure $ args cb
      hidden = dataptrs <> arrayLengths cb

      inArgs = filter ((/= DirectionOut) . direction) $ args cb
      hInArgs = filter (not . (`elem` hidden)) inArgs
      outArgs = filter ((/= DirectionIn) . direction) $ args cb
      hOutArgs = filter (not . (`elem` hidden)) outArgs

  if skipReturn cb
  then group $ do
    line $ "-- XXX Skipping callback " <> name'
    line $ "-- Callbacks skipping return unsupported :\n"
             <> T.pack (ppShow n) <> "\n" <> T.pack (ppShow cb)
  else do
    let closure = lcFirst name' <> "Closure"
        cb' = fixupCallerAllocates cb

    handleCGExc (\e -> line ("-- XXX Could not generate callback wrapper for "
                             <> name' <>
                             "\n-- Error was : " <> describeCGError e))
       (genClosure name' name' closure False >>
        genCCallbackPrototype name' cb' name' False >>
        genCallbackWrapperFactory name' name' >>
        genHaskellCallbackPrototype name' cb' name' hInArgs hOutArgs >>
        genCallbackWrapper name' cb' name' dataptrs hInArgs hOutArgs False)

-- | Return the name for the signal in Haskell CamelCase conventions.
signalHaskellName :: Text -> Text
signalHaskellName sn = let (w:ws) = T.split (== '-') sn
                       in w <> T.concat (map ucFirst ws)

genSignal :: Signal -> Name -> ExcCodeGen ()
genSignal (Signal { sigName = sn, sigCallable = cb }) on = do
  on' <- upperName on

  line $ "-- signal " <> on' <> "::" <> sn

  let inArgs = filter ((/= DirectionOut) . direction) $ args cb
      hInArgs = filter (not . (`elem` arrayLengths cb)) inArgs
      outArgs = filter ((/= DirectionIn) . direction) $ args cb
      hOutArgs = filter (not . (`elem` arrayLengths cb)) outArgs
      sn' = signalHaskellName (sn)
      signalConnectorName = on' <> ucFirst sn'
      cbType = signalConnectorName <> "Callback"

  genHaskellCallbackPrototype (ucFirst sn') cb cbType hInArgs hOutArgs

  genCCallbackPrototype (ucFirst sn') cb cbType True

  genCallbackWrapperFactory (ucFirst sn') cbType

  let closure = lcFirst signalConnectorName <> "Closure"
  genClosure (ucFirst sn') cbType closure True

  genCallbackWrapper (ucFirst sn') cb cbType [] hInArgs hOutArgs True

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
        line $ "cb' <- mk" <> cbType <> " (" <> lcFirst cbType <> "Wrapper cb)"
        line $ "connectSignalFunPtr obj \"" <> sn <> "\" cb' after"
