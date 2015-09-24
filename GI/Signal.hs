module GI.Signal
    ( genSignal
    , genCallback
    , signalHaskellName
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, forM_, when, unless)

import Data.List (intercalate)
import Data.Typeable (typeOf)
import Data.Bool (bool)
import qualified Data.Text as T

import Text.Show.Pretty (ppShow)

import GI.API
import GI.Callable (hOutType, arrayLengths, wrapMaybe)
import GI.Code
import GI.Conversions
import GI.SymbolNaming
import GI.Transfer (freeContainerType)
import GI.Type
import GI.Util (split, parenthesize, withComment)

-- The prototype of the callback on the Haskell side (what users of
-- the binding will see)
genHaskellCallbackPrototype :: Callable -> String -> [Arg] -> [Arg] ->
                               ExcCodeGen ()
genHaskellCallbackPrototype cb name' hInArgs hOutArgs = do
  group $ do
    line $ "type " ++ name' ++ " ="
    indent $ do
      forM_ hInArgs $ \arg -> do
        ht <- haskellType (argType arg)
        wrapMaybe arg >>= bool
                          (line $ show ht ++ " ->")
                          (line $ show (maybeT ht) ++ " ->")
      ret <- hOutType cb hOutArgs False
      line $ show $ io ret

  -- For optional parameters, in case we want to pass Nothing.
  group $ do
    line $ "no" ++ name' ++ " :: Maybe " ++ name'
    line $ "no" ++ name' ++ " = Nothing"

-- Prototype of the callback on the C side
genCCallbackPrototype :: Callable -> String -> Bool -> CodeGen ()
genCCallbackPrototype cb name' isSignal =
  group $ do
    line $ "type " ++ name' ++ "C ="
    indent $ do
      when isSignal $ line $ withComment "Ptr () ->" "object"
      forM_ (args cb) $ \arg -> do
        ht <- foreignType $ argType arg
        let ht' = if direction arg /= DirectionIn
                  then ptr ht
                  else ht
        line $ show ht' ++ " ->"
      when isSignal $ line $ withComment "Ptr () ->" "user_data"
      ret <- io <$> case returnType cb of
                      TBasicType TVoid -> return $ typeOf ()
                      t -> foreignType t
      line $ show ret

-- Generator for wrappers callable from C
genCallbackWrapperFactory :: String -> CodeGen ()
genCallbackWrapperFactory name' =
  group $ do
    line "foreign import ccall \"wrapper\""
    indent $ line $ "mk" ++ name' ++ " :: "
               ++ name' ++ "C -> IO (FunPtr " ++ name' ++ "C)"

-- Generator of closures
genClosure :: String -> String -> Bool -> CodeGen ()
genClosure callback closure isSignal =
    group $ do
      line $ closure ++ " :: " ++ callback ++ " -> IO Closure"
      line $ closure ++ " cb = newCClosure =<< mk" ++ callback ++ " wrapped"
      indent $
         line $ "where wrapped = " ++ lcFirst callback ++ "Wrapper " ++
              if isSignal
              then "cb"
              else "Nothing cb"

-- Wrap a conversion of a nullable object into "Maybe" object, by
-- checking whether the pointer is NULL.
convertNullable :: String -> BaseCodeGen e String -> BaseCodeGen e String
convertNullable aname c = do
  line $ "maybe" ++ ucFirst aname ++ " <-"
  indent $ do
    line $ "if " ++ aname ++ " == nullPtr"
    line   "then return Nothing"
    line   "else do"
    indent $ do
             unpacked <- c
             line $ "return $ Just " ++ unpacked
    return $ "maybe" ++ ucFirst aname

-- Convert a non-zero terminated out array, stored in a variable
-- named "aname", into the corresponding Haskell object.
convertCallbackInCArray :: Callable -> Arg -> Type -> String -> ExcCodeGen String
convertCallbackInCArray callable arg t@(TCArray False (-1) length _) aname =
  if length > -1
  then wrapMaybe arg >>= bool convertAndFree
                         (convertNullable aname convertAndFree)
  else
    -- Not much we can do, we just pass the pointer along, and let
    -- the callback deal with it.
    return aname
  where
    lname = escapeReserved $ argName $ args callable !! length

    convertAndFree :: ExcCodeGen String
    convertAndFree = do
      unpacked <- convert aname $ unpackCArray lname t (transfer arg)
      -- Free the memory associated with the array
      freeContainerType (transfer arg) t aname lname
      return unpacked

-- Remove the warning, this should never be reached.
convertCallbackInCArray _ t _ _ =
    error $ "convertOutCArray : unexpected " ++ show t

-- Prepare an argument for passing into the Haskell side.
prepareArgForCall :: Callable -> Arg -> ExcCodeGen String
prepareArgForCall cb arg = case direction arg of
  DirectionIn -> prepareInArg cb arg
  DirectionInout -> prepareInoutArg arg
  DirectionOut -> error "Unexpected DirectionOut!"

prepareInArg :: Callable -> Arg -> ExcCodeGen String
prepareInArg cb arg = do
  let name = (escapeReserved . argName) arg
  case argType arg of
    t@(TCArray False _ _ _) -> convertCallbackInCArray cb arg t name
    _ -> do
      let c = convert name $ fToH (argType arg) (transfer arg)
      wrapMaybe arg >>= bool c (convertNullable name c)

prepareInoutArg :: Arg -> ExcCodeGen String
prepareInoutArg arg = do
  let name = (escapeReserved . argName) arg
  name' <- genConversion name $ apply $ M "peek"
  convert name' $ fToH (argType arg) (transfer arg)

saveOutArg :: Arg -> ExcCodeGen ()
saveOutArg arg = do
  let name = (escapeReserved . argName) arg
      name' = "out" ++ name
  when (transfer arg /= TransferEverything) $
       notImplementedError $ "Unexpected transfer type for \"" ++ name ++ "\""
  name'' <- convert name' $ hToF (argType arg) TransferEverything
  line $ "poke " ++ name ++ " " ++ name''

-- The wrapper itself, marshalling to and from Haskell. The first
-- argument is possibly a pointer to a FunPtr to free (via
-- freeHaskellFunPtr) once the callback is run once, or Nothing if the
-- FunPtr will be freed by someone else (the function registering the
-- callback for ScopeTypeCall, or a destroy notifier for
-- ScopeTypeNotified).
genCallbackWrapper :: Callable -> String -> [Arg] -> [Arg] -> [Arg] ->
                      Bool -> ExcCodeGen ()
genCallbackWrapper cb name' dataptrs hInArgs hOutArgs isSignal = do
  let cName arg = if arg `elem` dataptrs
                  then "_"
                  else (escapeReserved . argName) arg
      cArgNames = map cName (args cb)

  group $ do
    line $ lcFirst name' ++ "Wrapper ::"
    indent $ do
      unless isSignal $
           line $ "Maybe (Ptr (FunPtr (" ++ name' ++ "C))) ->"
      line $ name' ++ " ->"
      when isSignal $ line "Ptr () ->"
      forM_ (args cb) $ \arg -> do
        ht <- foreignType $ argType arg
        let ht' = if direction arg /= DirectionIn
                  then ptr ht
                  else ht
        line $ show ht' ++ " ->"
      when isSignal $ line "Ptr () ->"
      ret <- io <$> case returnType cb of
                      TBasicType TVoid -> return $ typeOf ()
                      t -> foreignType t
      line $ show ret

    let allArgs = if isSignal
                  then unwords $ ["_cb", "_"] ++ cArgNames ++ ["_"]
                  else unwords $ ["funptrptr", "_cb"] ++ cArgNames
    line $ lcFirst name' ++ "Wrapper " ++ allArgs ++ " = do"
    indent $ do
      hInNames <- forM hInArgs (prepareArgForCall cb)

      let maybeReturn = case returnType cb of
                          TBasicType TVoid -> []
                          _                -> ["result"]
          argName' = escapeReserved . argName
          returnVars = maybeReturn ++ map (("out"++) . argName') hOutArgs
          returnBind = case returnVars of
                         []  -> ""
                         [r] -> r ++ " <- "
                         _   -> parenthesize (intercalate ", " returnVars) ++ " <- "
      line $ returnBind ++ "_cb " ++ concatMap (" " ++) hInNames

      forM_ hOutArgs saveOutArg

      unless isSignal $ line "maybeReleaseFunPtr funptrptr"

      when (returnType cb /= TBasicType TVoid) $
           if returnMayBeNull cb
           then do
             line "maybeM nullPtr result $ \\result' -> do"
             indent $ unwrapped "result'"
           else unwrapped "result"
           where
             unwrapped rname = do
               result' <- convert rname $ hToF (returnType cb) (returnTransfer cb)
               line $ "return " ++ result'

genCallback :: Name -> Callback -> CodeGen ()
genCallback n (Callback cb) = do
  name' <- upperName n
  line $ "-- callback " ++ name'

  let -- user_data pointers, which we generically omit
      dataptrs = map (args cb !!) . filter (/= -1) . map argClosure $ args cb
      hidden = dataptrs ++ arrayLengths cb

      inArgs = filter ((/= DirectionOut) . direction) $ args cb
      hInArgs = filter (not . (`elem` hidden)) inArgs
      outArgs = filter ((/= DirectionIn) . direction) $ args cb
      hOutArgs = filter (not . (`elem` hidden)) outArgs

  if skipReturn cb
  then group $ do
    line $ "-- XXX Skipping callback " ++ name'
    line $ "-- Callbacks skipping return unsupported :\n"
             ++ ppShow n ++ "\n" ++ ppShow cb
  else do
    let closure = lcFirst name' ++ "Closure"

    handleCGExc (\e -> line ("-- XXX Could not generate callback wrapper for "
                             ++ name' ++
                             "\n-- Error was : " ++ describeCGError e))
       (genClosure name' closure False >>
        genCCallbackPrototype cb name' False >>
        genCallbackWrapperFactory name' >>
        genHaskellCallbackPrototype cb name' hInArgs hOutArgs >>
        genCallbackWrapper cb name' dataptrs hInArgs hOutArgs False)

-- | Return the name for the signal in Haskell CamelCase conventions.
signalHaskellName :: String -> String
signalHaskellName sn = let (w:ws) = split '-' sn
                       in w ++ concatMap ucFirst ws

genSignal :: Signal -> Name -> ExcCodeGen ()
genSignal (Signal { sigName = sn, sigCallable = cb }) on = do
  on' <- upperName on

  line $ "-- signal " ++ on' ++ "::" ++ T.unpack sn

  let inArgs = filter ((/= DirectionOut) . direction) $ args cb
      hInArgs = filter (not . (`elem` arrayLengths cb)) inArgs
      outArgs = filter ((/= DirectionIn) . direction) $ args cb
      hOutArgs = filter (not . (`elem` arrayLengths cb)) outArgs
      sn' = signalHaskellName (T.unpack sn)
      signalConnectorName = on' ++ ucFirst sn'
      cbType = signalConnectorName ++ "Callback"

  genHaskellCallbackPrototype cb cbType hInArgs hOutArgs

  genCCallbackPrototype cb cbType True

  genCallbackWrapperFactory cbType

  let closure = lcFirst signalConnectorName ++ "Closure"
  genClosure cbType closure True

  genCallbackWrapper cb cbType [] hInArgs hOutArgs True

  -- Wrapper for connecting functions to the signal
  -- We can connect to a signal either before the default handler runs
  -- ("on...") or after the default handler runs (after...). We
  -- provide convenient wrappers for both cases.
  group $ do
    let signatureConstraints = "GObject a =>"
        signatureArgs = "a -> " ++ cbType ++ " -> IO SignalHandlerId"
        signature = " :: " ++ signatureConstraints ++ " " ++ signatureArgs
        onName = "on" ++ signalConnectorName
        afterName = "after" ++ signalConnectorName
    line $ onName ++ signature
    line $ onName ++ " obj cb = connect"
             ++ signalConnectorName ++ " obj cb SignalConnectBefore"
    line $ afterName ++ signature
    line $ afterName ++ " obj cb = connect"
             ++ signalConnectorName ++ " obj cb SignalConnectAfter"

  group $ do
    let fullName = "connect" ++ signalConnectorName
        signatureConstraints = "GObject a =>"
        signatureArgs = "a -> " ++ cbType
                        ++ " -> SignalConnectMode -> IO SignalHandlerId"
    line $ fullName ++ " :: " ++ signatureConstraints
    line $ replicate (4 + length fullName) ' ' ++ signatureArgs
    line $ fullName ++ " obj cb after = do"
    indent $ do
        line $ "cb' <- mk" ++ cbType ++ " (" ++ lcFirst cbType ++ "Wrapper cb)"
        line $ "connectSignalFunPtr obj \"" ++ T.unpack sn ++ "\" cb' after"
