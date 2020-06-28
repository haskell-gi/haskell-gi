module Data.GI.CodeGen.Signal
    ( genSignal
    , genCallback
    , signalHaskellName
    ) where

import Control.Monad (forM, forM_, when, unless)

import Data.Maybe (catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Bool (bool)
import qualified Data.Text as T
import Data.Text (Text)

import Text.Show.Pretty (ppShow)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Callable (hOutType, wrapMaybe,
                                 fixupCallerAllocates,
                                 genDynamicCallableWrapper,
                                 callableHInArgs, callableHOutArgs)
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.Haddock (deprecatedPragma,
                                RelativeDocPosition(..), writeHaddock,
                                writeDocumentation,
                                writeArgDocumentation, writeReturnDocumentation)
import Data.GI.CodeGen.SymbolNaming
import Data.GI.CodeGen.Transfer (freeContainerType)
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util (parenthesize, withComment, tshow, terror,
                             lcFirst, ucFirst, prime)
import Data.GI.GIR.Documentation (Documentation)

-- | The prototype of the callback on the Haskell side (what users of
-- the binding will see)
genHaskellCallbackPrototype :: Text -> Callable -> Text -> ExposeClosures ->
                               Documentation -> ExcCodeGen ()
genHaskellCallbackPrototype subsec cb htype expose doc = group $ do
    let name' = case expose of
                  WithClosures -> callbackHTypeWithClosures htype
                  WithoutClosures -> htype
        (hInArgs, _) = callableHInArgs cb expose
        inArgsWithArrows = zip ("" : repeat "-> ") hInArgs
        hOutArgs = callableHOutArgs cb

    export (NamedSubsection SignalSection subsec) name'
    writeDocumentation DocBeforeSymbol doc
    line $ "type " <> name' <> " ="
    indent $ do
      forM_ inArgsWithArrows $ \(arrow, arg) -> do
        ht <- isoHaskellType (argType arg)
        isMaybe <- wrapMaybe arg
        let formattedType = if isMaybe
                            then typeShow (maybeT ht)
                            else typeShow ht
        line $ arrow <> formattedType
        writeArgDocumentation arg
      ret <- hOutType cb hOutArgs
      let returnArrow = if null hInArgs
                        then ""
                        else "-> "
      line $ returnArrow <> typeShow (io ret)
      writeReturnDocumentation cb False

    blank

    -- For optional parameters, in case we want to pass Nothing.
    export (NamedSubsection SignalSection subsec) ("no" <> name')
    writeHaddock DocBeforeSymbol (noCallbackDoc name')
    line $ "no" <> name' <> " :: Maybe " <> name'
    line $ "no" <> name' <> " = Nothing"

  where noCallbackDoc :: Text -> Text
        noCallbackDoc typeName =
          "A convenience synonym for @`Nothing` :: `Maybe` `" <> typeName <>
          "`@."

-- | Generate the type synonym for the prototype of the callback on
-- the C side. Returns the name given to the type synonym.
genCCallbackPrototype :: Text -> Callable -> Text -> Bool -> CodeGen Text
genCCallbackPrototype subsec cb name' isSignal = group $ do
    let ctypeName = callbackCType name'

    export (NamedSubsection SignalSection subsec) ctypeName
    writeHaddock DocBeforeSymbol ccallbackDoc

    line $ "type " <> ctypeName <> " ="
    indent $ do
      when isSignal $ line $ withComment "Ptr () ->" "object"
      forM_ (args cb) $ \arg -> do
        ht <- foreignType $ argType arg
        let ht' = if direction arg /= DirectionIn
                  then ptr ht
                  else ht
        line $ typeShow ht' <> " ->"
      when (callableThrows cb) $
        line "Ptr (Ptr GError) ->"
      when isSignal $ line $ withComment "Ptr () ->" "user_data"
      ret <- io <$> case returnType cb of
                      Nothing -> return $ con0 "()"
                      Just t -> foreignType t
      line $ typeShow ret
    return ctypeName

  where
    ccallbackDoc :: Text
    ccallbackDoc = "Type for the callback on the (unwrapped) C side."

-- | Generator for wrappers callable from C
genCallbackWrapperFactory :: Text -> Text -> CodeGen ()
genCallbackWrapperFactory subsec name' = group $ do
    let factoryName = callbackWrapperAllocator name'
    writeHaddock DocBeforeSymbol factoryDoc
    line "foreign import ccall \"wrapper\""
    indent $ line $ factoryName <> " :: " <> callbackCType name'
               <> " -> IO (FunPtr " <> callbackCType name' <> ")"
    export (NamedSubsection SignalSection subsec) factoryName

  where factoryDoc :: Text
        factoryDoc = "Generate a function pointer callable from C code, from a `"
                     <> callbackCType name' <> "`."

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
  export (NamedSubsection SignalSection subsec) closure
  writeHaddock DocBeforeSymbol closureDoc
  group $ do
      line $ closure <> " :: MonadIO m => " <> callback <> " -> m (GClosure "
                     <> callbackCType callback <> ")"
      line $ closure <> " cb = liftIO $ do"
      indent $ do
            wrapped <- genWrappedCallback cb "cb" callback isSignal
            line $ callbackWrapperAllocator callback <> " " <> wrapped
                     <> " >>= B.GClosure.newGClosure"
  where
    closureDoc :: Text
    closureDoc = "Wrap the callback into a `GClosure`."

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
      let c = convert name $ transientToH (argType arg) (transfer arg)
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

  export (NamedSubsection SignalSection subsec) dropper
  writeHaddock DocBeforeSymbol dropperDoc

  line $ dropper <> " :: " <> name' <> " -> " <> callbackHTypeWithClosures name'
  line $ dropper <> " _f " <> T.unwords argNames <> " = _f "
           <> T.unwords (catMaybes (map passOrIgnore inWithClosures))

  where dropperDoc :: Text
        dropperDoc = "A simple wrapper that ignores the closure arguments."

-- | The wrapper itself, marshalling to and from Haskell. The `Callable`
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
      wrapperDoc = "Wrap a `" <> name' <> "` into a `" <>
                   callbackCType name' <> "`."

  export (NamedSubsection SignalSection subsec) wrapperName
  writeHaddock DocBeforeSymbol wrapperDoc

  group $ do
    line $ wrapperName <> " ::"
    indent $ do
      if isSignal
      then do
        line $ name' <> " ->"
      else do
           line $ "Maybe (Ptr (FunPtr " <> callbackCType name' <> ")) ->"
           let hType = if callableHasClosures cb
                       then callbackHTypeWithClosures name'
                       else name'
           line $ hType <> " ->"

      line $ callbackCType name'

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
          mkTuple = parenthesize . T.intercalate ", "
          returnBind = case returnVars of
                         []  -> ""
                         [r] -> r <> " <- "
                         _   -> mkTuple returnVars <> " <- "
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
genCallback n callback@(Callback {cbCallable = cb, cbDocumentation = cbDoc }) = do
  let Name _ name' = normalizedAPIName (APICallback callback) n
      cb' = fixupCallerAllocates cb

  line $ "-- callback " <> name'
  line $ "{- " <> T.pack (ppShow cb') <> "\n-}"

  if skipReturn cb
  then group $ do
    line $ "-- XXX Skipping callback " <> name'
    line $ "{- Callbacks skipping return unsupported :\n"
             <> T.pack (ppShow n) <> "\n" <> T.pack (ppShow cb') <> "-}"
  else do
    handleCGExc (\e -> do
                   line $ "-- XXX Could not generate callback wrapper for "
                          <> name'
                   printCGError e) $ do
      typeSynonym <- genCCallbackPrototype name' cb' name' False
      dynamic <- genDynamicCallableWrapper n typeSynonym cb
      export (NamedSubsection SignalSection name') dynamic
      genCallbackWrapperFactory name' name'
      deprecatedPragma name' (callableDeprecated cb')
      genHaskellCallbackPrototype name' cb' name' WithoutClosures cbDoc
      when (callableHasClosures cb') $ do
           genHaskellCallbackPrototype name' cb' name' WithClosures cbDoc
           genDropClosures name' cb' name'
      if callableThrows cb'
      then do
        {- [Note: Callables that throw]

          In the case that the Callable throws (GErrors) we cannot
          simply take a Haskell functions that throws and wrap it into
          a foreign function, since in the case that an exception is
          raised the return value of the function is undefined, but we
          need to provide some value to the FFI.

          Alternatively, we could ask the Haskell function to provide
          a return value and optionally a GError. If the GError is
          present we should then release the memory associated with
          the out/return values (the caller will not do it, since
          there was an error), and then return some bogus values. This
          is fairly complicated, and callbacks raising GErrors are
          fairly rare, so for the moment we do not generate wrappers
          for these cases.
        -}
        line $ "-- No Haskell->C wrapper generated since the function throws."
        blank
      else do
        genClosure name' cb' name' name' False
        genCallbackWrapper name' cb' name' False

-- | Generate the given signal instance for the given API object.
genSignalInfoInstance :: Name -> Signal -> CodeGen ()
genSignalInfoInstance owner signal = group $ do
  let name = upperName owner
  let sn = (ucFirst . signalHaskellName . sigName) signal
  si <- signalInfoName owner signal
  bline $ "data " <> si
  line $ "instance SignalInfo " <> si <> " where"
  indent $ do
      let signalConnectorName = name <> sn
          cbHaskellType = signalConnectorName <> "Callback"
      line $ "type HaskellCallbackType " <> si <> " = " <> cbHaskellType
      line $ "connectSignal obj cb connectMode detail = do"
      indent $ genSignalConnector signal cbHaskellType "connectMode" "detail"
  export (NamedSubsection SignalSection $ lcFirst sn) si

-- | Write some simple debug message when signal generation fails, and
-- generate a placeholder SignalInfo instance.
processSignalError :: Signal -> Name -> CGError -> CodeGen ()
processSignalError signal owner err = do
  let qualifiedSignalName = upperName owner <> "::" <> sigName signal
      sn = (ucFirst . signalHaskellName . sigName) signal
  line $ T.concat ["-- XXX Could not generate signal "
                  , qualifiedSignalName
                  , "\n", "-- Error was : "]
  printCGError err

  -- Generate a placeholder SignalInfo instance that raises a type
  -- error when one attempts to use it.
  cppIf CPPOverloading $ group $ do
    si <- signalInfoName owner signal
    bline $ "data " <> si
    line $ "instance SignalInfo " <> si <> " where"
    indent $ do
      line $ "type HaskellCallbackType " <> si <>
        " = B.Signals.SignalCodeGenError \"" <> qualifiedSignalName <> "\""
      line $ "connectSignal = undefined"
    export (NamedSubsection SignalSection $ lcFirst sn) si

-- | Generate a wrapper for a signal.
genSignal :: Signal -> Name -> CodeGen ()
genSignal s@(Signal { sigName = sn, sigCallable = cb }) on =
  handleCGExc (processSignalError s on) $ do
  let on' = upperName on

  line $ "-- signal " <> on' <> "::" <> sn

  let sn' = signalHaskellName sn
      signalConnectorName = on' <> ucFirst sn'
      cbType = signalConnectorName <> "Callback"
      docSection = NamedSubsection SignalSection $ lcFirst sn'

  deprecatedPragma cbType (callableDeprecated cb)

  genHaskellCallbackPrototype (lcFirst sn') cb cbType WithoutClosures (sigDoc s)

  _ <- genCCallbackPrototype (lcFirst sn') cb cbType True

  genCallbackWrapperFactory (lcFirst sn') cbType

  if callableThrows cb
    then do
      line $ "-- No Haskell->C wrapper generated since the function throws."
      blank
    else do
      genClosure (lcFirst sn') cb cbType signalConnectorName True
      genCallbackWrapper (lcFirst sn') cb cbType True

  -- Wrapper for connecting functions to the signal
  -- We can connect to a signal either before the default handler runs
  -- ("on...") or after the default handler runs (after...). We
  -- provide convenient wrappers for both cases.
  group $ do
    -- Notice that we do not include GObject here as a constraint,
    -- since if something provides signals it is necessarily a
    -- GObject.
    klass <- classConstraint on
    let signatureConstraints = "(" <> klass <> " a, MonadIO m) =>"
        signatureArgs = if sigDetailed s
          then "a -> P.Maybe T.Text -> " <> cbType <> " -> m SignalHandlerId"
          else "a -> " <> cbType <> " -> m SignalHandlerId"
        signature = " :: " <> signatureConstraints <> " " <> signatureArgs
        onName = "on" <> signalConnectorName
        afterName = "after" <> signalConnectorName

    group $ do
      writeHaddock DocBeforeSymbol onDoc
      line $ onName <> signature
      if sigDetailed s
        then do
        line $ onName <> " obj detail cb = liftIO $ do"
        indent $ genSignalConnector s cbType "SignalConnectBefore" "detail"
        else do
        line $ onName <> " obj cb = liftIO $ do"
        indent $ genSignalConnector s cbType "SignalConnectBefore" "Nothing"
      export docSection onName

    group $ do
      writeHaddock DocBeforeSymbol afterDoc
      line $ afterName <> signature
      if sigDetailed s
        then do
        line $ afterName <> " obj detail cb = liftIO $ do"
        indent $ genSignalConnector s cbType "SignalConnectAfter" "detail"
        else do
        line $ afterName <> " obj cb = liftIO $ do"
        indent $ genSignalConnector s cbType "SignalConnectAfter" "Nothing"
      export docSection afterName

  cppIf CPPOverloading (genSignalInfoInstance on s)

  where
    onDoc :: Text
    onDoc = let hsn = signalHaskellName sn
            in T.unlines [
      "Connect a signal handler for the [" <> hsn <> "](#signal:" <> hsn <>
        ") signal, to be run before the default handler."
      , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
      , ""
      , "@"
      , "'Data.GI.Base.Signals.on' " <> lowerName on <> " #"
        <> hsn <> " callback"
      , "@"
      , ""
      , detailedDoc ]

    afterDoc :: Text
    afterDoc = let hsn = signalHaskellName sn
               in T.unlines [
      "Connect a signal handler for the [" <> hsn <> "](#signal:" <> hsn <>
        ") signal, to be run after the default handler."
      , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
      , ""
      , "@"
      , "'Data.GI.Base.Signals.after' " <> lowerName on <> " #"
        <> hsn <> " callback"
      , "@"
      , ""
      , detailedDoc ]

    detailedDoc :: Text
    detailedDoc = if not (sigDetailed s)
                  then ""
                  else T.unlines [
      "This signal admits a optional parameter @detail@."
      , "If it's not @Nothing@, we will connect to “@" <> sn
        <> "::detail@” instead."
      ]


-- | Generate the code for connecting the given signal. This assumes
-- that it lives inside a @do@ block.
genSignalConnector :: Signal
                   -> Text -- ^ Callback type
                   -> Text -- ^ SignalConnectBefore or SignalConnectAfter
                   -> Text -- ^ Detail
                   -> CodeGen ()
genSignalConnector (Signal {sigName = sn, sigCallable = cb}) cbType when detail = do
  cb' <- genWrappedCallback cb "cb" cbType True
  let cb'' = prime cb'
  line $ cb'' <> " <- " <> callbackWrapperAllocator cbType <> " " <> cb'
  line $ "connectSignalFunPtr obj \"" <> sn <> "\" " <> cb'' <> " " <> when
          <> " " <> detail
