{-# LANGUAGE LambdaCase #-}
module Data.GI.CodeGen.Callable
    ( genCCallableWrapper
    , genDynamicCallableWrapper
    , ForeignSymbol(..)

    , hOutType
    , skipRetVal
    , arrayLengths
    , arrayLengthsMap
    , callableSignature
    , Signature(..)
    , fixupCallerAllocates

    , callableHInArgs
    , callableHOutArgs

    , wrapMaybe
    , inArgInterfaces
    ) where

import Control.Monad (forM, forM_, when, void)
import Data.Bool (bool)
import Data.List (nub)
import Data.Maybe (isJust)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.Haddock (deprecatedPragma, writeHaddock,
                                writeDocumentation, RelativeDocPosition(..),
                                writeArgDocumentation, writeReturnDocumentation)
import Data.GI.CodeGen.SymbolNaming
import Data.GI.CodeGen.Transfer
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

import Text.Show.Pretty (ppShow)

hOutType :: Callable -> [Arg] -> ExcCodeGen TypeRep
hOutType callable outArgs = do
  hReturnType <- case returnType callable of
                   Nothing -> return $ con0 "()"
                   Just r -> if skipRetVal callable
                             then return $ con0 "()"
                             else haskellType r
  hOutArgTypes <- forM outArgs $ \outarg ->
                  wrapMaybe outarg >>= bool
                                (haskellType (argType outarg))
                                (maybeT <$> haskellType (argType outarg))
  nullableReturnType <- maybe (return False) typeIsNullable (returnType callable)
  let maybeHReturnType = if returnMayBeNull callable
                            && not (skipRetVal callable)
                            && nullableReturnType
                         then maybeT hReturnType
                         else hReturnType
  return $ case (outArgs, typeShow maybeHReturnType) of
             ([], _)   -> maybeHReturnType
             (_, "()") -> "(,)" `con` hOutArgTypes
             _         -> "(,)" `con` (maybeHReturnType : hOutArgTypes)

-- | Generate a foreign import for the given C symbol. Return the name
-- of the corresponding Haskell identifier.
mkForeignImport :: Text -> Callable -> CodeGen Text
mkForeignImport cSymbol callable = do
    line first
    indent $ do
        mapM_ (\a -> line =<< fArgStr a) (args callable)
        when (callableThrows callable) $
               line $ padTo 40 "Ptr (Ptr GError) -> " <> "-- error"
        line =<< last
    return hSymbol
    where
    hSymbol = if T.any (== '_') cSymbol
              then lcFirst cSymbol
              else "_" <> cSymbol
    first = "foreign import ccall \"" <> cSymbol <> "\" " <> hSymbol <> " :: "
    fArgStr arg = do
        ft <- foreignType $ argType arg
        let ft' = if direction arg == DirectionIn || argCallerAllocates arg
                  then ft
                  else ptr ft
        let start = typeShow ft' <> " -> "
        return $ padTo 40 start <> "-- " <> (argCName arg)
                   <> " : " <> tshow (argType arg)
    last = typeShow <$> io <$> case returnType callable of
                                 Nothing -> return $ con0 "()"
                                 Just r  -> foreignType r

-- | Make a wrapper for foreign `FunPtr`s of the given type. Return
-- the name of the resulting dynamic Haskell wrapper.
mkDynamicImport :: Text -> CodeGen Text
mkDynamicImport typeSynonym = do
  line $ "foreign import ccall \"dynamic\" " <> dynamic <> " :: FunPtr "
           <> typeSynonym <> " -> " <> typeSynonym
  return dynamic
      where dynamic = "__dynamic_" <> typeSynonym

-- | Given an argument to a function, return whether it should be
-- wrapped in a maybe type (useful for nullable types). We do some
-- sanity checking to make sure that the argument is actually nullable
-- (a relatively common annotation mistake is to mix up (optional)
-- with (nullable)).
wrapMaybe :: Arg -> CodeGen Bool
wrapMaybe arg = if mayBeNull arg
                then typeIsNullable (argType arg)
                else return False

-- | Given the list of arguments returns the list of constraints and the
-- list of types in the signature.
inArgInterfaces :: [Arg] -> ExposeClosures -> ExcCodeGen ([Text], [Text])
inArgInterfaces args expose = do
  resetTypeVariableScope
  go args
  where go [] = return ([], [])
        go (arg:args) = do
          (t, cons) <- argumentType (argType arg) expose
          t' <- wrapMaybe arg >>= bool (return t)
            (return $ "Maybe (" <> t <> ")")
          (restCons, restTypes) <- go args
          return (cons <> restCons, t' : restTypes)

-- Given a callable, return a list of (array, length) pairs, where in
-- each pair "length" is the argument holding the length of the
-- (non-zero-terminated, non-fixed size) C array.
arrayLengthsMap :: Callable -> [(Arg, Arg)] -- List of (array, length)
arrayLengthsMap callable = go (args callable) []
    where
      go :: [Arg] -> [(Arg, Arg)] -> [(Arg, Arg)]
      go [] acc = acc
      go (a:as) acc = case argType a of
                        TCArray False fixedSize length _ ->
                            if fixedSize > -1 || length == -1
                            then go as acc
                            else go as $ (a, (args callable)!!length) : acc
                        _ -> go as acc

-- Return the list of arguments of the callable that contain length
-- arguments, including a possible length for the result of calling
-- the function.
arrayLengths :: Callable -> [Arg]
arrayLengths callable = map snd (arrayLengthsMap callable) <>
               -- Often one of the arguments is just the length of
               -- the result.
               case returnType callable of
                 Just (TCArray False (-1) length _) ->
                     if length > -1
                     then [(args callable)!!length]
                     else []
                 _ -> []

-- This goes through a list of [(a,b)], and tags every entry where the
-- "b" field has occurred before with the value of "a" for which it
-- occurred. (The first appearance is not tagged.)
classifyDuplicates :: Ord b => [(a, b)] -> [(a, b, Maybe a)]
classifyDuplicates args = doClassify Map.empty args
    where doClassify :: Ord b => Map.Map b a -> [(a, b)] -> [(a, b, Maybe a)]
          doClassify _ [] = []
          doClassify found ((value, key):args) =
              (value, key, Map.lookup key found) :
                doClassify (Map.insert key value found) args

-- Read the length of in array arguments from the corresponding
-- Haskell objects. A subtlety is that sometimes a single length
-- argument is expected from the C side to encode the length of
-- various lists. Ideally we would encode this in the types, but the
-- resulting API would be rather cumbersome. We insted perform runtime
-- checks to make sure that the given lists have the same length.
readInArrayLengths :: Name -> Callable -> [Arg] -> ExcCodeGen ()
readInArrayLengths name callable hInArgs = do
  let lengthMaps = classifyDuplicates $ arrayLengthsMap callable
  forM_ lengthMaps $ \(array, length, duplicate) ->
      when (array `elem` hInArgs) $
        case duplicate of
        Nothing -> readInArrayLength array length
        Just previous -> checkInArrayLength name array length previous

-- Read the length of an array into the corresponding variable.
readInArrayLength :: Arg -> Arg -> ExcCodeGen ()
readInArrayLength array length = do
  let lvar = escapedArgName length
      avar = escapedArgName array
  wrapMaybe array >>= bool
                (do
                  al <- computeArrayLength avar (argType array)
                  line $ "let " <> lvar <> " = " <> al)
                (do
                  line $ "let " <> lvar <> " = case " <> avar <> " of"
                  indent $ indent $ do
                    line $ "Nothing -> 0"
                    let jarray = "j" <> ucFirst avar
                    al <- computeArrayLength jarray (argType array)
                    line $ "Just " <> jarray <> " -> " <> al)

-- Check that the given array has a length equal to the given length
-- variable.
checkInArrayLength :: Name -> Arg -> Arg -> Arg -> ExcCodeGen ()
checkInArrayLength n array length previous = do
  let name = lowerName n
      funcName = namespace n <> "." <> name
      lvar = escapedArgName length
      avar = escapedArgName array
      expectedLength = avar <> "_expected_length_"
      pvar = escapedArgName previous
  wrapMaybe array >>= bool
            (do
              al <- computeArrayLength avar (argType array)
              line $ "let " <> expectedLength <> " = " <> al)
            (do
              line $ "let " <> expectedLength <> " = case " <> avar <> " of"
              indent $ indent $ do
                line $ "Nothing -> 0"
                let jarray = "j" <> ucFirst avar
                al <- computeArrayLength jarray (argType array)
                line $ "Just " <> jarray <> " -> " <> al)
  line $ "when (" <> expectedLength <> " /= " <> lvar <> ") $"
  indent $ line $ "error \"" <> funcName <> " : length of '" <> avar <>
             "' does not agree with that of '" <> pvar <> "'.\""

-- | Whether to skip the return value in the generated bindings. The
-- C convention is that functions throwing an error and returning
-- a gboolean set the boolean to TRUE iff there is no error, so
-- the information is always implicit in whether we emit an
-- exception or not, so the return value can be omitted from the
-- generated bindings without loss of information (and omitting it
-- gives rise to a nicer API). See
-- https://bugzilla.gnome.org/show_bug.cgi?id=649657
skipRetVal :: Callable -> Bool
skipRetVal callable = (skipReturn callable) ||
                      (callableThrows callable &&
                        returnType callable == Just (TBasicType TBoolean))

freeInArgs' :: (Arg -> Text -> Text -> ExcCodeGen [Text]) ->
               Callable -> Map.Map Text Text -> ExcCodeGen [Text]
freeInArgs' freeFn callable nameMap = concat <$> actions
    where
      actions :: ExcCodeGen [[Text]]
      actions = forM (args callable) $ \arg ->
        case Map.lookup (escapedArgName arg) nameMap of
          Just name -> freeFn arg name $
                       -- Pass in the length argument in case it's needed.
                       case argType arg of
                         TCArray False (-1) (-1) _ ->
                           parenthesize ("length " <> escapedArgName arg)
                         TCArray False (-1) length _ ->
                             escapedArgName $ (args callable)!!length
                         _ -> undefined
          Nothing -> badIntroError $ "freeInArgs: do not understand " <> tshow arg

-- | Return the list of actions freeing the memory associated with the
-- callable variables. This is run if the call to the C function
-- succeeds, if there is an error freeInArgsOnError below is called
-- instead.
freeInArgs :: Callable -> Map.Map Text Text -> ExcCodeGen [Text]
freeInArgs = freeInArgs' freeInArg

-- | Return the list of actions freeing the memory associated with the
-- callable variables. This is run in case there is an error during
-- the call.
freeInArgsOnError :: Callable -> Map.Map Text Text -> ExcCodeGen [Text]
freeInArgsOnError = freeInArgs' freeInArgOnError

-- Marshall the haskell arguments into their corresponding C
-- equivalents. omitted gives a list of DirectionIn arguments that
-- should be ignored, as they will be dealt with separately.
prepareArgForCall :: [Arg] -> Arg -> ExposeClosures -> ExcCodeGen Text
prepareArgForCall omitted arg expose = do
  callback <- findAPI (argType arg) >>=
                \case Just (APICallback c) -> return (Just c)
                      _ -> return Nothing

  when (isJust callback && direction arg /= DirectionIn) $
       notImplementedError "Only callbacks with DirectionIn are supported"

  case direction arg of
    DirectionIn -> if arg `elem` omitted
                   then return . escapedArgName $ arg
                   else case callback of
                        Just c -> if callableThrows (cbCallable c)
                                  -- See [Note: Callables that throw]
                                  then return (escapedArgName arg)
                                  else prepareInCallback arg c expose
                        Nothing -> prepareInArg arg
    DirectionInout -> prepareInoutArg arg
    DirectionOut -> prepareOutArg arg

prepareInArg :: Arg -> ExcCodeGen Text
prepareInArg arg = do
  let name = escapedArgName arg
  wrapMaybe arg >>= bool
            (convert name $ hToF (argType arg) (transfer arg))
            (do
              let maybeName = "maybe" <> ucFirst name
              line $ maybeName <> " <- case " <> name <> " of"
              indent $ do
                line $ "Nothing -> return nullPtr"
                let jName = "j" <> ucFirst name
                line $ "Just " <> jName <> " -> do"
                indent $ do
                         converted <- convert jName $ hToF (argType arg)
                                                           (transfer arg)
                         line $ "return " <> converted
                return maybeName)

-- | Callbacks are a fairly special case, we treat them separately.
prepareInCallback :: Arg -> Callback -> ExposeClosures -> CodeGen Text
prepareInCallback arg callback@(Callback {cbCallable = cb}) expose = do
  let name = escapedArgName arg
      ptrName = "ptr" <> name
      scope = argScope arg

  (maker, wrapper, drop) <-
      case argType arg of
        TInterface tn ->
            do
              let Name _ n = normalizedAPIName (APICallback callback) tn
              drop <- if callableHasClosures cb && expose == WithoutClosures
                      then Just <$> qualifiedSymbol (callbackDropClosures n) tn
                      else return Nothing
              wrapper <- qualifiedSymbol (callbackHaskellToForeign n) tn
              maker <- qualifiedSymbol (callbackWrapperAllocator n) tn
              return (maker, wrapper, drop)
        _ -> terror $ "prepareInCallback : Not an interface! " <> T.pack (ppShow arg)

  wrapMaybe arg >>= bool
            (do
              let name' = prime name
                  dropped =
                      case drop of
                        Just dropper -> parenthesize (dropper <> " " <> name)
                        Nothing -> name
              -- ScopeTypeAsync callbacks are somewhat tricky: they
              -- will be called only once, and the data associated to
              -- them will be invalid after the first call.
              --
              -- So we pass them a pointer to a dynamically allocated
              -- `Ptr FunPtr`, which contains a pointer to the
              -- `FunPtr` we dynamically allocate wrapping the Haskell
              -- function. On first invocation, the wrapper will then
              -- free this memory.
              p <- if (scope == ScopeTypeAsync)
                   then do ft <- typeShow <$> foreignType (argType arg)
                           line $ ptrName <> " <- callocMem :: IO (Ptr (" <> ft <> "))"
                           return $ parenthesize $ "Just " <> ptrName
                   else return "Nothing"

              line $ name' <> " <- " <> maker <> " "
                       <> parenthesize (wrapper <> " " <> p <> " " <> dropped)
              when (scope == ScopeTypeAsync) $
                   line $ "poke " <> ptrName <> " " <> name'
              return name')
            (do
              let maybeName = "maybe" <> ucFirst name
              line $ maybeName <> " <- case " <> name <> " of"
              indent $ do
                line $ "Nothing -> return (castPtrToFunPtr nullPtr)"
                let jName = "j" <> ucFirst name
                    jName' = prime jName
                line $ "Just " <> jName <> " -> do"
                indent $ do
                         let dropped = case drop of
                                   Just dropper ->
                                       parenthesize (dropper <> " " <> jName)
                                   Nothing -> jName
                         p <- if (scope == ScopeTypeAsync)
                           then do ft <- typeShow <$> foreignType (argType arg)
                                   line $ ptrName <> " <- callocMem :: IO (Ptr (" <> ft <> "))"
                                   return $ parenthesize $ "Just " <> ptrName
                           else return "Nothing"

                         line $ jName' <> " <- " <> maker <> " "
                                  <> parenthesize (wrapper <> " "
                                                   <> p <> " " <> dropped)
                         when (scope == ScopeTypeAsync) $
                              line $ "poke " <> ptrName <> " " <> jName'
                         line $ "return " <> jName'
              return maybeName)

prepareInoutArg :: Arg -> ExcCodeGen Text
prepareInoutArg arg = do
  name' <- prepareInArg arg
  ft <- foreignType $ argType arg
  allocInfo <- typeAllocInfo (argType arg)
  case allocInfo of
    Just (TypeAlloc allocator n) -> do
         wrapMaybe arg >>= bool
            (do
              name'' <- genConversion (prime name') $
                        literal $ M $ allocator <>
                                    " :: " <> typeShow (io ft)
              line $ "memcpy " <> name'' <> " " <> name' <> " " <> tshow n
              return name'')
             -- The semantics of this case are somewhat undefined.
            (notImplementedError "Nullable inout structs not supported")
    Nothing -> do
      if argCallerAllocates arg
      then return name'
      else do
        name'' <- genConversion (prime name') $
                  literal $ M $ "allocMem :: " <> typeShow (io $ ptr ft)
        line $ "poke " <> name'' <> " " <> name'
        return name''

prepareOutArg :: Arg -> ExcCodeGen Text
prepareOutArg arg = do
  let name = escapedArgName arg
  ft <- foreignType $ argType arg
  if argCallerAllocates arg
  then do
    allocInfo <- typeAllocInfo (argType arg)
    case allocInfo of
      Just (TypeAlloc allocator _) -> do
          genConversion name $ literal $ M $ allocator <>
                            " :: " <> typeShow (io ft)
      Nothing ->
          notImplementedError $ ("Don't know how to allocate \""
                                 <> argCName arg <> "\" of type "
                                 <> tshow (argType arg))
  else do
    -- Initialize pointers to NULL to avoid a crash in case the function
    -- does not initialize it.
    isPtr <- typeIsPtr (argType arg)
    let alloc = if isPtr
                then "callocMem"
                else "allocMem"
    genConversion name $ literal $ M $ alloc <> " :: " <> typeShow (io $ ptr ft)

-- Convert a non-zero terminated out array, stored in a variable
-- named "aname", into the corresponding Haskell object.
convertOutCArray :: Callable -> Type -> Text -> Map.Map Text Text ->
                    Transfer -> (Text -> Text) -> ExcCodeGen Text
convertOutCArray callable t@(TCArray False fixed length _) aname
                 nameMap transfer primeLength = do
  if fixed > -1
  then do
    unpacked <- convert aname $ unpackCArray (tshow fixed) t transfer
    -- Free the memory associated with the array.
    freeContainerType transfer t aname undefined
    return unpacked
  else do
    when (length == -1) $
         badIntroError $ "Unknown length for \"" <> aname <> "\""
    let lname = escapedArgName $ (args callable)!!length
    lname' <- case Map.lookup lname nameMap of
                Just n -> return n
                Nothing ->
                    badIntroError $ "Couldn't find out array length " <>
                                            lname
    let lname'' = primeLength lname'
    unpacked <- convert aname $ unpackCArray lname'' t transfer
    -- Free the memory associated with the array.
    freeContainerType transfer t aname lname''
    return unpacked

-- Remove the warning, this should never be reached.
convertOutCArray _ t _ _ _ _ =
    terror $ "convertOutCArray : unexpected " <> tshow t

-- Read the array lengths for out arguments.
readOutArrayLengths :: Callable -> Map.Map Text Text -> ExcCodeGen ()
readOutArrayLengths callable nameMap = do
  let lNames = nub $ map escapedArgName $
               filter ((/= DirectionIn) . direction) $
               arrayLengths callable
  forM_ lNames $ \lname -> do
    lname' <- case Map.lookup lname nameMap of
                   Just n -> return n
                   Nothing ->
                       badIntroError $ "Couldn't find out array length " <>
                                               lname
    genConversion lname' $ apply $ M "peek"

-- Touch DirectionIn arguments so we are sure that they exist when the
-- C function was called.
touchInArg :: Arg -> ExcCodeGen ()
touchInArg arg = when (direction arg /= DirectionOut) $ do
  let name = escapedArgName arg
  case elementType (argType arg) of
    Just a -> do
      managed <- isManaged a
      when managed $ wrapMaybe arg >>= bool
              (line $ "mapM_ touchManagedPtr " <> name)
              (line $ "whenJust " <> name <> " (mapM_ touchManagedPtr)")
    Nothing -> do
      managed <- isManaged (argType arg)
      when managed $ wrapMaybe arg >>= bool
           (line $ "touchManagedPtr " <> name)
           (line $ "whenJust " <> name <> " touchManagedPtr")

-- Find the association between closure arguments and their
-- corresponding callback.
closureToCallbackMap :: Callable -> ExcCodeGen (Map.Map Int Arg)
closureToCallbackMap callable =
    -- The introspection info does not specify the closure for destroy
    -- notify's associated with a callback, since it is implicitly the
    -- same one as the ScopeTypeNotify callback associated with the
    -- DestroyNotify.
    go (filter (not . (`elem` destroyers)) $ args callable) Map.empty

    where destroyers = map (args callable!!) . filter (/= -1) . map argDestroy
                       $ args callable

          go :: [Arg] -> Map.Map Int Arg -> ExcCodeGen (Map.Map Int Arg)
          go [] m = return m
          go (arg:as) m =
              if argScope arg == ScopeTypeInvalid
              then go as m
              else case argClosure arg of
                  (-1) -> go as m
                  c -> case Map.lookup c m of
                      Just _ -> notImplementedError $
                                "Closure for multiple callbacks unsupported"
                                <> T.pack (ppShow arg) <> "\n"
                                <> T.pack (ppShow callable)
                      Nothing -> go as $ Map.insert c arg m

-- user_data style arguments.
prepareClosures :: Callable -> Map.Map Text Text -> ExcCodeGen ()
prepareClosures callable nameMap = do
  m <- closureToCallbackMap callable
  let closures = filter (/= -1) . map argClosure $ args callable
  forM_ closures $ \closure ->
      case Map.lookup closure m of
        Nothing -> badIntroError $ "Closure not found! "
                                <> "\nClosure: " <> tshow closure
                                <> "\nc2cm: " <> T.pack (ppShow m)
                                <> "\ncallable: " <> T.pack (ppShow callable)
        Just cb -> do
          let closureName = escapedArgName $ (args callable)!!closure
              n = escapedArgName cb
          n' <- case Map.lookup n nameMap of
                  Just n -> return n
                  Nothing -> badIntroError $ "Cannot find closure name!! "
                                           <> T.pack (ppShow callable) <> "\n"
                                           <> T.pack (ppShow nameMap)
          -- Check that the given closure is an actual callback type.
          maybeAPI <- findAPI (argType cb)
          case maybeAPI of
            Just (APICallback _) -> do
              case argScope cb of
                ScopeTypeInvalid -> badIntroError $ "Invalid scope! "
                                              <> T.pack (ppShow callable)
                ScopeTypeNotified -> do
                  line $ "let " <> closureName <> " = castFunPtrToPtr " <> n'
                  case argDestroy cb of
                    (-1) -> badIntroError $
                            "ScopeTypeNotified without destructor! "
                            <> T.pack (ppShow callable)
                    k -> do
                      let destroyArg = (args callable)!!k
                          destroyName = escapedArgName destroyArg
                      destroyFun <- case argType destroyArg of
                        TInterface (Name "GLib" "DestroyNotify") ->
                          return "SP.safeFreeFunPtrPtr"
                        TInterface (Name "GObject" "ClosureNotify") ->
                          return "SP.safeFreeFunPtrPtr'"
                        _ -> notImplementedError $ "Unknown destroy type: " <> tshow (argType destroyArg)
                      line $ "let " <> destroyName <> " = " <> destroyFun
                ScopeTypeAsync -> do
                  line $ "let " <> closureName <> " = nullPtr"
                  case argDestroy cb of
                    -- Async callbacks don't really need destroy
                    -- notifications, as they can always be released
                    -- at the end of the callback.
                    (-1) -> return ()
                    n -> let destroyName = escapedArgName $ (args callable)!!n
                         in line $ "let " <> destroyName <> " = FP.nullFunPtr"
                ScopeTypeCall -> line $ "let " <> closureName <> " = nullPtr"
            _ -> badIntroError $ "Closure \"" <> n <> "\" is not a callback."

freeCallCallbacks :: Callable -> Map.Map Text Text -> ExcCodeGen ()
freeCallCallbacks callable nameMap =
    forM_ (args callable) $ \arg -> do
       let name = escapedArgName arg
       name' <- case Map.lookup name nameMap of
                  Just n -> return n
                  Nothing -> badIntroError $ "Could not find " <> name
                                <> " in " <> T.pack (ppShow callable) <> "\n"
                                <> T.pack (ppShow nameMap)
       when (argScope arg == ScopeTypeCall) $
            line $ "safeFreeFunPtr $ castFunPtrToPtr " <> name'

-- | Format the signature of the Haskell binding for the `Callable`.
formatHSignature :: Callable -> ForeignSymbol -> ExposeClosures -> ExcCodeGen ()
formatHSignature callable symbol expose = do
  sig <- callableSignature callable symbol expose
  indent $ do
      let constraints = "B.CallStack.HasCallStack" : signatureConstraints sig
      line $ "(" <> T.intercalate ", " constraints <> ") =>"
      forM_ (zip ("" : repeat "-> ") (signatureArgTypes sig)) $
        \(prefix, (maybeArg, t)) -> do
          line $ prefix <> t
          case maybeArg of
            Nothing -> return ()
            Just arg -> writeArgDocumentation arg
      let resultPrefix = if null (signatureArgTypes sig)
                         then ""
                         else "-> "
      line $ resultPrefix <> signatureReturnType sig
      writeReturnDocumentation (signatureCallable sig) (skipRetVal callable)

-- | Name for the first argument in dynamic wrappers (the `FunPtr`).
funPtr :: Text
funPtr = "__funPtr"

-- | Signature for a callable.
data Signature = Signature { signatureCallable    :: Callable
                           , signatureConstraints :: [Text]
                           , signatureArgTypes    :: [(Maybe Arg, Text)]
                           , signatureReturnType  :: Text
                           }

-- | The Haskell signature for the given callable. It returns a tuple
-- ([constraints], [(type, argname)]).
callableSignature :: Callable -> ForeignSymbol -> ExposeClosures
                  -> ExcCodeGen Signature
callableSignature callable symbol expose = do
  let (hInArgs, _) = callableHInArgs callable
                                    (case symbol of
                                       KnownForeignSymbol _ -> WithoutClosures
                                       DynamicForeignSymbol _ -> WithClosures)
  (argConstraints, types) <- inArgInterfaces hInArgs expose
  let constraints = ("MonadIO m" : argConstraints)
  outType <- hOutType callable (callableHOutArgs callable)
  return $ Signature {
      signatureCallable = callable,
      signatureConstraints = constraints,
      signatureReturnType = typeShow ("m" `con` [outType]),
      signatureArgTypes = case symbol of
          KnownForeignSymbol _ -> zip (map Just hInArgs) types
          DynamicForeignSymbol w -> zip (Nothing : map Just hInArgs)
                                    ("FunPtr " <> dynamicType w : types)
      }

-- | "In" arguments for the given callable on the Haskell side,
-- together with the omitted arguments.
callableHInArgs :: Callable -> ExposeClosures -> ([Arg], [Arg])
callableHInArgs callable expose =
    let inArgs = filter ((/= DirectionOut) . direction) $ args callable
                 -- We do not expose user_data arguments,
                 -- destroynotify arguments, and C array length
                 -- arguments to Haskell code.
        closures = map (args callable!!) . filter (/= -1) . map argClosure $ inArgs
        destroyers = map (args callable!!) . filter (/= -1) . map argDestroy $ inArgs
        omitted = case expose of
                    WithoutClosures -> arrayLengths callable <> closures <> destroyers
                    WithClosures -> arrayLengths callable
    in (filter (`notElem` omitted) inArgs, omitted)

-- | "Out" arguments for the given callable on the Haskell side.
callableHOutArgs :: Callable -> [Arg]
callableHOutArgs callable =
    let outArgs = filter ((/= DirectionIn) . direction) $ args callable
    in filter (`notElem` (arrayLengths callable)) outArgs

-- | Convert the result of the foreign call to Haskell.
convertResult :: Name -> Callable -> Map.Map Text Text ->
                 ExcCodeGen Text
convertResult n callable nameMap =
    if skipRetVal callable || returnType callable == Nothing
    then return (error "convertResult: unreachable code reached, bug!")
    else do
      nullableReturnType <- maybe (return False) typeIsNullable (returnType callable)
      if returnMayBeNull callable && nullableReturnType
      then do
        line $ "maybeResult <- convertIfNonNull result $ \\result' -> do"
        indent $ do
             converted <- unwrappedConvertResult "result'"
             line $ "return " <> converted
             return "maybeResult"
      else do
        when nullableReturnType $
             line $ "checkUnexpectedReturnNULL \"" <> lowerName n
                      <> "\" result"
        unwrappedConvertResult "result"

    where
      unwrappedConvertResult rname =
          case returnType callable of
            -- Arrays without length information cannot be converted
            -- into Haskell values.
            Just (t@(TCArray False (-1) (-1) _)) ->
                badIntroError ("`" <> tshow t <>
                "' is an array type, but contains no length information,\n"
                <> "so it cannot be unpacked.")
            -- Not zero-terminated C arrays require knowledge of the
            -- length, so we deal with them directly.
            Just (t@(TCArray False _ _ _)) ->
                convertOutCArray callable t rname nameMap
                                 (returnTransfer callable) prime
            Just t -> do
                result <- convert rname $ fToH t (returnTransfer callable)
                freeContainerType (returnTransfer callable) t rname undefined
                return result
            Nothing -> return (error "unwrappedConvertResult: bug!")

-- | Marshal a foreign out argument to Haskell, returning the name of
-- the variable containing the converted Haskell value.
convertOutArg :: Callable -> Map.Map Text Text -> Arg -> ExcCodeGen Text
convertOutArg callable nameMap arg = do
  let name = escapedArgName arg
  inName <- case Map.lookup name nameMap of
      Just name' -> return name'
      Nothing -> badIntroError $ "Parameter " <> name <> " not found!"
  case argType arg of
      t@(TCArray False (-1) (-1) _) ->
          if argCallerAllocates arg
          then return inName
          else  badIntroError ("`" <> tshow t <>
                "' is an array type, but contains no length information,\n"
                <> "so it cannot be unpacked.")
      t@(TCArray False _ _ _) -> do
          aname' <- if argCallerAllocates arg
                    then return inName
                    else genConversion inName $ apply $ M "peek"
          let arrayLength = if argCallerAllocates arg
                            then id
                            else prime
              wrapArray a = convertOutCArray callable t a
                                nameMap (transfer arg) arrayLength
          wrapMaybe arg >>= bool
                 (wrapArray aname')
                 (do line $ "maybe" <> ucFirst aname'
                         <> " <- convertIfNonNull " <> aname'
                         <> " $ \\" <> prime aname' <> " -> do"
                     indent $ do
                         wrapped <- wrapArray (prime aname')
                         line $ "return " <> wrapped
                     return $ "maybe" <> ucFirst aname')
      t -> do
          peeked <- if argCallerAllocates arg
                   then return inName
                   else genConversion inName $ apply $ M "peek"
          -- If we alloc we always take control of the resulting
          -- memory, otherwise we may leak.
          let transfer' = if argCallerAllocates arg
                         then TransferEverything
                         else transfer arg
          result <- do
              let wrap ptr = convert ptr $ fToH (argType arg) transfer'
              wrapMaybe arg >>= bool
                  (wrap peeked)
                  (do line $ "maybe" <> ucFirst peeked
                          <> " <- convertIfNonNull " <> peeked
                          <> " $ \\" <> prime peeked <> " -> do"
                      indent $ do
                          wrapped <- wrap (prime peeked)
                          line $ "return " <> wrapped
                      return $ "maybe" <> ucFirst peeked)
          -- Free the memory associated with the out argument
          freeContainerType transfer' t peeked undefined
          return result

-- | Convert the list of out arguments to Haskell, returning the
-- names of the corresponding variables containing the marshaled values.
convertOutArgs :: Callable -> Map.Map Text Text -> [Arg] -> ExcCodeGen [Text]
convertOutArgs callable nameMap hOutArgs =
    forM hOutArgs (convertOutArg callable nameMap)

-- | Invoke the given C function, taking care of errors.
invokeCFunction :: Callable -> ForeignSymbol -> [Text] -> CodeGen ()
invokeCFunction callable symbol argNames = do
  let returnBind = case returnType callable of
                     Nothing -> ""
                     _       -> if skipRetVal callable
                                then "_ <- "
                                else "result <- "
      maybeCatchGErrors = if callableThrows callable
                          then "propagateGError $ "
                          else ""
      call = case symbol of
               KnownForeignSymbol s -> s
               DynamicForeignSymbol w -> parenthesize (dynamicWrapper w
                                                      <> " " <> funPtr)
  line $ returnBind <> maybeCatchGErrors
           <> call <> (T.concat . map (" " <>)) argNames

-- | Return the result of the call, possibly including out arguments.
returnResult :: Callable -> Text -> [Text] -> CodeGen ()
returnResult callable result pps =
    if skipRetVal callable || returnType callable == Nothing
    then case pps of
        []      -> line "return ()"
        (pp:[]) -> line $ "return " <> pp
        _       -> line $ "return (" <> T.intercalate ", " pps <> ")"
    else case pps of
        [] -> line $ "return " <> result
        _  -> line $ "return (" <> T.intercalate ", " (result : pps) <> ")"

-- | Generate a Haskell wrapper for the given foreign function.
genHaskellWrapper :: Name -> ForeignSymbol -> Callable ->
                     ExposeClosures -> ExcCodeGen Text
genHaskellWrapper n symbol callable expose = group $ do
    let name = case symbol of
                 KnownForeignSymbol _ -> lowerName n
                 DynamicForeignSymbol _ -> callbackDynamicWrapper (upperName n)
        (hInArgs, omitted) = callableHInArgs callable expose
        hOutArgs = callableHOutArgs callable

    line $ name <> " ::"
    formatHSignature callable symbol expose
    let argNames = case symbol of
                     KnownForeignSymbol _ -> map escapedArgName hInArgs
                     DynamicForeignSymbol _ ->
                         funPtr : map escapedArgName hInArgs
    line $ name <> " " <> T.intercalate " " argNames <> " = liftIO $ do"
    indent (genWrapperBody n symbol callable hInArgs hOutArgs omitted expose)
    return name

-- | Generate the body of the Haskell wrapper for the given foreign symbol.
genWrapperBody :: Name -> ForeignSymbol -> Callable ->
                  [Arg] -> [Arg] -> [Arg] ->
                  ExposeClosures ->
                  ExcCodeGen ()
genWrapperBody n symbol callable hInArgs hOutArgs omitted expose = do
    readInArrayLengths n callable hInArgs
    inArgNames <- forM (args callable) $ \arg ->
                  prepareArgForCall omitted arg expose
    -- Map from argument names to names passed to the C function
    let nameMap = Map.fromList $ flip zip inArgNames
                               $ map escapedArgName $ args callable
    prepareClosures callable nameMap
    if callableThrows callable
    then do
        line "onException (do"
        indent $ do
            invokeCFunction callable symbol inArgNames
            readOutArrayLengths callable nameMap
            result <- convertResult n callable nameMap
            pps <- convertOutArgs callable nameMap hOutArgs
            freeCallCallbacks callable nameMap
            forM_ (args callable) touchInArg
            mapM_ line =<< freeInArgs callable nameMap
            returnResult callable result pps
        line " ) (do"
        indent $ do
            freeCallCallbacks callable nameMap
            actions <- freeInArgsOnError callable nameMap
            case actions of
                [] -> line $ "return ()"
                _ -> mapM_ line actions
        line " )"
    else do
        invokeCFunction callable symbol inArgNames
        readOutArrayLengths callable nameMap
        result <- convertResult n callable nameMap
        pps <- convertOutArgs callable nameMap hOutArgs
        freeCallCallbacks callable nameMap
        forM_ (args callable) touchInArg
        mapM_ line =<< freeInArgs callable nameMap
        returnResult callable result pps

-- | caller-allocates arguments are arguments that the caller
-- allocates, and the called function modifies. They are marked as
-- 'out' argumens in the introspection data, we sometimes treat them
-- as 'inout' arguments instead. The semantics are somewhat tricky:
-- for memory management purposes they should be treated as "in"
-- arguments, but from the point of view of the exposed API they
-- should be treated as "out" or "inout". Unfortunately we cannot
-- always just assume that they are purely "out", so in many cases the
-- generated API is somewhat suboptimal (since the initial values are
-- not important): for example for g_io_channel_read_chars the size of
-- the buffer to read is determined by the caller-allocates
-- argument. As a compromise, we assume that we can allocate anything
-- that is not a TCArray of length determined by an argument.
fixupCallerAllocates :: Callable -> Callable
fixupCallerAllocates c =
    c{args = map (fixupLength . fixupDir) (args c)}
    where fixupDir :: Arg -> Arg
          fixupDir a = case argType a of
                         TCArray _ _ l _ ->
                             if argCallerAllocates a && l > -1
                             then a { direction = DirectionInout
                                    , transfer = TransferEverything }
                             else a
                         _ -> a

          lengthsMap :: Map.Map Arg Arg
          lengthsMap = Map.fromList (map swap (arrayLengthsMap c))

          -- Length arguments of caller-allocates arguments should be
          -- treated as "in".
          fixupLength :: Arg -> Arg
          fixupLength a = case Map.lookup a lengthsMap of
                            Nothing -> a
                            Just array ->
                                if argCallerAllocates array
                                then a {direction = DirectionIn}
                                else a

-- | The foreign symbol to wrap. It is either a foreign symbol wrapped
-- in a foreign import, in which case we are given the name of the
-- Haskell wrapper, or alternatively the information about a "dynamic"
-- wrapper in scope.
data ForeignSymbol = KnownForeignSymbol Text -- ^ Haskell symbol in scope.
                   | DynamicForeignSymbol DynamicWrapper
                     -- ^ Info about the dynamic wrapper.

-- | Information about a dynamic wrapper.
data DynamicWrapper = DynamicWrapper {
      dynamicWrapper :: Text    -- ^ Haskell dynamic wrapper
    , dynamicType    :: Text    -- ^ Name of the type synonym for the
                                -- type of the function to be wrapped.
    }

-- | Some debug info for the callable.
genCallableDebugInfo :: Callable -> CodeGen ()
genCallableDebugInfo callable =
    group $ do
      commentShow "Args" (args callable)
      commentShow "Lengths" (arrayLengths callable)
      commentShow "returnType" (returnType callable)
      line $ "-- throws : " <> (tshow $ callableThrows callable)
      line $ "-- Skip return : " <> (tshow $ skipReturn callable)
      when (skipReturn callable && returnType callable /= Just (TBasicType TBoolean)) $
           do line "-- XXX return value ignored, but it is not a boolean."
              line "--     This may be a memory leak?"
  where commentShow :: Show a => Text -> a -> CodeGen ()
        commentShow prefix s =
          let padding = T.replicate (T.length prefix + 2) " "
              padded = case T.lines (T.pack $ ppShow s) of
                         [] -> []
                         (f:rest) -> "-- " <> prefix <> ": " <> f :
                                     map (("-- " <> padding) <>) rest
          in mapM_ line padded

-- | Generate a wrapper for a known C symbol.
genCCallableWrapper :: Name -> Text -> Callable -> ExcCodeGen ()
genCCallableWrapper n cSymbol callable
  | callableResolvable callable == Nothing =
      -- If we reach this point there is some internal error.
      terror ("Resolvability of “" <> cSymbol <> "” unkown.")
  | callableResolvable callable == Just False =
      badIntroError ("Could not resolve the symbol “" <> cSymbol
                     <> "” in the “" <> namespace n
                     <> "” namespace, ignoring.")
  | otherwise = do
      genCallableDebugInfo callable

      let callable' = fixupCallerAllocates callable

      hSymbol <- mkForeignImport cSymbol callable'

      blank

      deprecatedPragma (lowerName n) (callableDeprecated callable)
      writeDocumentation DocBeforeSymbol (callableDocumentation callable)
      void (genHaskellWrapper n (KnownForeignSymbol hSymbol) callable'
            WithoutClosures)

-- | For callbacks we do not need to keep track of which arguments are
-- closures.
forgetClosures :: Callable -> Callable
forgetClosures c = c {args = map forgetClosure (args c)}
    where forgetClosure :: Arg -> Arg
          forgetClosure arg = arg {argClosure = -1}

-- | Generate a wrapper for a dynamic C symbol (i.e. a Haskell
-- function that will invoke its first argument, which should be a
-- `FunPtr` of the appropriate type). The caller should have created a
-- type synonym with the right type for the foreign symbol.
genDynamicCallableWrapper :: Name -> Text -> Callable ->
                             ExcCodeGen Text
genDynamicCallableWrapper n typeSynonym callable = do
  genCallableDebugInfo callable

  let callable' = forgetClosures (fixupCallerAllocates callable)

  wrapper <- mkDynamicImport typeSynonym

  blank

  writeHaddock DocBeforeSymbol dynamicDoc

  let dyn = DynamicWrapper { dynamicWrapper = wrapper
                           , dynamicType    = typeSynonym }
  genHaskellWrapper n (DynamicForeignSymbol dyn) callable' WithClosures

  where
    dynamicDoc :: Text
    dynamicDoc = "Given a pointer to a foreign C function, wrap it into a function callable from Haskell."
