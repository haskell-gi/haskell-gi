{-# LANGUAGE LambdaCase #-}
module GI.Callable
    ( genCallable

    , hOutType
    , arrayLengths
    , arrayLengthsMap

    , wrapMaybe
    , inArgInterfaces
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, forM_, when)
import Data.Bool (bool)
import Data.List (nub, (\\))
import Data.Maybe (isJust)
import Data.Typeable (TypeRep, typeOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)

import GI.API
import GI.Code
import GI.Conversions
import GI.SymbolNaming
import GI.Transfer
import GI.Type
import GI.Util

import Text.Show.Pretty (ppShow)

hOutType :: Callable -> [Arg] -> Bool -> ExcCodeGen TypeRep
hOutType callable outArgs ignoreReturn = do
  hReturnType <- case returnType callable of
                   TBasicType TVoid -> return $ typeOf ()
                   _                -> if ignoreReturn
                                       then return $ typeOf ()
                                       else haskellType $ returnType callable
  hOutArgTypes <- forM outArgs $ \outarg ->
                  wrapMaybe outarg >>= bool
                                (haskellType (argType outarg))
                                (maybeT <$> haskellType (argType outarg))
  let maybeHReturnType = if returnMayBeNull callable && not ignoreReturn
                         then maybeT hReturnType
                         else hReturnType
  return $ case (outArgs, tshow maybeHReturnType) of
             ([], _)   -> maybeHReturnType
             (_, "()") -> "(,)" `con` hOutArgTypes
             _         -> "(,)" `con` (maybeHReturnType : hOutArgTypes)

mkForeignImport :: Text -> Callable -> Bool -> CodeGen ()
mkForeignImport symbol callable throwsGError = do
    line first
    indent $ do
        mapM_ (\a -> line =<< fArgStr a) (args callable)
        when throwsGError $
               line $ padTo 40 "Ptr (Ptr GError) -> " <> "-- error"
        line =<< last
    where
    first = "foreign import ccall \"" <> symbol <> "\" " <>
                symbol <> " :: "
    fArgStr arg = do
        ft <- foreignType $ argType arg
        weAlloc <- isJust <$> requiresAlloc (argType arg)
        let ft' = if direction arg == DirectionIn || weAlloc then
                      ft
                  else
                      ptr ft
        let start = tshow ft' <> " -> "
        return $ padTo 40 start <> "-- " <> (argName arg)
                   <> " : " <> tshow (argType arg)
    last = tshow <$> io <$> case returnType callable of
                             TBasicType TVoid -> return $ typeOf ()
                             _  -> foreignType (returnType callable)

-- Given an argument to a function, return whether it should be
-- wrapped in a maybe type (useful for nullable types). We do some
-- sanity checking to make sure that the argument is actually nullable
-- (a relatively common annotation mistake is to mix up (optional)
-- with (nullable)).
wrapMaybe :: Arg -> ExcCodeGen Bool
wrapMaybe arg =
    if mayBeNull arg
    then case argType arg of
           -- NULL GLists and GSLists are semantically the same as an
           -- empty list, so they don't need a Maybe wrapper on their
           -- type.
           TGList _ -> return False
           TGSList _ -> return False
           _ -> do
             nullable <- isNullable (argType arg)
             if nullable
             then return True
             else badIntroError $ "argument \"" <> (argName arg)
                      <> "\" is not of nullable type (" <> tshow (argType arg)
                      <> "), but it is marked as such."
    else return False

-- Given the list of arguments returns the list of constraints and the
-- list of types in the signature.
inArgInterfaces :: [Arg] -> ExcCodeGen ([Text], [Text])
inArgInterfaces inArgs = consAndTypes (['a'..'z'] \\ ['m']) inArgs
  where
    consAndTypes :: [Char] -> [Arg] -> ExcCodeGen ([Text], [Text])
    consAndTypes _ [] = return ([], [])
    consAndTypes letters (arg:args) = do
      (ls, t, cons) <- argumentType letters $ argType arg
      t' <- wrapMaybe arg >>= bool (return t)
                                   (return $ "Maybe (" <> t <> ")")
      (restCons, restTypes) <- consAndTypes ls args
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
                 TCArray False (-1) length _ ->
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
  let lvar = escapeReserved $ argName length
      avar = escapeReserved $ argName array
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
  name <- lowerName n
  let funcName = namespace n <> "." <> name
      lvar = escapeReserved $ argName length
      avar = escapeReserved $ argName array
      expectedLength = avar <> "_expected_length_"
      pvar = escapeReserved $ argName previous
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

-- Whether to skip the return value in the generated bindings. The
-- C convention is that functions throwing an error and returning
-- a gboolean set the boolean to TRUE iff there is no error, so
-- the information is always implicit in whether we emit an
-- exception or not, so the return value can be omitted from the
-- generated bindings without loss of information (and omitting it
-- gives rise to a nicer API). See
-- https://bugzilla.gnome.org/show_bug.cgi?id=649657
skipRetVal :: Callable -> Bool -> Bool
skipRetVal callable throwsGError =
    (skipReturn callable) ||
         (throwsGError && returnType callable == TBasicType TBoolean)

freeInArgs' :: (Arg -> Text -> Text -> ExcCodeGen [Text]) ->
               Callable -> Map.Map Text Text -> ExcCodeGen [Text]
freeInArgs' freeFn callable nameMap = concat <$> actions
    where
      actions :: ExcCodeGen [[Text]]
      actions = forM (args callable) $ \arg ->
        case Map.lookup (escapeReserved $ argName arg) nameMap of
          Just name -> freeFn arg name $
                       -- Pass in the length argument in case it's needed.
                       case argType arg of
                         TCArray False (-1) (-1) _ -> undefined
                         TCArray False (-1) length _ ->
                             escapeReserved $ argName $ (args callable)!!length
                         _ -> undefined
          Nothing -> badIntroError $ "freeInArgs: do not understand " <> tshow arg

-- Return the list of actions freeing the memory associated with the
-- callable variables. This is run if the call to the C function
-- succeeds, if there is an error freeInArgsOnError below is called
-- instead.
freeInArgs = freeInArgs' freeInArg

-- Return the list of actions freeing the memory associated with the
-- callable variables. This is run in case there is an error during
-- the call.
freeInArgsOnError = freeInArgs' freeInArgOnError

-- Marshall the haskell arguments into their corresponding C
-- equivalents. omitted gives a list of DirectionIn arguments that
-- should be ignored, as they will be dealt with separately.
prepareArgForCall :: [Arg] -> Arg -> ExcCodeGen Text
prepareArgForCall omitted arg = do
  isCallback <- findAPI (argType arg) >>=
                \case Just (APICallback _) -> return True
                      _ -> return False
  when (isCallback && direction arg /= DirectionIn) $
       notImplementedError "Only callbacks with DirectionIn are supported"

  case direction arg of
    DirectionIn -> if arg `elem` omitted
                   then return . escapeReserved . argName $ arg
                   else if isCallback
                        then prepareInCallback arg
                        else prepareInArg arg
    DirectionInout -> prepareInoutArg arg
    DirectionOut -> prepareOutArg arg

prepareInArg :: Arg -> ExcCodeGen Text
prepareInArg arg = do
  let name = escapeReserved $ argName arg
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

-- Callbacks are a fairly special case, we treat them separately.
prepareInCallback :: Arg -> ExcCodeGen Text
prepareInCallback arg = do
  let name = escapeReserved $ argName arg
      ptrName = "ptr" <> name
      scope = argScope arg

  (maker, wrapper) <- case argType arg of
                        (TInterface ns n) ->
                            do
                              prefix <- qualify ns
                              return $ (prefix <> "mk" <> n,
                                        prefix <> lcFirst n <> "Wrapper")
                        _ -> terror $ "prepareInCallback : Not an interface! " <> T.pack (ppShow arg)

  when (scope == ScopeTypeAsync) $ do
   ft <- tshow <$> foreignType (argType arg)
   line $ ptrName <> " <- callocMem :: IO (Ptr (" <> ft <> "))"

  wrapMaybe arg >>= bool
            (do
              let name' = prime name
                  p = if (scope == ScopeTypeAsync)
                      then parenthesize $ "Just " <> ptrName
                      else "Nothing"
              line $ name' <> " <- " <> maker <> " "
                       <> parenthesize (wrapper <> " " <> p <> " " <> name)
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
                         let p = if (scope == ScopeTypeAsync)
                                 then parenthesize $ "Just " <> ptrName
                                 else "Nothing"
                         line $ jName' <> " <- " <> maker <> " "
                                  <> parenthesize (wrapper <> " "
                                                   <> p <> " " <> jName)
                         when (scope == ScopeTypeAsync) $
                              line $ "poke " <> ptrName <> " " <> jName'
                         line $ "return " <> jName'
              return maybeName)

prepareInoutArg :: Arg -> ExcCodeGen Text
prepareInoutArg arg = do
  name' <- prepareInArg arg
  ft <- foreignType $ argType arg
  allocInfo <- requiresAlloc (argType arg)
  case allocInfo of
    Just (isBoxed, n) -> do
         let allocator = if isBoxed
                         then "callocBoxedBytes"
                         else "callocBytes"
         wrapMaybe arg >>= bool
            (do
              name'' <- genConversion (prime name') $
                        literal $ M $ allocator <> " " <> tshow n <>
                                    " :: " <> tshow (io ft)
              line $ "memcpy " <> name'' <> " " <> name' <> " " <> tshow n
              return name'')
             -- The semantics of this case are somewhat undefined.
            (notImplementedError "Nullable inout structs not supported")
    Nothing -> do
      name'' <- genConversion (prime name') $
                literal $ M $ "allocMem :: " <> tshow (io $ ptr ft)
      line $ "poke " <> name'' <> " " <> name'
      return name''

prepareOutArg :: Arg -> CodeGen Text
prepareOutArg arg = do
  let name = escapeReserved $ argName arg
  ft <- foreignType $ argType arg
  allocInfo <- requiresAlloc (argType arg)
  case allocInfo of
    Just (isBoxed, n) -> do
        let allocator = if isBoxed
                        then "callocBoxedBytes"
                        else "callocBytes"
        genConversion name $ literal $ M $ allocator <> " " <> tshow n <>
                                      " :: " <> tshow (io ft)
    Nothing ->
        genConversion name $
                  literal $ M $ "allocMem :: " <> tshow (io $ ptr ft)

-- Convert a non-zero terminated out array, stored in a variable
-- named "aname", into the corresponding Haskell object.
convertOutCArray :: Callable -> Type -> Text -> Map.Map Text Text ->
                    Transfer -> ExcCodeGen Text
convertOutCArray callable t@(TCArray False fixed length _) aname
                 nameMap transfer = do
  if fixed > -1
  then do
    unpacked <- convert aname $ unpackCArray (tshow fixed) t transfer
    -- Free the memory associated with the array
    freeContainerType transfer t aname undefined
    return unpacked
  else do
    when (length == -1) $
         badIntroError $ "Unknown length for \"" <> aname <> "\""
    let lname = escapeReserved $ argName $ (args callable)!!length
    lname' <- case Map.lookup lname nameMap of
                Just n -> return n
                Nothing ->
                    badIntroError $ "Couldn't find out array length " <>
                                            lname
    let lname'' = prime lname'
    unpacked <- convert aname $ unpackCArray lname'' t transfer
    -- Free the memory associated with the array
    freeContainerType transfer t aname lname''
    return unpacked

-- Remove the warning, this should never be reached.
convertOutCArray _ t _ _ _ =
    terror $ "convertOutCArray : unexpected " <> tshow t

-- Read the array lengths for out arguments.
readOutArrayLengths :: Callable -> Map.Map Text Text -> ExcCodeGen ()
readOutArrayLengths callable nameMap = do
  let lNames = nub $ map (escapeReserved . argName) $
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
  let name = escapeReserved $ argName arg
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
                                <> T.pack (ppShow callable)
                                <> "\n" <> T.pack (ppShow m)
                                <> "\n" <> tshow closure
        Just cb -> do
          let closureName = escapeReserved $ argName $ (args callable)!!closure
              n = escapeReserved $ argName cb
          n' <- case Map.lookup n nameMap of
                  Just n -> return n
                  Nothing -> badIntroError $ "Cannot find closure name!! "
                                           <> T.pack (ppShow callable) <> "\n"
                                           <> T.pack (ppShow nameMap)
          case argScope cb of
            ScopeTypeInvalid -> badIntroError $ "Invalid scope! "
                                              <> T.pack (ppShow callable)
            ScopeTypeNotified -> do
                line $ "let " <> closureName <> " = castFunPtrToPtr " <> n'
                case argDestroy cb of
                  (-1) -> badIntroError $
                          "ScopeTypeNotified without destructor! "
                           <> T.pack (ppShow callable)
                  k -> let destroyName =
                            escapeReserved . argName $ (args callable)!!k in
                       line $ "let " <> destroyName <> " = safeFreeFunPtrPtr"
            ScopeTypeAsync ->
                line $ "let " <> closureName <> " = nullPtr"
            ScopeTypeCall -> line $ "let " <> closureName <> " = nullPtr"

freeCallCallbacks :: Callable -> Map.Map Text Text -> ExcCodeGen ()
freeCallCallbacks callable nameMap =
    forM_ (args callable) $ \arg -> do
       let name = escapeReserved $ argName arg
       name' <- case Map.lookup name nameMap of
                  Just n -> return n
                  Nothing -> badIntroError $ "Could not find " <> name
                                <> " in " <> T.pack (ppShow callable) <> "\n"
                                <> T.pack (ppShow nameMap)
       when (argScope arg == ScopeTypeCall) $
            line $ "safeFreeFunPtr $ castFunPtrToPtr " <> name'

hSignature :: [Arg] -> TypeRep -> ExcCodeGen ()
hSignature hInArgs retType = do
  (argConstraints, types) <- inArgInterfaces hInArgs
  indent $ do
      line $ "(" <> T.intercalate ", " ("MonadIO m" : argConstraints) <> ") =>"
      forM_ (zip types hInArgs) $ \(t, a) ->
           line $ withComment (t <> " ->") $ (argName a)
      line . tshow $ "m" `con` [retType]

genCallable :: Name -> Text -> Callable -> Bool -> ExcCodeGen ()
genCallable n symbol callable throwsGError = do
    group $ do
        line $ "-- Args : " <> (tshow $ args callable)
        line $ "-- Lengths : " <> (tshow $ arrayLengths callable)
        line $ "-- hInArgs : " <> tshow hInArgs
        line $ "-- returnType : " <> (tshow $ returnType callable)
        line $ "-- throws : " <> (tshow throwsGError)
        line $ "-- Skip return : " <> (tshow $ skipReturn callable)
        when (skipReturn callable && returnType callable /= TBasicType TBoolean) $
             do line "-- XXX return value ignored, but it is not a boolean."
                line "--     This may be a memory leak?"
    mkForeignImport symbol callable throwsGError
    blank
    wrapper

    where
    inArgs = filter ((/= DirectionOut) . direction) $ args callable
    -- We do not expose user_data arguments, destroynotify arguments,
    -- and C array length arguments to Haskell code.
    closures = map (args callable!!) . filter (/= -1) . map argClosure $ inArgs
    destroyers = map (args callable!!) . filter (/= -1) . map argDestroy $ inArgs
    omitted = arrayLengths callable <> closures <> destroyers
    hInArgs = filter (`notElem` omitted) inArgs
    outArgs = filter ((/= DirectionIn) . direction) $ args callable
    hOutArgs = filter (`notElem` (arrayLengths callable)) outArgs

    ignoreReturn = skipRetVal callable throwsGError

    wrapper = group $ do
        let argName' = escapeReserved . argName
        name <- lowerName n
        export name
        line $ deprecatedPragma name $ callableDeprecated callable
        line $ name <> " ::"
        hSignature hInArgs =<< hOutType callable hOutArgs ignoreReturn
        line $ name <> " " <> T.intercalate " " (map argName' hInArgs) <> " = liftIO $ do"
        indent $ do
            readInArrayLengths n callable hInArgs
            inArgNames <- forM (args callable) $ \arg ->
                          prepareArgForCall omitted arg
            -- Map from argument names to names passed to the C function
            let nameMap = Map.fromList $ flip zip inArgNames
                                               $ map argName' $ args callable
            prepareClosures callable nameMap
            if throwsGError
            then do
                line "onException (do"
                indent $ do
                    invokeCFunction inArgNames
                    readOutArrayLengths callable nameMap
                    result <- convertResult nameMap
                    pps <- convertOut nameMap
                    freeCallCallbacks callable nameMap
                    forM_ (args callable) touchInArg
                    mapM_ line =<< freeInArgs callable nameMap
                    returnResult result pps
                line " ) (do"
                indent $ do
                    freeCallCallbacks callable nameMap
                    actions <- freeInArgsOnError callable nameMap
                    case actions of
                        [] -> line $ "return ()"
                        _ -> mapM_ line actions
                line " )"
            else do
                invokeCFunction inArgNames
                readOutArrayLengths callable nameMap
                result <- convertResult nameMap
                pps <- convertOut nameMap
                freeCallCallbacks callable nameMap
                forM_ (args callable) touchInArg
                mapM_ line =<< freeInArgs callable nameMap
                returnResult result pps

    invokeCFunction argNames = do
        let returnBind = case returnType callable of
                           TBasicType TVoid -> ""
                           _                -> if ignoreReturn
                                               then "_ <- "
                                               else "result <- "
            maybeCatchGErrors = if throwsGError
                                then "propagateGError $ "
                                else ""
        line $ returnBind <> maybeCatchGErrors
                 <> symbol <> (T.concat . map (" " <>)) argNames

    convertResult :: Map.Map Text Text -> ExcCodeGen Text
    convertResult nameMap =
        if ignoreReturn || returnType callable == TBasicType TVoid
        then return (error "convertResult: unreachable code reached, bug!")
        else do
            if returnMayBeNull callable
            then do
                line $ "maybeResult <- convertIfNonNull result $ \\result' -> do"
                indent $ do
                    converted <- unwrappedConvertResult "result'"
                    line $ "return " <> converted
                return "maybeResult"
            else do
              nullable <- isNullable (returnType callable)
              when nullable $
                 line $ "checkUnexpectedReturnNULL \"" <> symbol
                          <> "\" result"
              unwrappedConvertResult "result"

        where
        unwrappedConvertResult rname = case returnType callable of
            -- Arrays without length information are just passed
            -- along.
            TCArray False (-1) (-1) _ -> return rname
            -- Not zero-terminated C arrays require knowledge of the
            -- length, so we deal with them directly.
            t@(TCArray False _ _ _) ->
                convertOutCArray callable t rname nameMap
                                 (returnTransfer callable)
            t -> do
                result <- convert rname $ fToH (returnType callable)
                                               (returnTransfer callable)
                freeContainerType (returnTransfer callable) t rname undefined
                return result

    convertOut :: Map.Map Text Text -> ExcCodeGen [Text]
    convertOut nameMap = do
        -- Convert out parameters and result
        forM hOutArgs $ \arg -> do
            let name = escapeReserved $ argName arg
            inName <- case Map.lookup name nameMap of
                Just name' -> return name'
                Nothing -> badIntroError $ "Parameter " <> name <> " not found!"
            case argType arg of
                -- Passed along as a raw pointer
                TCArray False (-1) (-1) _ -> genConversion inName $ apply $ M "peek"
                t@(TCArray False _ _ _) -> do
                    aname' <- genConversion inName $ apply $ M "peek"
                    let wrapArray a = convertOutCArray callable t a
                                          nameMap (transfer arg)
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
                    weAlloc <- isJust <$> requiresAlloc t
                    peeked <- if weAlloc
                             then return inName
                             else genConversion inName $ apply $ M "peek"
                    -- If we alloc we always take control of the resulting
                    -- memory, otherwise we may leak.
                    let transfer' = if weAlloc
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

    returnResult :: Text -> [Text] -> CodeGen ()
    returnResult result pps =
        if ignoreReturn || returnType callable == TBasicType TVoid
        then case pps of
            []      -> line "return ()"
            (pp:[]) -> line $ "return " <> pp
            _       -> line $ "return (" <> T.intercalate ", " pps <> ")"
        else case pps of
            [] -> line $ "return " <> result
            _  -> line $ "return (" <> T.intercalate ", " (result : pps) <> ")"
