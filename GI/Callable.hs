module GI.Callable
    ( genCallable

    , hOutType
    , arrayLengths
    , arrayLengthsMap
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, when)
import Data.List (intercalate)
import Data.Typeable (typeOf)

import GI.API
import GI.Code
import GI.Conversions
import GI.GObject
import GI.SymbolNaming
import GI.Type
import GI.Internal.ArgInfo

-- Returns whether the given type corresponds to a ManagedPtr
-- instance, currently we manage APIObjects and Interfaces.
isManaged   :: Type -> CodeGen Bool
isManaged t = do
  a <- findAPI t
  case a of
    Just (APIObject _)    -> return True
    Just (APIInterface _) -> return True
    _                     -> return False

padTo n s = s ++ replicate (n - length s) ' '

hOutType callable outArgs = do
  hReturnType <- case returnType callable of
                   TBasicType TVoid -> return $ typeOf ()
                   _                -> haskellType $ returnType callable
  hOutArgTypes <- mapM (haskellType . argType) outArgs
  let justType = case (outArgs, show hReturnType) of
                   ([], _)   -> hReturnType
                   (_, "()") -> "(,)" `con` hOutArgTypes
                   _         -> "(,)" `con` (hReturnType : hOutArgTypes)
      maybeType = "Maybe" `con` [justType]
  return $ if returnMayBeNull callable then maybeType else justType

mkForeignImport :: String -> Callable -> CodeGen ()
mkForeignImport symbol callable = foreignImport $ do
    line first
    indent $ do
        mapM_ (\a -> line =<< fArgStr a) (args callable)
        line =<< last
    where
    first = "foreign import ccall \"" ++ symbol ++ "\" " ++
                symbol ++ " :: "
    fArgStr arg = do
        ft <- foreignType $ argType arg
        let ft' = case direction arg of
              DirectionInout -> ptr ft
              DirectionOut -> ptr ft
              DirectionIn -> ft
        let start = show ft' ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
                   ++ " : " ++ show (argType arg)
    last = show <$> io <$> case returnType callable of
                             TBasicType TVoid -> return $ typeOf ()
                             _  -> foreignType (returnType callable)

-- Given a type find the typeclasses the type belongs to, and return
-- the representation of the type in the function signature and the
-- list of typeclass constraints for the type.
argumentType :: [Char] -> Type -> CodeGen ([Char], String, [String])
argumentType [] _               = error "out of letters"
argumentType letters (TGList a) = do
  (ls, name, constraints) <- argumentType letters a
  return (ls, "[" ++ name ++ "]", constraints)
argumentType letters (TGSList a) = do
  (ls, name, constraints) <- argumentType letters a
  return (ls, "[" ++ name ++ "]", constraints)
argumentType letters@(l:ls) t   = do
  api <- findAPI t
  s <- show <$> haskellType t
  case api of
    Just (APIInterface _) -> return (ls, [l],
                                     [interfaceClassName s ++ " " ++ [l],
                                      "ManagedPtr " ++ [l]])
    -- Instead of restricting to the actual class,
    -- we allow for any object descending from it.
    Just (APIObject _) -> do
        isGO <- isGObject t
        if isGO
        then return (ls, [l], [klass s ++ " " ++ [l],
                               "ManagedPtr " ++ [l]])
        else return (letters, s, [])
    _ -> return (letters, s, [])

-- Given the list of arguments returns the list of constraints and the
-- list of types in the signature.
inArgInterfaces :: [Arg] -> CodeGen ([String], [String])
inArgInterfaces inArgs = rec "abcdefghijklmnopqrtstuvwxyz" inArgs
  where
    rec :: [Char] -> [Arg] -> CodeGen ([String], [String])
    rec _ [] = return ([], [])
    rec letters (arg:args) = do
      (ls, t, cons) <- argumentType letters $ argType arg
      (restCons, restTypes) <- rec ls args
      return (cons ++ restCons, t : restTypes)

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
                            if fixedSize > -1
                            then go as acc
                            else go as $ (a, (args callable)!!length) : acc
                        _ -> go as acc

-- Return the list of arguments of the callable that contain length
-- arguments, including a possible length for the result of calling
-- the function.
arrayLengths :: Callable -> [Arg]
arrayLengths callable = map snd (arrayLengthsMap callable) ++
               -- Often one of the arguments is just the length of
               -- the result.
               case returnType callable of
                 TCArray False (-1) length _ -> [(args callable)!!length]
                 _ -> []

-- XXX We should free the memory allocated for the [a] -> Glist (a')
-- etc. conversions.
genCallable :: Name -> String -> Callable -> CodeGen ()
genCallable n symbol callable = do
    group $ do
      line $ "-- Args : " ++ (show $ args callable)
      line $ "-- Lengths : " ++ (show $ arrayLengths callable)
      line $ "-- hInArgs : " ++ show hInArgs
      line $ "-- returnType : " ++ (show $ returnType callable)
    mkForeignImport symbol callable
    wrapper

    where
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    -- We do not need to expose the length of array arguments to Haskell code.
    hInArgs = filter (not . (`elem` (arrayLengths callable))) inArgs
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    hOutArgs = filter (not . (`elem` (arrayLengths callable))) outArgs
    wrapper = group $ do
        let argName' = escapeReserved . argName
        name <- lowerName n
        signature
        line $
            name ++ " " ++
            intercalate " " (map argName' hInArgs) ++
            " = do"
        indent $ do
          readInArrayLengths
          argNames <- convertIn
          let returnBind = case returnType callable of
                             TBasicType TVoid -> ""
                             _                -> "result <- "
          line $ returnBind ++ symbol ++ concatMap (" " ++) argNames
          touchInArgs
          convertOut
    signature = do
        name <- lowerName n
        line $ name ++ " ::"
        (constraints, types) <- inArgInterfaces hInArgs
        indent $ do
            when (not $ null constraints) $ do
                line $ "(" ++ intercalate ", " constraints ++ ") =>"
            forM_ (zip types hInArgs) $ \(t, a) ->
                 line $ withComment (t ++ " ->") $ argName a
            result >>= line
    result = show <$> io <$> hOutType callable hOutArgs
    convertIn = forM (args callable) $ \arg -> do
        ft <- foreignType $ argType arg
        let name = escapeReserved $ argName arg
        if direction arg == DirectionIn
            then convert name (hToF $ argType arg)
            else do
              name' <- genConversion name $
                            literal $ M $ "malloc :: " ++ show (io $ ptr ft)
              when (direction arg == DirectionInout) $
                   line $ "-- XXX: InOut argument: " ++ name'
              return name'
    -- Read the length of in array arguments from the corresponding
    -- Haskell objects.
    readInArrayLengths = forM_ (arrayLengthsMap callable) $ \(array, length) ->
       when (direction array == DirectionIn) $
            do let lvar = escapeReserved $ argName length
                   avar = escapeReserved $ argName array
               computeArrayLength lvar avar (argType array)
    -- XXX: Should create ForeignPtrs for pointer results.
    -- XXX: Check argument transfer.
    convertOut = do
        -- Convert return value and out paramters.
        result <- case returnType callable of
                    -- Non-zero terminated C arrays require knowledge of
                    -- the length, so we deal with them directly.
                    t@(TCArray False _ _ _) -> convertOutCArray t "result"
                    _ -> convert "result" (fToH $ returnType callable)
        pps <- forM hOutArgs $ \arg ->
               do let name = escapeReserved $ argName arg
                  case argType arg of
                    t@(TCArray False _ _ _) ->
                        do let aname = escapeReserved $ argName arg
                           aname' <- genConversion aname $ apply $ M "peek"
                           convertOutCArray t aname'
                    _ -> do
                      constructor <- fToH $ argType arg
                      genConversion name $ do apply $ M "peek"
                                              constructor
        case (returnType callable, pps) of
            (TBasicType TVoid, []) -> line $ "return ()"
            (TBasicType TVoid, pp:[]) -> line $ "return " ++ pp
            (TBasicType TVoid, _) -> line $
                                     "return (" ++ intercalate ", " pps ++ ")"
            (_ , []) -> line $ "return " ++ result
            (_ , _) -> line $
                "return (" ++ intercalate ", " (result : pps) ++ ")"
    -- Convert a non-zero terminated out array, stored in a variable
    -- named "aname".
    convertOutCArray t@(TCArray False fixed length _) aname = do
      if fixed > -1
      then convert aname $ unpackCArray (show fixed) t
      else do
        let lname = escapeReserved $ argName $ (args callable)!!length
        lname' <- genConversion lname $ apply $ M "peek"
        convert aname $ unpackCArray lname' t
    -- Remove the warning, this should never be reached.
    convertOutCArray t _ = error $ "convertOutCArray : unexpected " ++ show t
    -- Touch in arguments so we are sure that they exist when the C
    -- function was called.
    touchInArgs = forM_ (args callable) $ \arg -> do
        when (direction arg == DirectionIn) $ do
           let name = escapeReserved $ argName arg
           case argType arg of
             (TGList a) -> do
               managed <- isManaged a
               when managed $ line $ "mapM_ touchManagedPtr " ++ name
             (TGSList a) -> do
               managed <- isManaged a
               when managed $ line $ "mapM_ touchManagedPtr " ++ name
             _ -> do
               managed <- isManaged (argType arg)
               when managed $ line $ "touchManagedPtr " ++ name
    withComment a b = padTo 40 a ++ "-- " ++ b
