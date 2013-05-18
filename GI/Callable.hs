module GI.Callable
    ( genCallable
    , hOutType
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
                   _                -> nsHaskellType $ returnType callable
  hOutArgTypes <- mapM (nsHaskellType . argType) outArgs
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
        ft <- nsForeignType $ argType arg
        let ft' = case direction arg of
              DirectionInout -> ptr ft
              DirectionOut -> ptr ft
              DirectionIn -> ft
        let start = show ft' ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
    last = show <$> io <$> case returnType callable of
                             TBasicType TVoid -> return $ typeOf ()
                             _  -> nsForeignType (returnType callable)

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
  s <- show <$> nsHaskellType t
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

-- XXX We should free the memory allocated for the [a] -> Glist (a')
-- etc. conversions.
genCallable :: Name -> String -> Callable -> CodeGen ()
genCallable n symbol callable = do
    mkForeignImport symbol callable
    wrapper

    where
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    wrapper = group $ do
        let argName' = escapeReserved . argName
        name <- lowerName n
        signature
        line $
            name ++ " " ++
            intercalate " " (map argName' inArgs) ++
            " = do"
        indent $ do
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
        (constraints, types) <- inArgInterfaces inArgs
        indent $ do
            when (not $ null constraints) $ do
                line $ "(" ++ intercalate ", " constraints ++ ") =>"
            forM_ (zip types inArgs) $ \(t, a) ->
                             line $ withComment (t ++ " ->") $ argName a
            result >>= line
    result = show <$> io <$> hOutType callable outArgs
    convertIn = forM (args callable) $ \arg -> do
        ft <- nsForeignType $ argType arg
        let name = escapeReserved $ argName arg
        if direction arg == DirectionIn
            then convert (Var name) (hToF $ argType arg)
            else convert (M $ EStr name $ "malloc :: " ++ show (io $ ptr ft)) (return id)
    -- XXX: Should create ForeignPtrs for pointer results.
    -- XXX: Check argument transfer.
    convertOut = do
        -- Convert return value and out paramters.
        result <- convert (Var "result") (fToH $ returnType callable)
        pps <- forM outArgs $ \arg -> do
               convert (M $ App "peek" (Var $ escapeReserved $ argName arg))
                 (fToH $ argType arg)
        case (returnType callable, pps) of
            (TBasicType TVoid, []) -> line $ "return ()"
            (TBasicType TVoid, pp:[]) -> line $ "return " ++ pp
            (TBasicType TVoid, _) -> line $
                                     "return (" ++ intercalate ", " pps ++ ")"
            (_ , []) -> line $ "return " ++ result
            (_ , _) -> line $
                "return (" ++ intercalate ", " (result : pps) ++ ")"
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
