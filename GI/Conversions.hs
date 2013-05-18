{-# LANGUAGE PatternGuards #-}

module GI.Conversions
    ( convert
    , marshallFType
    , convertFMarshall
    , convertHMarshall
    , Expr(..)
    , hToF
    , fToH
    ) where

import Data.Typeable (TypeRep, tyConName, typeRepTyCon)

import GI.API
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Type

toPtr con = "(\\(" ++ con ++ " x) -> x)"

prime = (++ "'")
parenthesize s = "(" ++ s ++ ")"

interfaceName :: Type -> CodeGen (Maybe Name)
interfaceName t = do
    a <- findAPI t
    case a of
         Just (APIInterface _) -> do
              case t of
                   TInterface ns n -> return $ Just $ Name ns n
                   _ -> error "Interface without TInterface!"
         _ -> return Nothing

data Expr = Var String
          | App String Expr
          | M Expr
          | EMap Expr
          | EStr String String
          deriving Show

data Do = Bind String Expr | Let String Expr

-- This is somewhat ugly.
code :: Expr -> (String, [String])
code e = case doExpr e of
             ds@((Bind s _) : _) -> (s, map doStr $ reverse ds)
             ds@((Let s _) : _) -> (s, map doStr $ reverse ds)
             [] -> (name e, [])
  where
    name :: Expr -> String
    name (Var s) = s
    name (App _ e) = prime $ name e
    name (M e) = name e
    name (EStr n _) = n
    name (EMap e) = name e

    doExpr :: Expr -> [Do]
    doExpr (Var _) = []
    doExpr (App f e) = Let (prime $ name e) (App f (Var $ name e)) : (doExpr e)
    doExpr (M (App f e)) = Bind (prime $ name e) (App f (Var $ name e)) : (doExpr e)
    doExpr (M (EStr n e)) = [Bind n (EStr n e)]
    doExpr (EMap (App f e)) = Let (prime $ name e)
                              (App ("map " ++ f) (Var $ name e)) : (doExpr e)
    doExpr (EMap (M (App f e))) = Bind (prime $ name e)
                              (App ("mapM " ++ f) (Var $ name e)) : (doExpr e)
    doExpr e = error $ "doExpr: " ++ show e

    doStr (Let s e) = "let " ++ s ++ " = " ++ exprStr e
    doStr (Bind s e) = s ++ " <- " ++ exprStr e

    exprStr (Var s) = s
    exprStr (App f e) = f ++ " " ++ exprStr e
    exprStr (EStr _ s) = s
    exprStr e = error $ "exprStr: " ++ show e

-- Marshalling for conversions such as [a] -> GList (a')
primitivePacker :: String -> Type -> CodeGen (Expr -> Expr)
primitivePacker packer a = do
  let conv'      :: (Expr -> Expr) -> Expr -> Expr
      conv' ac e = M $ App packer $ EMap (ac e)
  aConverter <- hToF a
  return $ conv' aConverter

hToF' :: Type -> Maybe API -> TypeRep -> TypeRep -> CodeGen (Expr -> Expr)
hToF' t a hType fType
    | ( hType == fType ) = return id
    | Just (APIEnum _) <- a = return $ App "(fromIntegral . fromEnum)"
    | Just (APIFlags _) <- a = return $ App "fromIntegral"
    | Just (APIObject _) <- a = return $
                            App "(castPtr . unsafeManagedPtrGetPtr)"
    | Just (APIInterface _) <- a = return $
                               App "(castPtr . unsafeManagedPtrGetPtr)"
    | ptr hType == fType = do
        let con = tyConName $ typeRepTyCon hType
        return $ App $ toPtr con
    | (TGList a) <- t = primitivePacker "packGList" a
    | (TGSList a) <- t = primitivePacker "packGSList" a
    | otherwise = return $ case (show hType, show fType) of
               ("[Char]", "CString") -> M . App "newCString"
               ("Word", "Type")      -> App "fromIntegral"
               ("Bool", "CInt")      -> App "(fromIntegral . fromEnum)"
               _                     -> error $ "don't know how to convert "
                                        ++ show hType ++ " into "
                                        ++ show fType ++ "."

hToF :: Type -> CodeGen (Expr -> Expr)
hToF t = do
  hType <- nsHaskellType t
  fType <- nsForeignType t
  a <- findAPI t
  hToF' t a hType fType

-- Marshalling for conversions such as GList (a') -> a
primitiveUnpacker :: String -> Type -> CodeGen (Expr -> Expr)
primitiveUnpacker unpacker a = do
  let conv'      :: (Expr -> Expr) -> Expr -> Expr
      conv' ac e = EMap $ ac $ M $ App unpacker e
  aConverter <- fToH a
  return $ conv' aConverter

fToH' :: Type -> Maybe API -> TypeRep -> TypeRep -> CodeGen (Expr -> Expr)
fToH' t a hType fType
    | ( hType == fType ) = return id
    | Just (APIEnum _) <- a = return $ App "(toEnum . fromIntegral)"
    | Just (APIFlags _) <- a = return $ App "fromIntegral"
    | ptr hType == fType = do
          let constructor = tyConName $ typeRepTyCon hType
          isGO <- isGObject t
          prefixGO <- qualify "GObject"
          if isGO
          then return $ M . App (parenthesize $ prefixGO ++ "makeNewObject "
                                               ++ constructor)
          else do
            --- These are for routines that return
            --- abstract interfaces. We create a managed
            --- pointer without actual refcounting.
            ifaceName <- interfaceName t
            case ifaceName of
              Just _ -> return $ M . App (parenthesize $
                       "\\x -> " ++ constructor ++ " <$> newForeignPtr_ x")
              _ -> return $ App $ constructor
    | (TGList a) <- t = primitiveUnpacker "unpackGList" a
    | (TGSList a) <- t = primitiveUnpacker "unpackGSList" a
    | otherwise = return $ case (show fType, show hType) of
               ("CString", "[Char]") -> M . App "peekCString"
               ("Type", "Word")      -> App "fromIntegral"
               ("CInt", "Bool")      -> App "(/= 0)"
               _                     -> error $ "don't know how to convert "
                                        ++ show fType ++ " into "
                                        ++ show hType ++ "."

fToH :: Type -> CodeGen (Expr -> Expr)
fToH t = do
    hType <- nsHaskellType t
    fType <- nsForeignType t
    a <- findAPI t
    fToH' t a hType fType

convert :: Expr -> CodeGen (Expr -> Expr) -> CodeGen String
convert e f = do
  converter <- f
  let (name, lines) = code $ converter e
  mapM_ line lines
  return name

-- The marshaller C code has some built in support for basic types, so
-- we only generate conversions for things that the marshaller cannot
-- do itself. (This list should be kept in sync with hsgclosure.c)

-- Marshaller to haskell types.
-- There is no support in the marshaller for converting Haskell
-- strings into C strings directly.
marshallFType :: Type -> CodeGen TypeRep
marshallFType t@(TBasicType TUTF8) = nsForeignType t
marshallFType t@(TBasicType TFileName) = nsForeignType t
marshallFType t@(TBasicType _) = return $ haskellType t
marshallFType a = nsForeignType a

convertFMarshall name t@(TBasicType TUTF8) = convert (Var name) (fToH t)
convertFMarshall name t@(TBasicType TFileName) = convert (Var name) (fToH t)
convertFMarshall name (TBasicType _) = return name
convertFMarshall name t = convert (Var name) (fToH t)

convertHMarshall name t@(TBasicType TUTF8) = convert (Var name) (hToF t)
convertHMarshall name t@(TBasicType TFileName) = convert (Var name) (hToF t)
convertHMarshall name (TBasicType _) = return name
convertHMarshall name t = convert (Var name) (hToF t)

