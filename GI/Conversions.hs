{-# LANGUAGE PatternGuards, DeriveFunctor, OverloadedStrings #-}

module GI.Conversions
    ( convert
    , genConversion

    , marshallFType
    , convertFMarshall
    , convertHMarshall
    , hToF
    , fToH

    , apply
    , mapC
    , literal
    , Constructor(..)
    ) where

import Control.Monad.Free (Free(..), liftF)
import Data.Typeable (TypeRep, tyConName, typeRepTyCon)
import GHC.Exts (IsString(..))

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

-- String identifying a constructor in the generated code, which is
-- either (by default) a pure function P, or a function returning
-- values on a monad M. 'Id' denotes the identity function.
data Constructor = P String | M String | Id
                   deriving (Eq,Show)
instance IsString Constructor where
    fromString cs = P cs

data FExpr next = Apply Constructor next
                | MapC Constructor next
                | Literal Constructor next
                  deriving (Show, Functor)

type Converter = Free FExpr ()

apply f = liftF $ Apply f ()
mapC f = liftF $ MapC f ()
literal f = liftF $ Literal f ()

genConversion :: String -> Converter -> CodeGen String
genConversion l (Pure ()) = return l
genConversion l (Free k) = do
  let l' = prime l
  case k of
    Apply (P f) next ->
        do line $ "let " ++ l' ++ " = " ++ f ++ " " ++ l
           genConversion l' next
    Apply (M f) next ->
        do line $ l' ++ " <- " ++ f ++ " " ++ l
           genConversion l' next
    Apply Id next -> genConversion l next

    MapC (P f) next ->
        do line $ "let " ++ l' ++ " = map " ++ f ++ " " ++ l
           genConversion l' next
    MapC (M f) next ->
        do line $ l' ++ " <- mapM " ++ f ++ " " ++ l
           genConversion l' next
    MapC Id next -> genConversion l next

    Literal (P f) next ->
        do line $ "let " ++ l ++ " = " ++ f
           genConversion l next
    Literal (M f) next ->
        do line $ l ++ " <- " ++ f
           genConversion l next
    Literal Id next -> genConversion l next

convert :: String -> CodeGen Converter -> CodeGen String
convert l c = do
  c' <- c
  genConversion l c'

-- Given the Haskell and Foreign types, returns the name of the
-- function marshalling between both.
hToF' :: Maybe API -> TypeRep -> TypeRep -> CodeGen Constructor
hToF' a hType fType
    | ( hType == fType ) = return Id
    | Just (APIEnum _) <- a = return "(fromIntegral . fromEnum)"
    | Just (APIFlags _) <- a = return "fromIntegral"
    | Just (APIObject _) <- a = return "(castPtr . unsafeManagedPtrGetPtr)"
    | Just (APIInterface _) <- a = return "(castPtr . unsafeManagedPtrGetPtr)"
    | ptr hType == fType = do
        let con = tyConName $ typeRepTyCon hType
        return $ P $ toPtr con
    | otherwise = return $ case (show hType, show fType) of
               ("[Char]", "CString") -> M "newCString"
               ("Word", "Type")      -> "fromIntegral"
               ("Bool", "CInt")      -> "(fromIntegral . fromEnum)"
               _                     -> error $ "don't know how to convert "
                                        ++ show hType ++ " into "
                                        ++ show fType ++ "."

hToF :: Type -> CodeGen Converter
hToF (TGList t) = do
  a <- findAPI t
  hType <- nsHaskellType t
  fType <- nsForeignType t
  innerConstructor <- hToF' a hType fType
  return $ do
    mapC innerConstructor
    apply (M "packGList")

hToF (TGSList t) = do
  a <- findAPI t
  hType <- nsHaskellType t
  fType <- nsForeignType t
  innerConstructor <- hToF' a hType fType
  return $ do
    mapC innerConstructor
    apply (M "packGSList")

hToF t = do
  a <- findAPI t
  hType <- nsHaskellType t
  fType <- nsForeignType t
  constructor <- hToF' a hType fType
  return $ apply constructor

fToH' :: Type -> Maybe API -> TypeRep -> TypeRep -> CodeGen Constructor
fToH' t a hType fType
    | ( hType == fType ) = return Id
    | Just (APIEnum _) <- a = return "(toEnum . fromIntegral)"
    | Just (APIFlags _) <- a = return "fromIntegral"
    | ptr hType == fType = do
          let constructor = tyConName $ typeRepTyCon hType
          isGO <- isGObject t
          prefixGO <- qualify "GObject"
          if isGO
          then return $ M $ parenthesize $ prefixGO ++ "makeNewObject "
                                           ++ constructor
          else do
            --- These are for routines that return
            --- abstract interfaces. We create a managed
            --- pointer without actual refcounting.
            ifaceName <- interfaceName t
            case ifaceName of
              Just _ -> return $ M $ parenthesize $
                       "\\x -> " ++ constructor ++ " <$> newForeignPtr_ x"
              _ -> return $ P constructor
    | otherwise = return $ case (show fType, show hType) of
               ("CString", "[Char]") -> M "peekCString"
               ("Type", "Word")      -> "fromIntegral"
               ("CInt", "Bool")      -> "(/= 0)"
               _                     -> error $ "don't know how to convert "
                                        ++ show fType ++ " into "
                                        ++ show hType ++ "."

fToH :: Type -> CodeGen Converter
fToH (TGList t) = do
  a <- findAPI t
  hType <- nsHaskellType t
  fType <- nsForeignType t
  innerConstructor <- fToH' t a hType fType
  return $ do
    apply (M "unpackGList")
    mapC innerConstructor

fToH (TGSList t) = do
  a <- findAPI t
  hType <- nsHaskellType t
  fType <- nsForeignType t
  innerConstructor <- fToH' t a hType fType
  return $ do
    apply (M "unpackGSList")
    mapC innerConstructor

fToH t = do
  a <- findAPI t
  hType <- nsHaskellType t
  fType <- nsForeignType t
  constructor <- fToH' t a hType fType
  return $ apply constructor

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

convertFMarshall :: String -> Type -> CodeGen String
convertFMarshall name t@(TBasicType TUTF8) = convert name (fToH t)
convertFMarshall name t@(TBasicType TFileName) = convert name (fToH t)
convertFMarshall name (TBasicType _) = return name
convertFMarshall name t = convert name (fToH t)

convertHMarshall :: String -> Type -> CodeGen String
convertHMarshall name t@(TBasicType TUTF8) = convert name (hToF t)
convertHMarshall name t@(TBasicType TFileName) = convert name (hToF t)
convertHMarshall name (TBasicType _) = return name
convertHMarshall name t = convert name (hToF t)
