{-# LANGUAGE PatternGuards, DeriveFunctor, OverloadedStrings #-}

module GI.Conversions
    ( convert
    , genConversion
    , unpackCArray
    , computeArrayLength

    , marshallFType
    , convertFMarshall
    , convertHMarshall
    , hToF
    , fToH
    , haskellType
    , foreignType

    , apply
    , mapC
    , literal
    , Constructor(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Free (Free(..), liftF)
import Data.Typeable (TypeRep, tyConName, typeRepTyCon, typeOf)
import GHC.Exts (IsString(..))
import Data.Int
import Data.Word

import GI.API
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Type
import GI.Util

toPtr con = "(\\(" ++ con ++ " x) -> x)"

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

-- Given an array, together with its type, return the code for reading
-- its length.
computeArrayLength :: String -> Type -> String
computeArrayLength array (TCArray _ _ _ t) =
    "fromIntegral $ " ++ reader ++ " " ++ array
    where reader = case t of
                     TBasicType TUInt8 -> "B.length"
                     TBasicType _ -> "length"
                     TInterface _ _ -> "length"
                     _ -> error $ "Don't know how to compute length of "
                          ++ show t
computeArrayLength _ t =
    error $ "computeArrayLength called on non-CArray type " ++ show t

convert :: String -> CodeGen Converter -> CodeGen String
convert l c = do
  c' <- c
  genConversion l c'

-- Given the Haskell and Foreign types, returns the name of the
-- function marshalling between both.
hToF' :: Type -> Maybe API -> TypeRep -> TypeRep -> CodeGen Constructor
hToF' t a hType fType
    | ( hType == fType ) = return Id
    | Just (APIEnum _) <- a = return "(fromIntegral . fromEnum)"
    | Just (APIFlags _) <- a = return "fromIntegral"
    | Just (APIObject _) <- a = return "(castPtr . unsafeManagedPtrGetPtr)"
    | Just (APIInterface _) <- a = return "(castPtr . unsafeManagedPtrGetPtr)"
    | ptr hType == fType = do
        let con = tyConName $ typeRepTyCon hType
        return $ P $ toPtr con
    | TByteArray <- t = return $ M "packGByteArray"
    | TCArray True _ _ (TBasicType TUTF8) <- t =
        return $ M "packZeroTerminatedUTF8CArray"
    | TCArray True _ _ (TBasicType TFileName) <- t =
        return $ M "packZeroTerminatedFileNameArray"
    | TCArray True _ _ (TBasicType TVoid) <- t =
        return $ M "packZeroTerminatedPtrArray"
    | TCArray True _ _ (TBasicType TUInt8) <- t =
        return $ M "packZeroTerminatedByteString"
    | TCArray True _ _ (TBasicType TBoolean) <- t =
        return $ M "(packMapZeroTerminatedStorableArray (fromIntegral . fromEnum))"
    | TCArray True _ _ (TBasicType _) <- t =
        return $ M "packZeroTerminatedStorableArray"
    | TCArray False _ _ (TBasicType TUTF8) <- t =
        return $ M "packUTF8CArray"
    | TCArray False _ _ (TBasicType TFileName) <- t =
        return $ M "packFileNameArray"
    | TCArray False _ _ (TBasicType TVoid) <- t =
        return $ M "packPtrArray"
    | TCArray False _ _ (TBasicType TUInt8) <- t =
        return $ M "packByteString"
    | TCArray False _ _ (TBasicType TBoolean) <- t =
        return $ M "(packMapStorableArray (fromIntegral . fromEnum))"
    | TCArray False _ _ (TBasicType _) <- t =
        return $ M "packStorableArray"
    | TCArray _ _ _ _ <- t =
                         error $ "Don't know how to pack C array of type "
                                   ++ show t
    | otherwise = return $ case (show hType, show fType) of
               ("[Char]", "CString") -> M "newCString"
               ("ByteString", "CString") -> M "byteStringToCString"
               ("Char", "CInt")      -> "(fromIntegral . ord)"
               ("Bool", "CInt")      -> "(fromIntegral . fromEnum)"
               _                     -> error $ "don't know how to convert "
                                        ++ show hType ++ " into "
                                        ++ show fType ++ ".\n"
                                        ++ "Internal type: "
                                        ++ show t

hToF :: Type -> CodeGen Converter
hToF (TGList t) = hToF_PackedType t "packGList"
hToF (TGSList t) = hToF_PackedType t "packGSList"
hToF (TGArray t) = hToF_PackedType t "packGArray"
hToF (TPtrArray t) = hToF_PackedType t "packGPtrArray"
hToF (TCArray zt _ _ t@(TInterface _ _)) = do
  isScalar <- getIsScalar t
  let packer = if zt
               then "packZeroTerminated"
               else "pack"
  if isScalar
  then hToF_PackedType t $ packer ++ "StorableArray"
  else hToF_PackedType t $ packer ++ "PtrArray"

hToF t = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  constructor <- hToF' t a hType fType
  return $ apply constructor

hToF_PackedType t packer = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  innerConstructor <- hToF' t a hType fType
  return $ do
    mapC innerConstructor
    apply (M packer)

findInterface :: Type -> CodeGen (Maybe Interface)
findInterface t = do
    a <- findAPI t
    case a of
         Just (APIInterface iface) -> return $ Just iface
         _ -> return Nothing

fToH' :: Type -> Maybe API -> TypeRep -> TypeRep -> CodeGen Constructor
fToH' t a hType fType
    | ( hType == fType ) = return Id
    | Just (APIEnum _) <- a = return "(toEnum . fromIntegral)"
    | Just (APIFlags _) <- a = return "fromIntegral"
    | ptr hType == fType = do
          let constructor = tyConName $ typeRepTyCon hType
          isGO <- isGObject t
          if isGO
          then return $ M $ parenthesize $ "makeNewObject " ++ constructor
          else do
            --- These are for routines that return abstract interfaces
            --- which are not GObjects.
            maybeIface <- findInterface t
            case maybeIface of
              Just _ -> do
                line $ "-- XXX (Leak) Interface does not require GObject : "
                         ++ show t
                return $ M $ parenthesize $
                       "\\x -> " ++ constructor ++ " <$> newForeignPtr_ x"
              Nothing -> return $ P constructor
    | TCArray True _ _ (TBasicType TUTF8) <- t =
        return $ M "unpackZeroTerminatedUTF8CArray"
    | TCArray True _ _ (TBasicType TFileName) <- t =
        return $ M "unpackZeroTerminatedFileNameArray"
    | TCArray True _ _ (TBasicType TUInt8) <- t =
        return $ M "unpackZeroTerminatedByteString"
    | TCArray True _ _ (TBasicType TVoid) <- t =
        return $ M "unpackZeroTerminatedPtrArray"
    | TCArray True _ _ (TBasicType TBoolean) <- t =
        return $ M "(unpackMapZeroTerminatedStorableArray (/= 0))"
    | TCArray True _ _ (TBasicType _) <- t =
        return $ M "unpackZeroTerminatedStorableArray"
    | TCArray _ _ _ _ <- t =
                         error $
                           "fToH' : Don't know how to unpack C array of type "
                           ++ show t
    | TByteArray <- t = return $ M "unpackGByteArray"
    | otherwise = return $ case (show fType, show hType) of
               ("CString", "[Char]") -> M "peekCString"
               ("CString", "ByteString") -> M "B.packCString"
               ("CInt", "Char")      -> "(chr . fromIntegral)"
               ("CInt", "Bool")      -> "(/= 0)"
               _                     -> error $ "don't know how to convert "
                                        ++ show fType ++ " into "
                                        ++ show hType ++ ".\n"
                                        ++ "Internal type: "
                                        ++ show t


fToH :: Type -> CodeGen Converter

fToH (TGList t) = fToH_PackedType t "unpackGList"
fToH (TGSList t) = fToH_PackedType t "unpackGSList"
fToH (TGArray t) = fToH_PackedType t "unpackGArray"
fToH (TPtrArray t) = fToH_PackedType t "unpackGPtrArray"
fToH (TCArray True _ _ t@(TInterface _ _)) = do
  isScalar <- getIsScalar t
  if isScalar
  then fToH_PackedType t "unpackZeroTerminatedStorableArray"
  else fToH_PackedType t "unpackZeroTerminatedPtrArray"

fToH t = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  constructor <- fToH' t a hType fType
  return $ apply constructor

fToH_PackedType t unpacker = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  innerConstructor <- fToH' t a hType fType
  return $ do
    apply (M unpacker)
    mapC innerConstructor

unpackCArray :: String -> Type -> CodeGen Converter
unpackCArray length (TCArray False _ _ t) =
  case t of
    TBasicType TUTF8 -> return $ apply $ M $ parenthesize $
                        "unpackUTF8CArrayWithLength " ++ length
    TBasicType TFileName -> return $ apply $ M $ parenthesize $
                            "unpackFileNameArrayWithLength " ++ length
    TBasicType TUInt8 -> return $ apply $ M $ parenthesize $
                         "unpackByteStringWithLength " ++ length
    TBasicType TVoid -> return $ apply $ M $ parenthesize $
                         "unpackPtrArrayWithLength " ++ length
    TBasicType TBoolean -> return $ apply $ M $ parenthesize $
                         "unpackMapStorableArrayWithLength (/= 0) " ++ length
    TBasicType _ -> return $ apply $ M $ parenthesize $
                         "unpackStorableArrayWithLength " ++ length
    TInterface _ _ -> do
           a <- findAPI t
           isScalar <- getIsScalar t
           hType <- haskellType t
           fType <- foreignType t
           innerConstructor <- fToH' t a hType fType
           let unpacker = if isScalar
                          then "unpackStorableArrayWithLength"
                          else "unpackPtrArrayWithLength"
           return $ do
             apply $ M $ parenthesize $ unpacker ++ " " ++ length
             mapC innerConstructor
    _ -> error $ "unpackCArray : Don't know how to unpack C Array of type "
                 ++ show t

unpackCArray _ _ = error $ "unpackCArray : unexpected array type."

-- The signal marshaller C code has some built in support for basic
-- types, so we only generate conversions for things that the
-- marshaller cannot do itself. (This list should be kept in sync with
-- hsgclosure.c)

-- Marshaller to haskell types.
-- There is no support in the marshaller for converting Haskell
-- strings into C strings directly.
marshallFType :: Type -> CodeGen TypeRep
marshallFType t@(TBasicType TUTF8) = foreignType t
marshallFType t@(TBasicType TFileName) = foreignType t
marshallFType t@(TBasicType _) = haskellType t
marshallFType a = foreignType a

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

haskellBasicType TVoid     = (ptr (typeOf ()))
haskellBasicType TBoolean  = typeOf True
haskellBasicType TInt8     = typeOf (0 :: Int8)
haskellBasicType TUInt8    = typeOf (0 :: Word8)
haskellBasicType TInt16    = typeOf (0 :: Int16)
haskellBasicType TUInt16   = typeOf (0 :: Word16)
haskellBasicType TInt32    = typeOf (0 :: Int32)
haskellBasicType TUInt32   = typeOf (0 :: Word32)
haskellBasicType TInt64    = typeOf (0 :: Int64)
haskellBasicType TUInt64   = typeOf (0 :: Word64)
haskellBasicType TGType    = "GType" `con` []
 -- XXX Text may be more appropriate
haskellBasicType TUTF8     = typeOf ("" :: String)
haskellBasicType TFloat    = typeOf (0 :: Float)
haskellBasicType TDouble   = typeOf (0 :: Double)
haskellBasicType TUniChar  = typeOf ('\0' :: Char)
haskellBasicType TFileName = "ByteString" `con` []

-- This translates GI types to the types used for generated Haskell code.
haskellType :: Type -> CodeGen TypeRep
haskellType (TBasicType bt) = return $ haskellBasicType bt
haskellType (TCArray _ _ _ (TBasicType TUInt8)) =
    return $ "ByteString" `con` []
haskellType (TCArray _ _ _ (TBasicType b)) =
    return $ "[]" `con` [haskellBasicType b]
haskellType (TCArray _ _ _ a@(TInterface _ _)) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TCArray _ _ _ a) =
    error $ "haskellType : Don't recognize CArray of type " ++ show a
haskellType (TGArray a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TPtrArray a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TByteArray) = return $ "ByteString" `con` []
haskellType (TGList a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGSList a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGHash a b) = do
  innerA <- haskellType a
  innerB <- haskellType b
  return $ "GHashTable" `con` [innerA, innerB]
haskellType TError = return $ "Error" `con` []
haskellType (TInterface ns n) = do
  prefix <- qualify ns
  return $ (prefix ++ n) `con` []

foreignBasicType TVoid     = ptr (typeOf ())
foreignBasicType TBoolean  = "CInt" `con` []
foreignBasicType TUTF8     = "CString" `con` []
foreignBasicType TFileName = "CString" `con` []
foreignBasicType TUniChar  = "CInt" `con` []
foreignBasicType t         = haskellBasicType t

-- This translates GI types to the types used in foreign function calls.
foreignType :: Type -> CodeGen TypeRep
foreignType (TBasicType t) = return $ foreignBasicType t
foreignType (TCArray _ _ _ t) = ptr <$> foreignType t
foreignType (TGArray a) = do
  inner <- foreignType a
  return $ ptr ("GArray" `con` [inner])
foreignType (TPtrArray a) = do
  inner <- foreignType a
  return $ ptr ("GPtrArray" `con` [inner])
foreignType (TByteArray) = return $ ptr ("GByteArray" `con` [])
foreignType (TGList a) = do
  inner <- foreignType a
  return $ ptr ("GList" `con` [inner])
foreignType (TGSList a) = do
  inner <- foreignType a
  return $ ptr ("GSList" `con` [inner])
foreignType t@(TGHash _ _) = ptr <$> haskellType t
foreignType t@TError = ptr <$> haskellType t
foreignType t@(TInterface ns n) = do
  isScalar <- getIsScalar t
  if isScalar
  then return $ "Word" `con` []
  else do
    prefix <- qualify ns
    return $ ptr $ (prefix ++ n) `con` []

getIsScalar :: Type -> CodeGen Bool
getIsScalar t = do
  a <- findAPI t
  case a of
    Nothing -> return False
    (Just (APIEnum _)) -> return True
    (Just (APIFlags _)) -> return True
    _ -> return False
