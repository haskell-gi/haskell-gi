
module GI.Type
    ( BasicType(..)
    , Type(..)
    , typeFromTypeInfo
    , io
    , ptr
    , con
    , haskellType
    , foreignType
    ) where

import Data.Int
import Data.Typeable
import Data.Word

import GI.Internal.BaseInfo
import GI.Internal.TypeInfo
import GI.Internal.Types

-- This enum mirrors the definition in gitypes.h.
data BasicType
     = TVoid
     | TBoolean
     | TInt8
     | TUInt8
     | TInt16
     | TUInt16
     | TInt32
     | TUInt32
     | TInt64
     | TUInt64
     | TFloat
     | TDouble
     | TUniChar
     | TGType
     | TUTF8
     | TFileName
    deriving (Eq, Enum, Show)

-- This type represents the types found in GObject-Introspection
-- interfaces: the types of constants, arguments, etc.
data Type
    = TBasicType BasicType
    | TArray Type
    | TInterface String String
    | TGList Type
    | TGSList Type
    | TGHash Type Type
    | TError
    deriving (Eq, Show)

basicTypeFromTypeTag TypeTagVoid = Just TVoid
basicTypeFromTypeTag TypeTagBoolean = Just TBoolean
basicTypeFromTypeTag TypeTagInt8 = Just TInt8
basicTypeFromTypeTag TypeTagInt16 = Just TInt16
basicTypeFromTypeTag TypeTagInt32 = Just TInt32
basicTypeFromTypeTag TypeTagInt64 = Just TInt64
basicTypeFromTypeTag TypeTagUint8 = Just TUInt8
basicTypeFromTypeTag TypeTagUint16 = Just TUInt16
basicTypeFromTypeTag TypeTagUint32 = Just TUInt32
basicTypeFromTypeTag TypeTagUint64 = Just TUInt64
basicTypeFromTypeTag TypeTagFloat = Just TFloat
basicTypeFromTypeTag TypeTagDouble = Just TDouble
basicTypeFromTypeTag TypeTagUnichar = Just TUniChar
basicTypeFromTypeTag TypeTagUtf8 = Just TUTF8
basicTypeFromTypeTag TypeTagFilename = Just TFileName
basicTypeFromTypeTag TypeTagGtype = Just TGType
basicTypeFromTypeTag _ = Nothing

typeFromTypeInfo :: TypeInfo -> Type
typeFromTypeInfo ti =
    case basicTypeFromTypeTag tag of
      Just bt -> TBasicType bt
      Nothing -> case tag of
           TypeTagArray -> TArray p1
           -- TypeTagInterface -> TInterface (typeTagToString . typeInfoTag $ ti)
           TypeTagInterface ->
               let bi = baseInfo . typeInfoInterface $ ti
                   namespace = baseInfoNamespace bi
                   name = baseInfoName bi
                in TInterface namespace name
           TypeTagGlist -> TGList p1
           TypeTagGslist -> TGSList p1
           TypeTagGhash -> TGHash p1 p2
           -- XXX: Include more information.
           TypeTagError -> TError
           _ -> error $ "implement me: " ++ show (tag, fromEnum tag, fromEnum TypeTagArray)

    where tag = typeInfoTag ti
          p1 = typeFromTypeInfo $ typeInfoParamType ti 0
          p2 = typeFromTypeInfo $ typeInfoParamType ti 1


con :: String -> [TypeRep] -> TypeRep
con s xs = mkTyConApp (mkTyCon s) xs

io :: TypeRep -> TypeRep
io t = "IO" `con` [t]

ptr :: TypeRep -> TypeRep
ptr t = "Ptr" `con` [t]

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
-- XXX: Is this correct?
haskellBasicType TGType    = typeOf (0 :: Word)
haskellBasicType TUTF8     = typeOf ""
haskellBasicType TFloat    = typeOf (0 :: Float)
haskellBasicType TDouble   = typeOf (0 :: Double)
haskellBasicType TUniChar  = typeOf ('\0' :: Char)
haskellBasicType TFileName = typeOf ""

-- This translates GI types to the types used for generated Haskell code.
haskellType :: Type -> TypeRep
haskellType (TBasicType bt) = haskellBasicType bt
haskellType (TArray a) = "GArray" `con` [haskellType a]
haskellType (TGList a) = "GList" `con` [haskellType a]
haskellType (TGSList a) = "GSList" `con` [haskellType a]
haskellType (TGHash a b) = "GHashTable" `con` [haskellType a, haskellType b]
haskellType TError = "GError" `con` []
-- We assume that any name qualification (e.g. "Checksum" ->
-- "GChecksum") has been done already, and that the interface's name
-- (i.e. ignoring the namespace) is the final name.
haskellType (TInterface _ns s) = s `con` []

foreignBasicType TVoid     = ptr (typeOf ())
foreignBasicType TBoolean  = "CInt" `con` []
foreignBasicType TUTF8     = "CString" `con` []
foreignBasicType TGType    = "GType" `con` []
foreignBasicType TFileName = "CString" `con` []
foreignBasicType t         = haskellBasicType t

-- This translates GI types to the types used in foreign function calls.
foreignType :: Type -> TypeRep
foreignType (TBasicType t) = foreignBasicType t
foreignType t@(TArray _) = ptr (haskellType t)
foreignType t@(TGList _) = ptr (haskellType t)
foreignType t@(TGSList _) = ptr (haskellType t)
foreignType t@(TGHash _ _) = ptr (haskellType t)
foreignType t@TError = ptr (haskellType t)
foreignType t@(TInterface _ _) = ptr (haskellType t)
