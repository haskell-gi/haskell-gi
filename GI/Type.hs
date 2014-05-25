
module GI.Type
    ( BasicType(..)
    , Type(..)
    , typeFromTypeInfo
    , io
    , ptr
    , con
    , maybeT
    ) where

import Data.Typeable

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
    -- Zero terminated, Array Fixed Size, Array Length, Element Type
    | TCArray Bool Int Int Type
    | TGArray Type
    | TPtrArray Type
    | TByteArray
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
           TypeTagArray -> case typeInfoArrayType ti of
                             ArrayTypeC         ->
                                 TCArray (typeInfoIsZeroTerminated ti)
                                         (typeInfoArrayFixedSize ti)
                                         (typeInfoArrayLength ti)
                                         p1
                             ArrayTypeArray     -> TGArray p1
                             ArrayTypePtrArray  -> TPtrArray p1
                             ArrayTypeByteArray -> TByteArray
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
con "[]" xs = mkTyConApp listCon xs
              where listCon = typeRepTyCon (typeOf [""])
con "(,)" xs = mkTyConApp tupleCon xs
               where tupleCon = typeRepTyCon (typeOf ("",""))
con s xs = mkTyConApp (mkTyCon3 "GI" "GI" s) xs

io :: TypeRep -> TypeRep
io t = "IO" `con` [t]

ptr :: TypeRep -> TypeRep
ptr t = "Ptr" `con` [t]

maybeT :: TypeRep -> TypeRep
maybeT t = "Maybe" `con` [t]
