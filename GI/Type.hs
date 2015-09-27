
module GI.Type
    ( BasicType(..)
    , Type(..)
    , io
    , ptr
    , con
    , maybeT

    , typeSize
    , typeAlign
    ) where

import Foreign.Ptr (nullPtr)
import Foreign.C.Types
import Foreign.Storable (sizeOf, alignment)
import Data.Int
import Data.Word
import Data.Typeable

import GI.Utils.BasicTypes (CGType)

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
    deriving (Eq, Enum, Show, Ord)

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
    | TVariant
    | TParamSpec
    deriving (Eq, Show, Ord)

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

-- | Compute the size in bytes of the C type corresponding to a 'Type'.
typeSize :: Type -> Int
typeSize (TBasicType b) = basicSize b
-- XXX Everything else we assume to be a pointer. This is incorrect,
-- though: often struct fields of type "TInterface a b" are not
-- pointers to the struct, but embedded structs (commmonly, a "parent"
-- struct). Taking this into account is important for accurately
-- computing the sizes of structs, and offsets of fields.
typeSize _ = sizeOf nullPtr

-- | Compute the size in bytes of the C type corresponding to a 'BasicType'
basicSize :: BasicType -> Int
basicSize TVoid = sizeOf nullPtr
basicSize TBoolean = sizeOf (0 :: CInt)
basicSize TInt8 = sizeOf (0 :: Int8)
basicSize TUInt8 = sizeOf (0 :: Word8)
basicSize TInt16 = sizeOf (0 :: Int16)
basicSize TUInt16 = sizeOf (0 :: Word16)
basicSize TInt32 = sizeOf (0 :: Int32)
basicSize TUInt32 = sizeOf (0 :: Word32)
basicSize TInt64 = sizeOf (0 :: Int64)
basicSize TUInt64 = sizeOf (0 :: Word64)
basicSize TFloat = sizeOf (0 :: CFloat)
basicSize TDouble = sizeOf (0 :: CDouble)
basicSize TUniChar = sizeOf (0 :: Word32)
basicSize TGType = sizeOf (0 :: CGType)
basicSize TUTF8 = sizeOf nullPtr
basicSize TFileName = sizeOf nullPtr

-- | Compute the alignment requirements, in bytes, for the C type
-- corresponding to the given 'Type'.
typeAlign :: Type -> Int
typeAlign (TBasicType b) = basicAlign b
typeAlign _ = sizeOf nullPtr

-- | Compute the alignment requirements, in bytes, for the C type
-- corresponding to the given 'Type'.
basicAlign :: BasicType -> Int
basicAlign TVoid = alignment nullPtr
basicAlign TBoolean = alignment (0 :: CInt)
basicAlign TInt8 = alignment (0 :: Int8)
basicAlign TUInt8 = alignment (0 :: Word8)
basicAlign TInt16 = alignment (0 :: Int16)
basicAlign TUInt16 = alignment (0 :: Word16)
basicAlign TInt32 = alignment (0 :: Int32)
basicAlign TUInt32 = alignment (0 :: Word32)
basicAlign TInt64 = alignment (0 :: Int64)
basicAlign TUInt64 = alignment (0 :: Word64)
basicAlign TFloat = alignment (0 :: CFloat)
basicAlign TDouble = alignment (0 :: CDouble)
basicAlign TUniChar = alignment (0 :: Word32)
basicAlign TGType = alignment (0 :: CGType)
basicAlign TUTF8 = alignment nullPtr
basicAlign TFileName = alignment nullPtr
