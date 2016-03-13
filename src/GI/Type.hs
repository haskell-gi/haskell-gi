
module GI.Type
    ( BasicType(..)
    , isBasicScalar
    , Type(..)
    , io
    , ptr
    , funptr
    , con
    , maybeT
    ) where

import Data.Typeable
import qualified Data.Text as T
import Data.Text (Text)

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
     | TIntPtr
     | TUIntPtr
    deriving (Eq, Enum, Show, Ord)

-- This type represents the types found in GObject Introspection
-- interfaces: the types of constants, arguments, etc.
data Type
    = TBasicType BasicType
    -- Zero terminated, Array Fixed Size, Array Length, Element Type
    | TCArray Bool Int Int Type
    | TGArray Type
    | TPtrArray Type
    | TByteArray
    | TInterface Text Text
    | TGList Type
    | TGSList Type
    | TGHash Type Type
    | TError
    | TVariant
    | TParamSpec
    deriving (Eq, Show, Ord)

-- | Whether the given type is a basic scalar, i.e. everything that is
-- not a pointer to a memory region.
isBasicScalar :: Type -> Bool
isBasicScalar (TBasicType b) = basicIsScalar b
isBasicScalar _ = False

-- | Whether the given basic type is a scalar, i.e. not a pointer to a
-- memory region.
basicIsScalar :: BasicType -> Bool
basicIsScalar TVoid = False
basicIsScalar TUTF8 = False
basicIsScalar TFileName = False
basicIsScalar _ = True

con :: Text -> [TypeRep] -> TypeRep
con "[]" xs = mkTyConApp listCon xs
              where listCon = typeRepTyCon (typeOf [True])
con "(,)" xs = mkTyConApp tupleCon xs
               where tupleCon = typeRepTyCon (typeOf (True, True))
con s xs = mkTyConApp (mkTyCon3 "GI" "GI" (T.unpack s)) xs

io :: TypeRep -> TypeRep
io t = "IO" `con` [t]

ptr :: TypeRep -> TypeRep
ptr t = "Ptr" `con` [t]

funptr :: TypeRep -> TypeRep
funptr t = "FunPtr" `con` [t]

maybeT :: TypeRep -> TypeRep
maybeT t = "Maybe" `con` [t]
