
module GI.Type
    ( BasicType(..)
    , Type(..)
    , io
    , ptr
    , con
    , maybeT
    ) where

import Data.Typeable

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
