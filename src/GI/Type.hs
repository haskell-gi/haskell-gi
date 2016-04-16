-- | Type constructors.
module GI.Type
    ( Type(..)  -- Reexported for convenience.
    , BasicType(..)

    , io
    , ptr
    , funptr
    , con
    , maybeT
    ) where

import Data.Typeable (TypeRep, mkTyConApp, typeOf, typeRepTyCon, mkTyCon3)
import qualified Data.Text as T
import Data.Text (Text)

import GI.GIR.BasicTypes (Type(..), BasicType(..))

-- | Type constructor applied to the given types.
con :: Text -> [TypeRep] -> TypeRep
con "[]" xs = mkTyConApp listCon xs
              where listCon = typeRepTyCon (typeOf [True])
con "(,)" xs = mkTyConApp tupleCon xs
               where tupleCon = typeRepTyCon (typeOf (True, True))
con s xs = mkTyConApp (mkTyCon3 "GI" "GI" (T.unpack s)) xs

-- | Embed in the `IO` monad.
io :: TypeRep -> TypeRep
io t = "IO" `con` [t]

-- | A `Ptr` to the type.
ptr :: TypeRep -> TypeRep
ptr t = "Ptr" `con` [t]

-- | A `FunPtr` to the type.
funptr :: TypeRep -> TypeRep
funptr t = "FunPtr" `con` [t]

-- | Embed in the `Maybe` monad.
maybeT :: TypeRep -> TypeRep
maybeT t = "Maybe" `con` [t]
