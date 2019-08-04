-- | An abstraction for representing type constructors. This is a very
-- simplified version of `Data.Typeable`, which we don't use directly
-- to avoid compatibility headaches.
module Data.GI.CodeGen.Type
    ( Type(..)  -- Reexported for convenience.
    , BasicType(..)

    , TypeRep

    , con
    , con0

    , typeShow
    , typeConName

    , io
    , ptr
    , funptr
    , maybeT
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.GIR.BasicTypes (Type(..), BasicType(..))

-- | A fully applied type.
data TypeRep = TypeRep { typeCon     :: TypeCon
                       , typeConArgs :: [TypeRep]
                       } deriving (Eq)

-- | A type constructor. We single out some specific constructors
-- since they have special syntax in their Haskell representation.
data TypeCon = TupleCon
             | ListCon
             | TextualCon Text
  deriving (Eq)

-- | Give a valid Haskell source representation of the given
-- `TypeRep`.
typeShow :: TypeRep -> Text
typeShow (TypeRep TupleCon args) =
  "(" <> T.intercalate ", " (map typeShow args) <> ")"
typeShow (TypeRep ListCon args) =
  "[" <> T.intercalate ", " (map typeShow args) <> "]"
typeShow (TypeRep (TextualCon con) args) =
  T.intercalate " " (con : map (parenthesize . typeShow) args)
  where parenthesize :: Text -> Text
        parenthesize s = if T.any (== ' ') s
                         then "(" <> s <> ")"
                         else s

-- | Return a textual representation of the type constructor for the
-- given `TypeRep`.
typeConName :: TypeRep -> Text
typeConName (TypeRep TupleCon _) = "(,)"
typeConName (TypeRep ListCon _) = "[,]"
typeConName (TypeRep (TextualCon s) _) = s

-- | Type constructor applied to the given types.
con :: Text -> [TypeRep] -> TypeRep
con "[]" xs = TypeRep {typeCon = ListCon, typeConArgs = xs }
con "(,)" xs = TypeRep {typeCon = TupleCon, typeConArgs = xs }
con s xs = TypeRep {typeCon = TextualCon s, typeConArgs = xs}

-- | A shorthand for a type constructor taking no arguments.
con0 :: Text -> TypeRep
con0 c = con c []

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
