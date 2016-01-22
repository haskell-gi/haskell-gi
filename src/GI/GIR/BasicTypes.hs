-- | Basic types used in GIR parsing.
module GI.GIR.BasicTypes
    ( Name(..)
    , Transfer(..)
    , Alias(..)
    ) where

import Data.Text (Text)

-- | Name for a symbol in the GIR file.
data Name = Name { namespace :: Text, name :: Text }
    deriving (Eq, Ord, Show)

-- | Transfer mode for an argument or property.
data Transfer = TransferNothing
              | TransferContainer
              | TransferEverything
                deriving (Show, Eq, Ord)

-- | An alias, which is simply (Namespace, name).
newtype Alias = Alias (Text, Text) deriving (Ord, Eq, Show)
