-- | Basic types used in GIR parsing.
module GI.GIR.BasicTypes
    ( ParseContext(..)
    , Alias(..)
    , Name(..)
    , Transfer(..)
    , nameInCurrentNS
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import GI.Type (Type)

-- | Name for a symbol in the GIR file.
data Name = Name { namespace :: String, name :: String }
    deriving (Eq, Ord, Show)

-- | An alias, which is simply (Namespace, name).
newtype Alias = Alias (Text, Text) deriving (Ord, Eq, Show)

-- | Info to carry around when parsing.
data ParseContext = ParseContext {
      currentNamespace :: Text,
      knownAliases     :: M.Map Alias Type
    } deriving Show

-- | Transfer mode for an argument or property.
data Transfer = TransferNothing
              | TransferContainer
              | TransferEverything
                deriving (Show, Eq, Ord)

-- | Build a name in the current namespace.
nameInCurrentNS :: ParseContext -> Text -> Name
nameInCurrentNS ctx n = Name (T.unpack (currentNamespace ctx)) (T.unpack n)
