-- | Configuration for the code generator.
module Data.GI.CodeGen.Config
    ( Config(..)
    ) where

import Data.Text (Text)
import Data.GI.CodeGen.Overrides (Overrides)

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Text,
      -- | Whether to print extra info.
      verbose        :: Bool,
      -- | List of loaded overrides for the code generator.
      overrides      :: Overrides
    } deriving Show
