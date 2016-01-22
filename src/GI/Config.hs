module GI.Config
    ( Config(..)
    ) where

import Data.Text (Text)
import GI.Overrides (Overrides)

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Maybe Text,
      -- | Whether to print extra info.
      verbose        :: Bool,
      -- | List of loaded overrides for the code generator.
      overrides      :: Overrides
    }
