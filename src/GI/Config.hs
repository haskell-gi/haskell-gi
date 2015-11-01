module GI.Config
    ( Config(..)
    ) where

import GI.Overrides (Overrides)

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Maybe String,
      -- | Whether to print extra info.
      verbose        :: Bool,
      -- | List of loaded overrides for the code generator.
      overrides      :: Overrides
    }
