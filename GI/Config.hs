module GI.Config
    ( Config(..)
    ) where

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Maybe String,
      -- | Whether to print extra info.
      verbose        :: Bool
    } deriving Show
