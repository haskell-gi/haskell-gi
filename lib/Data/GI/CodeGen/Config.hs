-- | Configuration for the code generator.
module Data.GI.CodeGen.Config
    ( Config(..)
    , CodeGenFlags(..)
    ) where

import Data.Text (Text)
import Data.GI.CodeGen.Overrides (Overrides)

-- | Flags controlling different aspects of the code generator.
data CodeGenFlags = CodeGenFlags {
    -- | Whether to generate overloaded properties.
      cgOverloadedProperties :: Bool
    -- | Whether to generate support for overloaded signals.
    , cgOverloadedSignals    :: Bool
    -- | Whether to generate support for overloaded methods.
    , cgOverloadedMethods    :: Bool
    } deriving Show

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Maybe Text,
      -- | Whether to print extra info.
      verbose        :: Bool,
      -- | List of loaded overrides for the code generator.
      overrides      :: Overrides,
      -- | List of flags for the code generator.
      cgFlags        :: CodeGenFlags
    } deriving Show
