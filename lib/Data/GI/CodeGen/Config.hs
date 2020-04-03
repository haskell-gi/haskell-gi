-- | Configuration for the code generator.
module Data.GI.CodeGen.Config
    ( Config(..)
    ) where

import qualified Data.Map as M

import Data.Text (Text)
import Data.GI.CodeGen.Overrides (Overrides)
import Data.GI.CodeGen.LibGIRepository (Typelib)

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Text,
      -- | Whether to print extra info.
      verbose        :: Bool,
      -- | List of loaded overrides for the code generator.
      overrides      :: Overrides,
      -- | Map from namespaces to the corresponding loaded typelibs.
      typelibMap     :: M.Map Text Typelib
    } deriving Show
