-- | Configuration for the code generator.
module Data.GI.CodeGen.Config
    ( Config(..)
    ) where

import Data.Text (Text)
import Data.GI.CodeGen.Overrides (Overrides)

data Config = Config {
      -- | GIR name of the module being generated (Gtk, GObject, ...).
      modName        :: Text,
      -- | Version of the GIR API for the package being generated
      -- ("3.0", "2.0", ...).
      modVersion     :: Text,
      -- | Haskell package being generated (gi-gtk, gi-gobject, ...).
      ghcPkgName        :: Text,
      -- | Version of the haskell package ("3.0.35", "2.0.21", ...).
      ghcPkgVersion     :: Text,
      -- | Whether to print extra info.
      verbose        :: Bool,
      -- | List of loaded overrides for the code generator.
      overrides      :: Overrides
    } deriving Show
