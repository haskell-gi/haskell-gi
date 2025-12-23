{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Gio"
        version = "2.0"
        pkgName = "gi-gio"
        pkgVersion = "2.0.38"
        overridesFile = Just "Gio.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides]
