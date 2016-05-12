{-# LANGUAGE OverloadedStrings #-}

import Distribution.Simple
import Data.GI.CodeGen.CabalHooks (confCodeGenHook)

main = defaultMainWithHooks simpleUserHooks
  { confHook = confCodeGenHook name version verbose overridesFile outputDir
               (confHook simpleUserHooks)
  }
    where name = "GLib"
          version = "2.0"
          verbose = False
          overridesFile = Just "GLib.overrides"
          outputDir = Nothing
