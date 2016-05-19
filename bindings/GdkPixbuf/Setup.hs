{-# LANGUAGE OverloadedStrings #-}

import Distribution.Simple (defaultMainWithHooks)
import Data.GI.CodeGen.CabalHooks (simpleHaskellGIHooks)

main = defaultMainWithHooks (simpleHaskellGIHooks name version verbose
                             overridesFile outputDir)
    where name = "GdkPixbuf"
          version = "2.0"
          verbose = False
          overridesFile = Just "GdkPixbuf.overrides"
          outputDir = Nothing
