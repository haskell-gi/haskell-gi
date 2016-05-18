{-# LANGUAGE OverloadedStrings #-}

import Distribution.Simple (defaultMainWithHooks)
import Data.GI.CodeGen.CabalHooks (simpleHaskellGIHooks)

main = defaultMainWithHooks (simpleHaskellGIHooks name version verbose
                             overridesFile outputDir)
    where name = "WebKit2"
          version = "4.0"
          verbose = False
          overridesFile = Just "WebKit2.overrides"
          outputDir = Nothing
