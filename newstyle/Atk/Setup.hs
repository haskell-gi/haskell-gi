{-# LANGUAGE OverloadedStrings #-}

import Distribution.Simple (defaultMainWithHooks)
import Data.GI.CodeGen.CabalHooks (simpleHaskellGIHooks)

main = defaultMainWithHooks (simpleHaskellGIHooks name version verbose
                             overridesFile outputDir)
    where name = "Atk"
          version = "1.0"
          verbose = False
          overridesFile = Just "Atk.overrides"
          outputDir = Nothing
