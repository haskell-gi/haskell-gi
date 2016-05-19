{-# LANGUAGE OverloadedStrings #-}

import Distribution.Simple (defaultMainWithHooks)
import Data.GI.CodeGen.CabalHooks (simpleHaskellGIHooks)

main = defaultMainWithHooks (simpleHaskellGIHooks name version verbose
                             overridesFile outputDir)
    where name = "GstAudio"
          version = "1.0"
          verbose = False
          overridesFile = Nothing
          outputDir = Nothing
