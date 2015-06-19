import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks {
         haddockHook = haddockHook
       }
    where
      haddockHook _ _ _ _ = putStrLn "Skipping haddock, this package provides no documentation."
