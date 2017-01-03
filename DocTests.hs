import Test.DocTest

main :: IO ()
main = doctest [ "-XCPP", "-XOverloadedStrings", "-XRankNTypes", "-XLambdaCase"
               , "-ilib"
               , "Data.GI.GIR.GtkDoc"
               , "Data.GI.CodeGen.ModulePath"
               , "Data.GI.CodeGen.SymbolNaming" ]
