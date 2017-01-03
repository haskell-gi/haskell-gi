import Test.DocTest

main :: IO ()
main = doctest [ "-XCPP", "-XOverloadedStrings", "-XRankNTypes", "-XLambdaCase"
               , "-idist/build/autogen"
               , "lib/Data/GI/GIR"
               , "lib/Data/GI/CodeGen" ]
