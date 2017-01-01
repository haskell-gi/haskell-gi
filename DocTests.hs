import Test.DocTest

main :: IO ()
main = doctest [ "-XCPP", "-XOverloadedStrings"
               , "lib/Data/GI/GIR/GtkDoc.hs"]
