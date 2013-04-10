module System.Glib.Initialize (initArgs) where

import System.Environment (getArgs)

foreign import ccall "g_type.h g_type_init"
    typeInit :: IO ()

initArgs :: IO [String]
initArgs = do
    typeInit -- Initialize the Glib's type system
    getArgs
