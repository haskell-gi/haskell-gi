module GI.GIR.Signal
    ( Signal(..)
    ) where

import GI.GIR.Callable (Callable(..))
import GI.GIR.Deprecation (DeprecationInfo)

data Signal = Signal {
        sigName :: String,
        sigCallable :: Callable,
        sigDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)
