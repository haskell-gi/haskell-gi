module GI.GIR.Callable
    ( Callable(..)
    ) where

import GI.Type (Type)
import GI.GIR.Arg (Arg(..))
import GI.GIR.Deprecation (DeprecationInfo)

import GI.GIR.BasicTypes (Transfer(..))

data Callable = Callable {
        returnType :: Type,
        returnMayBeNull :: Bool,
        returnTransfer :: Transfer,
        returnAttributes :: [(String, String)],
        args :: [Arg],
        skipReturn :: Bool,
        callableDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)
