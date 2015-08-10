module GI.GIR.Callable
    ( Callable(..)
    ) where

import GI.Type (Type)
import GI.Internal.ArgInfo (Transfer)
import GI.GIR.Arg (Arg(..))
import GI.GIR.Deprecation (DeprecationInfo)

data Callable = Callable {
        returnType :: Type,
        returnMayBeNull :: Bool,
        returnTransfer :: Transfer,
        returnAttributes :: [(String, String)],
        args :: [Arg],
        skipReturn :: Bool,
        callableDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)
