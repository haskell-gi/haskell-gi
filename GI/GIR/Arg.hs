module GI.GIR.Arg
    ( Arg(..)
    ) where

import GI.Type (Type)
import GI.Internal.ArgInfo (Direction, Scope, Transfer)

data Arg = Arg {
        argName :: String,
        argType :: Type,
        direction :: Direction,
        mayBeNull :: Bool,
        argScope :: Scope,
        argClosure :: Int,
        argDestroy :: Int,
        transfer :: Transfer
    } deriving (Show, Eq, Ord)
