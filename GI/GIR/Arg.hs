module GI.GIR.Arg
    ( Arg(..)
    , Direction(..)
    , Scope(..)
    ) where

import GI.Type (Type)

import GI.GIR.BasicTypes (Transfer)

data Direction = DirectionIn
               | DirectionOut
               | DirectionInout
                 deriving (Show, Eq, Ord)

data Scope = ScopeTypeInvalid
           | ScopeTypeCall
           | ScopeTypeAsync
           | ScopeTypeNotified
             deriving (Show, Eq, Ord)

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
