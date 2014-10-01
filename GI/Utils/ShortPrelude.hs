-- The Haskell Prelude exports a number of symbols that can easily
-- collide with functions appearing in bindings. The generated code
-- requires just a small subset of the functions in the Prelude, which
-- we reexport explicitly here.
module GI.Utils.ShortPrelude
    ( Enum(fromEnum, toEnum)
    , Show(..)
    , Eq(..)
    , IO
    , Monad(..)
    , Maybe(..)
    , (.)
    , ($)
    , (++)
    , Bool(..)
    , Float
    , Double
    , undefined
    , error
    , map
    , length
    , mapM
    , mapM_
    , when
    , fromIntegral
    , realToFrac
    ) where

import Control.Monad (when)
