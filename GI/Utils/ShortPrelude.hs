-- The Haskell Prelude exports a number of symbols that can easily
-- collide with functions appearing in bindings. The generated code
-- requires just a small subset of the functions in the Prelude, which
-- we reexport explicitly here.
module GI.Utils.ShortPrelude
    ( Enum(..)
    , Show(..)
    , Eq(..)
    , IO
    , Monad(..)
    , Maybe(..)
    , (.)
    , ($)
    , Bool(..)
    , Float
    , Double
    , undefined
    , map
    , length
    , mapM
    , mapM_
    , fromIntegral
    , realToFrac
    ) where

