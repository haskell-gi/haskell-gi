{- |
   == Convenience header for basic GObject-Introspection modules

See the documentation for each individual module for a description and
usage help.
-}
module Data.GI.Base
    ( module Data.GI.Base.Attributes
    , module Data.GI.Base.BasicConversions
    , module Data.GI.Base.BasicTypes
    , module Data.GI.Base.GClosure
    , module Data.GI.Base.Constructible
    , module Data.GI.Base.GError
    , module Data.GI.Base.GHashTable
    , module Data.GI.Base.GValue
    , module Data.GI.Base.GVariant
    , module Data.GI.Base.ManagedPtr
    , module Data.GI.Base.Signals
    , module Data.GI.Base.Overloading
    ) where

import Data.GI.Base.Attributes (get, set, AttrOp(..))
import Data.GI.Base.BasicConversions
import Data.GI.Base.BasicTypes
import Data.GI.Base.GClosure (GClosure)
import Data.GI.Base.Constructible (new)
import Data.GI.Base.GError
import Data.GI.Base.GHashTable
import Data.GI.Base.GValue (GValue(..), fromGValue, toGValue, IsGValue(..))
import Data.GI.Base.GVariant
import Data.GI.Base.ManagedPtr
import Data.GI.Base.Signals (on, after, SignalProxy(PropertyNotify, (:::)))
import Data.GI.Base.Overloading (asA)
