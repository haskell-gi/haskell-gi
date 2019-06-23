{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses,
  UndecidableInstances, KindSignatures, TypeFamilies #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | `Constructible` types are those for which `new` is
-- defined. Often these are `GObject`s, but it is possible to
-- construct new (zero-initialized) structures and unions too.

module Data.GI.Base.Constructible
    ( Constructible(..)
    ) where

import Control.Monad.IO.Class (MonadIO)

import Data.GI.Base.Attributes (AttrOp, AttrOpTag(..))
import Data.GI.Base.BasicTypes (GObject, ManagedPtr)
import Data.GI.Base.GObject (constructGObject)

-- | Constructible types, i.e. those which can be allocated by `new`.
class Constructible a (tag :: AttrOpTag) where
  -- | Allocate a new instance of the given type, with the given attributes.
  new :: MonadIO m => (ManagedPtr a -> a) -> [AttrOp a tag] -> m a

-- | Default instance, assuming we have a `GObject`.
instance {-# OVERLAPPABLE #-}
    (GObject a, tag ~ 'AttrConstruct) => Constructible a tag where
        new = constructGObject
