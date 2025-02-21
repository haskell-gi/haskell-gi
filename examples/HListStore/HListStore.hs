-- | A typed ListStore which is safe to use from Haskell.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE CPP #-}

-- Haskell-gi embeds much of the subclassing information on the type
-- level. The following extensions allow us to do the needful.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

module HListStore
  ( HListStore(..)
  , IsHListStore
  , toHListStore

  , hListStoreAppend
  , hListStoreFind
  , hListStoreGetItem
  , hListStoreInsert
  , hListStoreInsertSorted
  , hListStoreInsertSortedBy
  , hListStoreSort
  , hListStoreSortBy
  , hListStoreSplice
  ) where

import Data.GI.Base (GObject, TypedObject(glibType), ManagedPtr(..),
                     new, AttrOp((:=)), withTransient, unsafeCastTo)
import Data.GI.Base.Attributes(AttrOpTag(AttrConstruct))
import Data.GI.Base.Constructible (Constructible)
import Data.GI.Base.GObject (DerivedGObject(..), registerGType,
                             constructGObject)
import qualified Data.GI.Base.Overloading as O

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr)

import GOWrapper (GOWrapper(..), goWrapHValue, goUnwrapHValue)

import GHC.OverloadedLabels as OL

#if MIN_VERSION_base(4,13,0)
import qualified GHC.Records as R
#endif

import qualified GI.Gio as Gio
import qualified GI.GObject as GObject

-- The basic type definition. This should always have the form
--
-- > newtype X = X (ManagedPtr X)
--
-- for any type that you wish to define.
newtype HListStore a = HListStore (ManagedPtr (HListStore a))

-- Declare that the new type is a GObject, with a type to be
-- registered at runtime. The information on the type will be declared
-- in the 'DerivedGObject' instance below.
instance TypedObject (HListStore a) where
  glibType = registerGType @(HListStore a) HListStore

instance GObject (HListStore a)

-- Information for the type system. This will be picked up by
-- 'registerGType' above.
instance DerivedGObject (HListStore a) where
  -- The parent type.
  type GObjectParentType (HListStore a) = Gio.ListStore

  -- Every custom type has associated private data, which can be of
  -- any type.
  type GObjectPrivateData (HListStore a) = ()

  -- Name of the type we are about to register. Make sure that it does
  -- not clash with an existing type name. See
  -- https://developer.gnome.org/gobject/stable/gtype-conventions.html
  -- for the naming conventions.
  objectTypeName = "HaskellGI-HListStore"

  -- This is run when the class is instantiated for the first time,
  -- typically when the first object of this type is created.
  objectClassInit = \_ -> return ()

  -- This is run for each instance of the type (each new object of
  -- this type being created). The main goal of this function is to
  -- prepare the private data for each object.
  objectInstanceInit = \_ _ -> return ()

-- Our type descends from a parent type, and implements various
-- interfaces (by virtue of descending from its parent type), make
-- sure that the type system knows about that. First we make our type
-- an instance of O.HasParentTypes (having this class leads to better
-- error messages).
instance O.HasParentTypes (HListStore a)

-- We declare what our parent types are here. This is a type-level
-- list of every type that we can be safely cast to. This is our
-- direct parent and its ancestors, and includes implemented
-- interfaces too.
type instance O.ParentTypes (HListStore a) = Gio.ListStore ': O.ParentTypes Gio.ListStore

-- The following is a typeclass that encodes whether a given type
-- descends from us.
class (GObject o, O.IsDescendantOf (HListStore a) o) => IsHListStore a o
instance (GObject o, O.IsDescendantOf (HListStore a) o) => IsHListStore a o

-- If we have a type that descends from us, it can be safely cast to
-- our type.
toHListStore :: (MonadIO m, IsHListStore a o) => o -> m (HListStore a)
toHListStore = liftIO . unsafeCastTo HListStore

-- Allow the overloaded attribute syntax to work. In this case we
-- inherit all attributes of our parent type.
instance O.HasAttributeList (HListStore a)
type instance O.AttributeList (HListStore a) = O.AttributeList Gio.ListStore

-- Support overloaded signals. The following says that we support all
-- signals of our parent type.
type instance O.SignalList (HListStore a) = O.SignalList Gio.ListStore


-- | Make sure to set the appropriate element type when constructing
-- the 'HListStore'.
instance {-# OVERLAPPING #-}
  (tag ~ 'AttrConstruct) => Constructible (HListStore a) tag where
  new = constructHListStore

constructHListStore :: forall a m. (MonadIO m)
  => (ManagedPtr (HListStore a) -> HListStore a)
  -> [AttrOp (HListStore a) 'AttrConstruct]
  -> m (HListStore a)
constructHListStore constructor attrs = liftIO $ do
  itemType <- glibType @(GOWrapper a)
  constructGObject constructor $ (#itemType := itemType) : attrs

#if MIN_VERSION_base(4,13,0)
{- The circular instance trick is to avoid the liberal coverage
condition. We should be using DYSFUNCTIONAL pragmas instead, once
those are implemented:
https://github.com/ghc-proposals/ghc-proposals/pull/374
-}
instance (info ~ ResolveHListStoreMethod method (HListStore a),
          O.OverloadedMethod info (HListStore a) p,
          R.HasField method (HListStore a) p)
    => R.HasField method (HListStore a) p where
  getField = O.overloadedMethod @info
#endif

-- Make overloaded labels applied to HListStore resolve to methods.
instance (info ~ ResolveHListStoreMethod t (HListStore a),
          O.OverloadedMethod info (HListStore a) p)
         => OL.IsLabel t (HListStore a -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

-- This is useful for debugging
instance (info ~ ResolveHListStoreMethod t (HListStore a),
          O.OverloadedMethodInfo info (HListStore a))
         => OL.IsLabel t (O.MethodProxy info (HListStore a)) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

-- Support for overloaded methods. We support the same as our parent
-- type.
type family ResolveHListStoreMethod t o where
  ResolveHListStoreMethod "append" o = HListStoreAppendMethodInfo
  ResolveHListStoreMethod "find" o = HListStoreFindMethodInfo
  ResolveHListStoreMethod "getItem" o = HListStoreGetItemMethodInfo
  ResolveHListStoreMethod "insert" o = HListStoreInsertMethodInfo
  ResolveHListStoreMethod "insertSorted" o = HListStoreInsertSortedMethodInfo
  ResolveHListStoreMethod "insertSortedBy" o = HListStoreInsertSortedByMethodInfo
  ResolveHListStoreMethod "sort" o = HListStoreSortMethodInfo
  ResolveHListStoreMethod "sortBy" o = HListStoreSortByMethodInfo
  ResolveHListStoreMethod "splice" o = HListStoreSpliceMethodInfo
  ResolveHListStoreMethod t o = Gio.ResolveListStoreMethod t o

data HListStoreAppendMethodInfo
instance (signature ~ (a -> m ()), MonadIO m) => O.OverloadedMethod HListStoreAppendMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreAppend

hListStoreAppend :: MonadIO m => HListStore a -> a -> m ()
hListStoreAppend ls value = liftIO $ do
  wrapped <- goWrapHValue value
  Gio.listStoreAppend ls wrapped

data HListStoreInsertMethodInfo
instance (signature ~ (Word32 -> a -> m ()), MonadIO m) => O.OverloadedMethod HListStoreInsertMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreInsert

hListStoreInsert :: MonadIO m => HListStore a -> Word32 -> a -> m ()
hListStoreInsert ls idx value = liftIO $ do
  wrapped <- goWrapHValue value
  Gio.listStoreInsert ls idx wrapped

data HListStoreInsertSortedMethodInfo
instance (signature ~ (a -> m Word32), MonadIO m, Ord a) => O.OverloadedMethod HListStoreInsertSortedMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreInsertSorted

hListStoreInsertSorted :: forall a m. (MonadIO m, Ord a) =>
                          HListStore a -> a -> m Word32
hListStoreInsertSorted ls value = hListStoreInsertSortedBy ls value compare

data HListStoreInsertSortedByMethodInfo
instance (signature ~ (a -> (a -> a -> Ordering) -> m Word32), MonadIO m) => O.OverloadedMethod HListStoreInsertSortedByMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreInsertSortedBy

hListStoreInsertSortedBy :: forall a m. MonadIO m =>
                          HListStore a -> a -> (a -> a -> Ordering) -> m Word32
hListStoreInsertSortedBy ls value ordering = liftIO $ do
  wrapped <- goWrapHValue value
  Gio.listStoreInsertSorted ls wrapped $ \xPtr yPtr ->
    let xPtr' = castPtr xPtr :: Ptr (GOWrapper a)
        yPtr' = castPtr yPtr :: Ptr (GOWrapper a)
    in withTransient xPtr' $ \wrappedX ->
      withTransient yPtr' $ \wrappedY -> do
        x <- goUnwrapHValue wrappedX
        y <- goUnwrapHValue wrappedY
        case ordering x y of
          LT -> return (-1)
          EQ -> return 0
          GT -> return 1

data HListStoreSortMethodInfo
instance (signature ~ m (), MonadIO m, Ord a) => O.OverloadedMethod HListStoreSortMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreSort

hListStoreSort :: forall a m. (MonadIO m, Ord a) =>
                  HListStore a -> m ()
hListStoreSort ls = hListStoreSortBy ls compare

data HListStoreSortByMethodInfo
instance (signature ~ ((a -> a -> Ordering) -> m ()), MonadIO m) => O.OverloadedMethod HListStoreSortByMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreSortBy

hListStoreSortBy :: forall a m. MonadIO m =>
                  HListStore a -> (a -> a -> Ordering) -> m ()
hListStoreSortBy ls ordering = liftIO $ do
  Gio.listStoreSort ls $ \xPtr yPtr ->
    let xPtr' = castPtr xPtr :: Ptr (GOWrapper a)
        yPtr' = castPtr yPtr :: Ptr (GOWrapper a)
    in withTransient xPtr' $ \wrappedX ->
      withTransient yPtr' $ \wrappedY -> do
        x <- goUnwrapHValue wrappedX
        y <- goUnwrapHValue wrappedY
        case ordering x y of
          LT -> return (-1)
          EQ -> return 0
          GT -> return 1

data HListStoreFindMethodInfo
instance (signature ~ (a -> m (Bool, Word32)), MonadIO m, Eq a) =>
  O.OverloadedMethod HListStoreFindMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreFind

hListStoreFind :: forall a m. (MonadIO m, Eq a) =>
                  HListStore a -> a -> m (Bool, Word32)
hListStoreFind ls x = liftIO $ do
  Gio.listStoreFindWithEqualFunc ls (Nothing @(GOWrapper a)) $ \yPtr _ ->
    let yPtr' = castPtr yPtr :: Ptr (GOWrapper a)
    in withTransient yPtr' $ \wrappedY -> do
      y <- goUnwrapHValue wrappedY
      return $ x == y

data HListStoreGetItemMethodInfo
instance (signature ~ (Word32 -> m (Maybe a)), MonadIO m) => O.OverloadedMethod HListStoreGetItemMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreGetItem

hListStoreGetItem :: forall a m. MonadIO m =>
                     HListStore a -> Word32 -> m (Maybe a)
hListStoreGetItem ls idx = liftIO $ do
  gobject <- Gio.listModelGetItem ls idx
  case coerce gobject :: Maybe (GOWrapper a) of
    Nothing -> return Nothing
    Just wrapped -> Just <$> goUnwrapHValue wrapped

data HListStoreSpliceMethodInfo
instance (signature ~ (Word32 -> Word32 -> [a] -> m ()), MonadIO m) => O.OverloadedMethod HListStoreSpliceMethodInfo (HListStore a) signature where
    overloadedMethod = hListStoreSplice

hListStoreSplice :: forall a m. MonadIO m =>
                     HListStore a -> Word32 -> Word32 -> [a] -> m ()
hListStoreSplice ls position nremovals additions = liftIO $ do
  gobjects <- mapM (goWrapHValue >=> GObject.toObject) additions
  Gio.listStoreSplice ls position nremovals gobjects
