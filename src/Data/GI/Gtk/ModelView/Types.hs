{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts
--
--  Created: 31 March 2006
--
--  Copyright (C) 2006-2007 Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Common types and classes for the ModelView modules.
--
module Graphics.UI.Gtk.ModelView.Types (
  TypedTreeModel(..),
  TypedTreeModelClass,
  toTypedTreeModel,
  unsafeTreeModelToGeneric,

  TypedTreeModelSort(..),
  unsafeTreeModelSortToGeneric,
  TypedTreeModelFilter(..),
  unsafeTreeModelFilterToGeneric,

  -- TreeIterRaw
  TreeIterRaw(..),
  receiveTreeIter,
  peekTreeIter,
  treeIterSetStamp,
  treeIterNew,
  treeIterFromRaw,
  treeIterToRaw,

  -- TreePath
  treePathNewFromIndices',
  treePathGetIndices',
  withTreePath,
  maybeWithTreePath,
  stringToTreePath,

  treeSelectionGetSelectedRows',

  -- Columns
  ColumnAccess(..),
  ColumnId(..),

  -- Storing the model in a ComboBox
  comboQuark,

  equalManagedPtr
  ) where

import GHC.Exts (unsafeCoerce#)

import Data.Char ( isDigit )
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Coerce (coerce)
import Control.Monad ( liftM )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (catch)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr, minusPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Utils (with)
import Data.GI.Base.BasicTypes (ForeignPtrNewtype, UnexpectedNullPointerReturn, GObject(..))
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.GI.Base.GValue (GValue)
import Data.GI.Base.Overloading (ParentTypes)
import GI.GObject.Objects.Object (Object(..))
import GI.Gtk.Interfaces.TreeModel (TreeModel)
import GI.Gtk.Objects.TreeModelSort (TreeModelSort)
import GI.Gtk.Objects.TreeSelection (TreeSelectionK, treeSelectionCountSelectedRows, treeSelectionGetSelectedRows)
import GI.Gtk.Objects.TreeModelFilter (TreeModelFilter)
import GI.Gtk.Interfaces.TreeSortable (TreeSortable)
import GI.GLib.Functions (quarkFromString)
import GI.GdkPixbuf.Objects.Pixbuf (Pixbuf(..))
import GI.Gtk.Structs.TreeIter (TreeIter(..), treeIterCopy)
import GI.Gtk.Structs.TreePath (TreePath(..), treePathGetIndices, treePathNewFromIndices, treePathNew, treePathGetDepth)

nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr

equalManagedPtr :: ForeignPtrNewtype a => a -> a -> Bool
equalManagedPtr a b =
    (coerce a :: ForeignPtr ()) == (coerce b :: ForeignPtr ())

{# context lib="gtk" prefix="gtk" #}

newtype TypedTreeModel row = TypedTreeModel (ForeignPtr (TypedTreeModel row))

class TypedTreeModelClass model where
  dummy :: model a -> a
  dummy _ = error "not used"
  -- this is to get the right kind for model :: * -> *
  -- TODO: when haddock is fixed we can use an explicit kind annotation

toTypedTreeModel :: TypedTreeModelClass model => model row -> TypedTreeModel row
toTypedTreeModel = unsafeCoerce#

unsafeTreeModelToGeneric :: TreeModel -> model row
unsafeTreeModelToGeneric = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModel

newtype TypedTreeModelSort row = TypedTreeModelSort (ForeignPtr (TypedTreeModelSort row))

type instance ParentTypes (TypedTreeModelSort row) = TypedTreeModelSortParentTypes
type TypedTreeModelSortParentTypes = '[TreeModelSort, TreeSortable, TreeModel, Object]

instance GObject (TypedTreeModelSort row) where
    gobjectIsInitiallyUnowned _ = False
    gobjectType _ = gobjectType (undefined :: TreeModelSort)

unsafeTreeModelSortToGeneric :: TreeModelSort -> TypedTreeModelSort row
unsafeTreeModelSortToGeneric = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModelSort

newtype TypedTreeModelFilter row = TypedTreeModelFilter (ForeignPtr (TypedTreeModelFilter row))

unsafeTreeModelFilterToGeneric :: TreeModelFilter -> TypedTreeModelFilter row
unsafeTreeModelFilterToGeneric = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModelFilter

-- | Tree Iterator: a pointer to an entry in a
-- 'Graphics.UI.Gtk.ModelView.TreeModel'. The constructor of this structure is
-- public for the sake of creating custom tree models. The first value is a
-- time stamp that is handled by the functions that interface with Gtk. The
-- time stamps are used to print warnings if programmers use an iter to a
-- model that has changed meanwhile. The other three fields are used by the
-- custom model implementation to implement an indexing scheme. The precise
-- use of the three words is therefore implementation specific. See also
-- 'TreePath'.
--
-- TreeIterRaw is for implementation of
data TreeIterRaw = TreeIterRaw {-# UNPACK #-} !CInt !Word32 !Word32 !Word32
              deriving Show

{#pointer *TreeIter as TreeIterRawPtr -> TreeIterRaw #}

treeIterNew :: MonadIO m => CInt -> Word32 -> Word32 -> Word32 -> m TreeIter
treeIterNew s u1 u2 u3 = treeIterFromRaw (TreeIterRaw s u1 u2 u3)

treeIterFromRaw :: MonadIO m => TreeIterRaw -> m TreeIter
treeIterFromRaw raw = liftIO $ with raw (\p -> newForeignPtr_ (castPtr p) >>= treeIterCopy . TreeIter )

treeIterToRaw :: MonadIO m => TreeIter -> m TreeIterRaw
treeIterToRaw i = liftIO $ withManagedPtr i (peek . castPtr)

instance Storable TreeIterRaw where
  sizeOf _ = {# sizeof TreeIter #}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    stamp      <- {# get TreeIter->stamp      #} ptr
    user_data  <- {# get TreeIter->user_data  #} ptr
    user_data2 <- {# get TreeIter->user_data2 #} ptr
    user_data3 <- {# get TreeIter->user_data3 #} ptr
    return (TreeIterRaw stamp (ptrToWord user_data)
                           (ptrToWord user_data2)
                           (ptrToWord user_data3))

    where ptrToWord :: Ptr a -> Word32
          ptrToWord ptr = fromIntegral (ptr `minusPtr` nullPtr)

  poke ptr (TreeIterRaw stamp user_data user_data2 user_data3) = do
    {# set TreeIter->stamp      #} ptr stamp
    {# set TreeIter->user_data  #} ptr (wordToPtr user_data)
    {# set TreeIter->user_data2 #} ptr (wordToPtr user_data2)
    {# set TreeIter->user_data3 #} ptr (wordToPtr user_data3)

    where wordToPtr :: Word32 -> Ptr a
          wordToPtr word = nullPtr `plusPtr` fromIntegral word

-- Pass a pointer to a structure large enough to hold a GtkTreeIter
-- structure. If the function returns true, read the tree iter and
-- return it.
receiveTreeIter :: (Ptr TreeIterRaw -> IO CInt) -> IO (Maybe TreeIterRaw)
receiveTreeIter body =
  alloca $ \iterPtr -> do
  result <- body iterPtr
  if toBool result
    then liftM Just (peek iterPtr)
    else return Nothing

-- Note that this function does throw an error if the pointer is NULL rather
-- than returning some random tree iterator.
peekTreeIter :: Ptr TreeIterRaw -> IO TreeIterRaw
peekTreeIter ptr
  | ptr==nullPtr = fail "peekTreeIter: ptr is NULL, tree iterator is invalid"
  | otherwise = peek ptr

-- update the stamp of a tree iter
treeIterSetStamp :: TreeIterRaw -> CInt -> TreeIterRaw
treeIterSetStamp (TreeIterRaw _ a b c) s = (TreeIterRaw s a b c)

-- | TreePath : a list of indices to specify a subtree or node in a
-- 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel'. The node that correspond
-- to a given 'TreePath' might change if nodes are removed or added and a
-- 'TreePath' may refer to a different or even non-existent node after a
-- modification of the model. In contrast, a 'TreeIter' is a more compact
-- representation of a 'TreePath' which becomes invalid after each
-- modification of the underlying model. An intelligent index that is adjusted
-- with each update of the model to point to the same node (whenever possible)
-- is 'Graphics.UI.Gtk.ModelView.TreeRowReference.TreeRowReference'.
--
treePathNewFromIndices' :: MonadIO m => [Int32] -> m TreePath
treePathNewFromIndices' [] = treePathNew
treePathNewFromIndices' x = treePathNewFromIndices x

treePathGetIndices' :: MonadIO m => TreePath -> m [Int32]
treePathGetIndices' path = treePathGetDepth path >>= \case
                                0 -> return []
                                _ -> treePathGetIndices path

withTreePath :: MonadIO m => [Int32] -> (TreePath -> m a) -> m a
withTreePath tp act = treePathNewFromIndices' tp >>= act

maybeWithTreePath :: MonadIO m => Maybe [Int32] -> (TreePath -> m a) -> m a
maybeWithTreePath mbTp act = maybe (act (TreePath nullForeignPtr)) (`withTreePath` act) mbTp

treeSelectionGetSelectedRows' :: (MonadIO m, TreeSelectionK sel) => sel -> m [TreePath]
treeSelectionGetSelectedRows' sel = treeSelectionCountSelectedRows sel >>= \case
    0 -> return []
    _ -> liftIO $ (fst <$> treeSelectionGetSelectedRows sel) `catch` (\(_::UnexpectedNullPointerReturn) -> return [])

-- | Convert a comma or colon separated string into a 'TreePath'. Any
-- non-digit characters are assumed to separate indices, thus, the function
-- always is always successful.
stringToTreePath :: Text -> [Int32]
stringToTreePath = stringToTreePath' . T.unpack
  where
  stringToTreePath' "" = []
  stringToTreePath' path = getNum 0 (dropWhile (not . isDigit) path)
  getNum acc ('0':xs) = getNum (10*acc) xs
  getNum acc ('1':xs) = getNum (10*acc+1) xs
  getNum acc ('2':xs) = getNum (10*acc+2) xs
  getNum acc ('3':xs) = getNum (10*acc+3) xs
  getNum acc ('4':xs) = getNum (10*acc+4) xs
  getNum acc ('5':xs) = getNum (10*acc+5) xs
  getNum acc ('6':xs) = getNum (10*acc+6) xs
  getNum acc ('7':xs) = getNum (10*acc+7) xs
  getNum acc ('8':xs) = getNum (10*acc+8) xs
  getNum acc ('9':xs) = getNum (10*acc+9) xs
  getNum acc xs = acc:stringToTreePath' (dropWhile (not . isDigit) xs)

-- | Accessing a row for a specific value. Used for 'ColumnMap'.
data ColumnAccess row where
  CAInvalid :: ColumnAccess row
  CAInt     :: (row -> Int32) -> ColumnAccess row
  CABool    :: (row -> Bool) -> ColumnAccess row
  CAString  :: (row -> Text) -> ColumnAccess row
  CAPixbuf  :: (row -> Pixbuf) -> ColumnAccess row

-- | The type of a tree column.
data ColumnId row ty
  = ColumnId (GValue -> IO ty) ((row -> ty) -> ColumnAccess row) Int32

-- it shouldn't matter if the following function is actually inlined
{-# NOINLINE comboQuark #-}
comboQuark :: Word32
comboQuark =
  unsafePerformIO $ quarkFromString (Just "comboBoxHaskellStringModelQuark")

