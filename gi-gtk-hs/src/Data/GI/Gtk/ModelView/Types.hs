{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeApplications #-}

-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts
--
--  Created: 31 March 2006
--
--  Copyright (C) 2006-2016 Duncan Coutts, Axel Simon, Hamish Mackenzie
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
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Common types and classes for the ModelView modules.
--
module Data.GI.Gtk.ModelView.Types (
  TypedTreeModel(..),
  IsTypedTreeModel,
  toTypedTreeModel,
  unsafeTreeModelToGeneric,

  TypedTreeModelSort(..),
  unsafeTreeModelSortToGeneric,
  TypedTreeModelFilter(..),
  unsafeTreeModelFilterToGeneric,

  -- TreePath
  treePathNewFromIndices',
  treePathGetIndices',
  withTreePath,
  stringToTreePath,

  treeSelectionGetSelectedRows',

  -- Columns
  ColumnAccess(..),
  ColumnId(..),

  -- Storing the model in a ComboBox
  comboQuark,

  equalManagedPtr
  ) where

import Prelude ()
import Prelude.Compat
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
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Utils (with)
import Data.GI.Base.BasicTypes
       (ManagedPtr(..), ManagedPtrNewtype, UnexpectedNullPointerReturn,
        TypedObject(..), GObject)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Base.GValue (GValue)
import GI.GObject.Objects.Object (Object(..))
import GI.Gtk.Interfaces.TreeModel (TreeModel, IsTreeModel(..))
import GI.Gtk.Objects.TreeModelSort (TreeModelSort, IsTreeModelSort(..))
import GI.Gtk.Objects.TreeSelection (IsTreeSelection, treeSelectionCountSelectedRows, treeSelectionGetSelectedRows)
import GI.Gtk.Objects.TreeModelFilter (TreeModelFilter)
import GI.Gtk.Interfaces.TreeSortable (TreeSortable, IsTreeSortable(..))
import GI.GLib.Functions (quarkFromString)
import GI.GdkPixbuf.Objects.Pixbuf (Pixbuf(..))
import GI.Gtk.Structs.TreeIter
       (TreeIter(..), treeIterCopy)
import GI.Gtk.Structs.TreePath (TreePath(..), treePathGetIndices, treePathAppendIndex, treePathNew, treePathGetDepth)
import Data.GI.Base.Constructible (Constructible(..))
import Data.GI.Base.Attributes (AttrOp(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.GI.Base (set, get)
import Data.IORef (newIORef)

equalManagedPtr :: ManagedPtrNewtype a => a -> a -> Bool
equalManagedPtr a b =
    managedForeignPtr (coerce a :: ManagedPtr ()) == managedForeignPtr (coerce b :: ManagedPtr ())

newtype TypedTreeModel row = TypedTreeModel (ManagedPtr (TypedTreeModel row))

class IsTypedTreeModel model where
  dummy :: model a -> a
  dummy _ = error "not used"
  -- this is to get the right kind for model :: * -> *
  -- TODO: when haddock is fixed we can use an explicit kind annotation

toTypedTreeModel :: IsTypedTreeModel model => model row -> TypedTreeModel row
toTypedTreeModel = unsafeCoerce#

unsafeTreeModelToGeneric :: TreeModel -> model row
unsafeTreeModelToGeneric = unsafeCoerce#

instance IsTypedTreeModel TypedTreeModel

newtype TypedTreeModelSort row = TypedTreeModelSort (ManagedPtr (TypedTreeModelSort row))

instance HasParentTypes (TypedTreeModelSort row)
type instance ParentTypes (TypedTreeModelSort row) = '[TreeSortable, TreeModel, TreeModelSort]

instance TypedObject (TypedTreeModelSort row) where
  glibType = glibType @TreeModelSort

instance GObject (TypedTreeModelSort row)

unsafeTreeModelSortToGeneric :: TreeModelSort -> TypedTreeModelSort row
unsafeTreeModelSortToGeneric = unsafeCoerce#

instance IsTypedTreeModel TypedTreeModelSort

newtype TypedTreeModelFilter row = TypedTreeModelFilter (ManagedPtr (TypedTreeModelFilter row))

unsafeTreeModelFilterToGeneric :: TreeModelFilter -> TypedTreeModelFilter row
unsafeTreeModelFilterToGeneric = unsafeCoerce#

instance IsTypedTreeModel TypedTreeModelFilter

-- | TreePath is a list of indices to specify a subtree or node in a
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
treePathNewFromIndices' x = do
    path <- treePathNew
    mapM_ (treePathAppendIndex path) x
    return path
    -- TODO (once every one has Gtk+ 3.12) use treePathNewFromIndices x

treePathGetIndices' :: MonadIO m => TreePath -> m [Int32]
treePathGetIndices' path = treePathGetDepth path >>= \case
                                0 -> return []
                                _ -> do
                                  indices <- treePathGetIndices path
                                  case indices of
                                    Just ixs -> return ixs
                                    Nothing -> return []

withTreePath :: MonadIO m => [Int32] -> (TreePath -> m a) -> m a
withTreePath tp act = treePathNewFromIndices' tp >>= act

--maybeWithTreePath :: MonadIO m => Maybe [Int32] -> (TreePath -> m a) -> m a
--maybeWithTreePath mbTp act = maybe (act (TreePath nullManagedPtr)) (`withTreePath` act) mbTp

treeSelectionGetSelectedRows' :: (MonadIO m, IsTreeSelection sel) => sel -> m [TreePath]
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

