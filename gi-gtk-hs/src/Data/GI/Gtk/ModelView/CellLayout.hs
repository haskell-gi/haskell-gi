{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface CellLayout
--
--  Author : Axel Simon
--
--  Created: 23 January 2006
--
--  Copyright (C) 2016-2016 Axel Simon, Hamish Mackenzie
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
-- |
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- An interface for packing cells
--
-- * Module available since Gtk+ version 2.4
--
module Data.GI.Gtk.ModelView.CellLayout (
-- * Detail
--
-- | 'CellLayout' is an interface which is implemented by all objects which
-- provide a 'TreeViewColumn' API for packing cells, setting attributes and data funcs.

-- * Class Hierarchy
-- |
-- @
-- |  Interface CellLayout
-- |   +----'TreeViewColumn'
-- |   +----'CellView'
-- |   +----'IconView'
-- |   +----'EntryCompletion'
-- |   +----'ComboBox'
-- |   +----'ComboBoxEntry'
-- @

    module GI.Gtk.Interfaces.CellLayout
--  , cellLayoutAddColumnAttribute
  , cellLayoutSetAttributes
  , cellLayoutSetDataFunction
  , cellLayoutSetDataFunc'
  , convertIterFromParentToChildModel
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import Data.GI.Base.Attributes (AttrOp, AttrOpTag(..), set)
import Data.GI.Base.ManagedPtr (castTo, withManagedPtr)
import GI.Gtk.Interfaces.CellLayout
import GI.Gtk.Objects.TreeModelFilter (TreeModelFilter(..), getTreeModelFilterChildModel, treeModelFilterConvertIterToChildIter)
import GI.Gtk.Objects.TreeModelSort (TreeModelSort(..), getTreeModelSortModel, treeModelSortConvertIterToChildIter)
import GI.Gtk.Structs.TreeIter
       (getTreeIterStamp, getTreeIterUserData3, getTreeIterUserData2,
        getTreeIterUserData, TreeIter(..))
import GI.Gtk.Objects.CellRenderer (IsCellRenderer, CellRenderer(..), toCellRenderer)
import Data.GI.Gtk.ModelView.Types
import Data.GI.Gtk.ModelView.TreeModel
import Data.GI.Gtk.ModelView.CustomStore (customStoreGetRow)
import Data.GI.Base (get)
import Data.GI.Base.BasicTypes (ManagedPtr(..))

--------------------
-- Methods

-- | Adds an attribute mapping to the renderer @cell@. The @column@ is
-- the 'ColumnId' of the model to get a value from, and the @attribute@ is the
-- parameter on @cell@ to be set from the value. So for example if column 2 of
-- the model contains strings, you could have the \"text\" attribute of a
-- 'CellRendererText' get its values from column 2.
--
-- cellLayoutAddColumnAttribute :: (MonadIO m, IsCellLayout self, IsCellRenderer cell) => self
--  -> cell   -- ^ @cell@ - A 'CellRenderer'.
--  -> ReadWriteAttr cell a v  -- ^ @attribute@ - An attribute of a renderer.
--  -> ColumnId row v    -- ^ @column@ - The virtual column of the model from which to
--                       -- retrieve the attribute.
--  -> m ()
-- cellLayoutAddColumnAttribute self cell attr column =
--   cellLayoutAddAttribute self cell (T.pack $ show attr) (columnIdToNumber column)

-- | Specify how a row of the @model@ defines the
-- attributes of the 'CellRenderer' @cell@. This is a convenience wrapper
-- around 'cellLayoutSetAttributeFunc' in that it sets the cells of the @cell@
-- with the data retrieved from the model.
--
-- * Note on using 'Data.GI.Gtk.ModelView.TreeModelSort.TreeModelSort' and
-- 'Data.GI.Gtk.ModelView.TreeModelFilter.TreeModelFilter': These two models
-- wrap another model, the so-called child model, instead of storing their own
-- data. This raises the problem that the data of cell renderers must be set
-- using the child model, while the 'TreeIter's that the view works with refer to
-- the model that encapsulates the child model. For convenience, this function
-- transparently translates an iterator to the child model before extracting the
-- data using e.g. 'Data.GI.Gtk.TreeModel.TreeModelSort.treeModelSortConvertIterToChildIter'.
-- Hence, it is possible to install the encapsulating model in the view and to
-- pass the child model to this function.
--
cellLayoutSetAttributes :: (MonadIO m,
                            IsCellLayout self,
                            IsCellRenderer cell,
                            IsTreeModel (model row),
                            IsTypedTreeModel model)
 => self
 -> cell   -- ^ @cell@ - A 'CellRenderer'.
 -> model row -- ^ @model@ - A model containing rows of type @row@.
 -> (row -> [AttrOp cell 'AttrSet]) -- ^ Function to set attributes on the cell renderer.
 -> m ()
cellLayoutSetAttributes self cell model attributes =
  cellLayoutSetDataFunc' self cell model $ \iter -> do
    row <- customStoreGetRow model iter
    set cell (attributes row)

-- | Like 'cellLayoutSetAttributes', but allows any IO action to be used
cellLayoutSetDataFunction :: (MonadIO m,
                            IsCellLayout self,
                            IsCellRenderer cell,
                            IsTreeModel (model row),
                            IsTypedTreeModel model)
 => self
 -> cell   -- ^ @cell@ - A 'CellRenderer'.
 -> model row -- ^ @model@ - A model containing rows of type @row@.
 -> (row -> IO ()) -- ^ Function to set data on the cell renderer.
 -> m ()
cellLayoutSetDataFunction self cell model callback =
  cellLayoutSetDataFunc' self cell model $ \iter -> do
    row <- customStoreGetRow model iter
    callback row

-- | Install a function that looks up a row in the model and sets the
-- attributes of the 'CellRenderer' @cell@ using the row's content.
--
cellLayoutSetDataFunc' :: (MonadIO m,
                               IsCellLayout self,
                               IsCellRenderer cell,
                               IsTreeModel model)
 => self
 -> cell   -- ^ @cell@ - A 'CellRenderer'.
 -> model  -- ^ @model@ - A model from which to draw data.
 -> (TreeIter -> IO ()) -- ^ Function to set attributes on the cell renderer.
 -> m ()
cellLayoutSetDataFunc' self cell model func = liftIO $ do
  cellLayoutSetCellDataFunc self cell . Just $ \_ (CellRenderer cellPtr') model' iter -> do
    castModel <- toTreeModel model
    iter <- convertIterFromParentToChildModel iter model' castModel
    CellRenderer cellPtr <- toCellRenderer cell
    if managedForeignPtr cellPtr /= managedForeignPtr cellPtr' then
      error ("cellLayoutSetAttributeFunc: attempt to set attributes of "++
             "a different CellRenderer.")
      else func iter

-- Given a 'TreeModelFilter' or a 'TreeModelSort' and a 'TreeIter', get the
-- child model of these models and convert the iter to an iter of the child
-- model. This is an ugly internal function that is needed for some widgets
-- which pass iterators to the callback function of set_cell_data_func that
-- refer to some internal TreeModelFilter models that they create around the
-- user model. This is a bug but since C programs mostly use the columns
-- rather than the cell_layout way to extract attributes, this bug does not
-- show up in many programs. Reported in the case of EntryCompletion as bug
-- \#551202.
--
convertIterFromParentToChildModel ::
     TreeIter -- ^ the iterator
  -> TreeModel -- ^ the model that we got from the all back
  -> TreeModel -- ^ the model that we actually want
  -> IO TreeIter
convertIterFromParentToChildModel iter parentModel@(TreeModel parentModelPtr) childModel =
  let (TreeModel modelPtr) = childModel in
  if managedForeignPtr modelPtr == managedForeignPtr parentModelPtr
    then return iter
    else
        castTo TreeModelFilter parentModel >>= \case
            Just tmFilter -> do
                childIter <- treeModelFilterConvertIterToChildIter tmFilter iter
                Just child@(TreeModel childPtr) <- getTreeModelFilterChildModel tmFilter
                if managedForeignPtr childPtr == managedForeignPtr modelPtr
                    then return childIter
                    else convertIterFromParentToChildModel childIter child childModel
            Nothing -> do
                castTo TreeModelSort parentModel >>= \case
                    Just tmSort -> do
                        childIter <- treeModelSortConvertIterToChildIter tmSort iter
                        child@(TreeModel childPtr) <- getTreeModelSortModel tmSort
                        if managedForeignPtr childPtr == managedForeignPtr modelPtr
                            then return childIter
                            else convertIterFromParentToChildModel childIter child childModel
                    Nothing -> do
                        stamp <- getTreeIterStamp iter
                        ud1 <- getTreeIterUserData iter
                        ud2 <- getTreeIterUserData2 iter
                        ud3 <- getTreeIterUserData3 iter
                        error ("CellLayout: don't know how to convert iter "++show (stamp, ud1, ud2, ud3)++
                               " from model "++show (managedForeignPtr parentModelPtr)++" to model "++
                               show (managedForeignPtr modelPtr)++". Is it possible that you are setting the "++
                               "attributes of a CellRenderer using a different model than "++
                               "that which was set in the view?")

