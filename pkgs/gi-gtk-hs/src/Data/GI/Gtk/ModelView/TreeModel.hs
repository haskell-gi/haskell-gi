-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeModel
--
--  Author : Axel Simon
--
--  Created: 8 May 2001
--
--  Copyright (C) 1999-2016 Axel Simon, Hamish Mackenzie
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

--
-- |
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- The tree interface used by 'TreeView'.
--
module Data.GI.Gtk.ModelView.TreeModel (
-- * Detail
--
-- | The 'TreeModel' interface defines a generic storage object for use by the
-- 'TreeView' and similar widgets. Specifically, the functions in defined here
-- are used by Gtk's widgets to access the stored data. Thus, rather than
-- calling these functions, an application programmer has to implement them.
-- While the module "Data.GI.Gtk.ModelView.CustomStore" provides the
-- necessary functions to implement the 'TreeMode' interface, it is often
-- sufficient to use the wo implementations that come with gi-gtk-hs, namely are
-- 'ListStore' and 'TreeStore'.
--
-- The model is represented as a hierarchical tree of values. It is important
-- to note that this interface only provides a way of examining a model and
-- observing changes. The implementation of each individual model decides how
-- and if changes are made.
--
-- Two generic models are provided that implement the 'TreeModel' interface:
-- the 'TreeStore' and the 'ListStore'. To use these, the developer simply
-- inserts data into these models as necessary. These models provide the data
-- structure as well as the 'TreeModel' interface. In fact, they implement
-- other interfaces, making drag and drop and storing data trivial.
--
-- A 'TreeModel' stores records of the same type. Each record is referred to
-- as row, just like in a relational database. Defining how the information of
-- a row is displayed can be done in two ways: If the widget displays data
-- using 'Data.GI.Gtk.ModelView.CellRenderer.CellRenderer' or one of its
-- derivatives, it is possible to state how a row is mapped to the attributes
-- of a renderer using the
-- 'Data.GI.Gtk.ModelView.CellLayout.cellLayoutSetAttributes' function.
-- Some widgets do not use
-- 'Data.GI.Gtk.ModelView.CellRenderer.CellRenderer's to display their
-- data. In this case an extraction function can be defined that maps a row to
-- one of a few basic types (like 'String's or 'Int's). This extraction
-- function is associated with a 'ColumnId' using
-- 'Data.GI.Gtk.ModelView.CustomStore.treeModelSetColumn'. The latter can
-- be set in the widget for the property that should be set. The widget then
-- uses the function 'treeModelGetValue' and the 'ColumnId' to extract the
-- value from the model. As the name suggests, using 'ColumnId's creates a
-- view of the data as if each row were divided into a well-defined set of
-- columns, again, like a relational database.
--
-- Models are accessed on a node level of granularity. There are two index
-- types used to reference a particular node in a model. They are the
-- 'TreePath' and the 'TreeIter'. Most of the interface consists of operations
-- on a 'TreeIter'.
--
-- A path is essentially a potential node. It is a location on a model that
-- may or may not actually correspond to a node on a specific model. A
-- 'TreePath' is in fact a synonym for a list of 'Int's and hence are easy to
-- manipulate. Each number refers to the offset at that level. Thus, the path
-- @[0]@ refers to the root node and the path @[2,4]@ refers to the fifth
-- child of the third node.
--
-- By contrast, a 'TreeIter' is a reference to a specific node on a specific
-- model. It is an abstract data type filled in by the model. One can convert
-- a path to an iterator by calling 'treeModelGetIter'. These iterators are
-- the primary way of accessing a model and are similar to the iterators used
-- by 'TextBuffer'. The model interface defines a set of operations using them
-- for navigating the model. Iterators are expected to always be valid for as
-- long as the model is unchanged (and doesn't emit a signal).
--

-- * Class Hierarchy
-- |
-- @
-- |  GInterface
-- |   +----TreeModel
-- |   +--------TypedTreeModel
-- @

  module Export,

  ColumnId,

-- * Constructors
  makeColumnIdInt,
  makeColumnIdBool,
  makeColumnIdString,
  makeColumnIdPixbuf,
  invalidColumnId,

-- * Methods
  columnIdToNumber,
  stringToTreePath,
  treeModelGetValue,
  treeModelGetIter
  ) where

import Prelude ()
import Prelude.Compat
import Data.Int (Int32)
import Data.Text (Text)
import Data.GI.Base.GValue (GValue(..), fromGValue, get_object)
import Data.GI.Base.ManagedPtr (withManagedPtr, newObject)
import Foreign.Ptr (Ptr)
import GI.GdkPixbuf.Objects.Pixbuf (Pixbuf(..))
import GI.Gtk.Structs.TreeIter (TreeIter)
import GI.Gtk.Interfaces.TreeModel as Export hiding (treeModelGetValue, treeModelGetIter)
import qualified GI.Gtk.Interfaces.TreeModel as GI (treeModelGetValue, treeModelGetIter)
import Data.GI.Gtk.ModelView.Types (stringToTreePath,
                                        ColumnId(..),
                                        ColumnAccess(..))
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk.Structs.TreePath (treePathGetDepth, TreePath(..))
import Data.Maybe (fromJust)

--------------------
-- Constructors


-- | Create a 'ColumnId' to extract an integer.
makeColumnIdInt :: Int32 -> ColumnId row Int32
makeColumnIdInt = ColumnId fromGValue CAInt

-- | Create a 'ColumnId' to extract an Boolean.
makeColumnIdBool :: Int32 -> ColumnId row Bool
makeColumnIdBool = ColumnId fromGValue CABool

-- | Create a 'ColumnId' to extract an string.
makeColumnIdString :: Int32 -> ColumnId row Text
makeColumnIdString = ColumnId (\v -> fromJust <$> fromGValue v) CAString

-- | Create a 'ColumnId' to extract an 'Pixbuf'.
makeColumnIdPixbuf :: Int32 -> ColumnId row Pixbuf
makeColumnIdPixbuf = ColumnId gvalueToPixbuf CAPixbuf
  where gvalueToPixbuf :: GValue -> IO Pixbuf
        gvalueToPixbuf gv = withManagedPtr gv $ \gvPtr -> do
          objPtr <- get_object gvPtr :: IO (Ptr Pixbuf)
          newObject Pixbuf objPtr

-- | Convert a 'ColumnId' to a bare number.
columnIdToNumber :: ColumnId row ty -> Int32
columnIdToNumber (ColumnId _ _ i) = i

-- | The invalid 'ColumnId'. Widgets use this value if no column id has
--   been set.
invalidColumnId :: ColumnId row ty
invalidColumnId = ColumnId (error "invalidColumnId: no GValue extractor")
  (error "invalidColumnId: no access type") (-1)

instance Eq (ColumnId row ty) where
  (ColumnId _ _ i1) == (ColumnId _ _ i2) = i1==i2

instance Show (ColumnId row ty) where
  show (ColumnId _ _ i) = show i


--------------------
-- Methods

-- | Read the value of at a specific column and 'TreeIter'.
--
treeModelGetValue :: IsTreeModel self => self
 -> TreeIter
 -> ColumnId row ty         -- ^ @column@ - The column to lookup the value at.
 -> IO ty
treeModelGetValue self iter (ColumnId getter _ colId) =
  GI.treeModelGetValue self iter colId >>= getter

-- | Gets the a `TreeIter` or Nothing if the path is invalid or empty
treeModelGetIter :: (MonadIO m, IsTreeModel model) => model -> TreePath -> m (Maybe TreeIter)
treeModelGetIter model path =
    treePathGetDepth path >>= \case
        0 -> return Nothing
        _ -> GI.treeModelGetIter model path >>= \case
            (True, iter) -> return $ Just iter
            _            -> return Nothing
