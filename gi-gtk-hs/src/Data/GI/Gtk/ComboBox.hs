-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBox
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Copyright (C) 2004-2016 Duncan Coutts, Axel Simon, Hamish Mackenzie
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

{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A widget used to choose from a list of items.
--
-- * Module available since Gtk+ version 2.4
--
module Data.GI.Gtk.ComboBox (

-- * Detail
--
-- | A 'ComboBox' is a widget that allows the user to choose from a list of
-- valid choices. The 'ComboBox' displays the selected choice. When activated,
-- the 'ComboBox' displays a popup which allows the user to make a new choice.
-- The style in which the selected value is displayed, and the style of the
-- popup is determined by the current theme. It may be similar to a
-- 'OptionMenu', or similar to a Windows-style combo box.
--
-- Unlike its predecessors 'Combo' and 'OptionMenu', the 'ComboBox' uses the
-- model-view pattern; the list of valid choices is specified in the form of a
-- tree model, and the display of the choices can be adapted to the data in
-- the model by using cell renderers, as you would in a tree view. This is
-- possible since 'ComboBox' implements the 'CellLayout' interface. The tree
-- model holding the valid choices is not restricted to a flat list, it can be
-- a real tree, and the popup will reflect the tree structure.
--
-- In addition to the general model-view API, 'ComboBox' offers the function
-- 'comboBoxNewText' which creates a text-only combo box.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----ComboBox
-- |                                 +----'ComboBoxEntry'
-- @

  module GI.Gtk.Objects.ComboBox,

-- ** Simple Text API
  comboBoxNewText,
  comboBoxSetModelText,
  comboBoxGetModelText,
  comboBoxAppendText,
  comboBoxInsertText,
  comboBoxPrependText,
  comboBoxRemoveText,
  comboBoxGetActiveText,

  ) where

import Prelude ()
import Prelude.Compat
import Control.Monad    (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.StablePtr (newStablePtr, castStablePtrToPtr, deRefStablePtr, castPtrToStablePtr)
import Data.Text (Text)
import Data.Word (Word32)
import Data.Int (Int32)
import Data.GI.Base.BasicTypes (GObject)
import Data.GI.Base.ManagedPtr (unsafeManagedPtrCastPtr, touchManagedPtr, unsafeCastTo)
import Data.GI.Gtk.ModelView.Types (comboQuark)
import Data.GI.Gtk.ModelView.TreeModel (makeColumnIdString)
import Data.GI.Gtk.ModelView.CustomStore (customStoreSetColumn, customStoreGetRow)
import Data.GI.Gtk.ModelView.SeqStore ( SeqStore(..), seqStoreNew,
  seqStoreInsert, seqStorePrepend, seqStoreAppend, seqStoreRemove,
  seqStoreSafeGetValue )
import GI.Gtk.Objects.ComboBox
import Data.GI.Gtk.ModelView.CellLayout (CellLayout(..), cellLayoutClear, cellLayoutPackStart, cellLayoutSetDataFunction, cellLayoutGetCells)
import GI.Gtk.Objects.CellRendererText (CellRendererText(..), cellRendererTextNew, setCellRendererTextText)
import GI.GObject.Objects.Object (Object, toObject)

type GQuark = Word32

-- | The address of a function freeing a 'StablePtr'. See 'destroyFunPtr'.
foreign import ccall unsafe "&hs_free_stable_ptr" destroyStablePtr :: FunPtr(Ptr () -> IO ())

foreign import ccall "g_object_set_qdata" g_object_set_qdata ::
    Ptr Object -> GQuark -> Ptr () -> IO ()

foreign import ccall "g_object_set_qdata_full" g_object_set_qdata_full ::
    Ptr Object -> GQuark -> Ptr () -> FunPtr(Ptr () -> IO ()) -> IO ()

-- | Set the value of an association.
--
objectSetAttribute :: (MonadIO m, GObject o) => o -> GQuark -> Maybe a -> m ()
objectSetAttribute obj attr Nothing = liftIO $ do
  obj' <- unsafeManagedPtrCastPtr obj
  g_object_set_qdata obj' (fromIntegral attr) nullPtr
  touchManagedPtr obj
objectSetAttribute obj attr (Just val) = liftIO $ do
  sPtr <- newStablePtr val
  obj' <- unsafeManagedPtrCastPtr obj
  g_object_set_qdata_full obj' attr (castStablePtrToPtr sPtr) destroyStablePtr
  touchManagedPtr obj

foreign import ccall "g_object_get_qdata" g_object_get_qdata ::
    Ptr Object -> GQuark -> IO (Ptr ())

-- | Get the value of an association.
--
-- * Note that this function may crash the Haskell run-time since the
--   returned type can be forced to be anything. See 'objectCreateAttribute'
--   for a safe wrapper around this funciton.
--
objectGetAttributeUnsafe :: (MonadIO m, GObject o) => o -> GQuark -> m (Maybe a)
objectGetAttributeUnsafe obj attr = liftIO $ do
  obj' <- unsafeManagedPtrCastPtr obj
  sPtr <- g_object_get_qdata obj' attr
  touchManagedPtr obj
  if sPtr==nullPtr then return Nothing else
    liftM Just $! deRefStablePtr (castPtrToStablePtr sPtr)

--------------------
-- Constructors

-- | Convenience function which constructs a new text combo box that is a
-- 'ComboBox' just displaying strings. This function internally calls
-- 'comboBoxSetModelText' after creating a new combo box.
--
comboBoxNewText :: MonadIO m => m ComboBox
comboBoxNewText = do
  combo <- comboBoxNew
  comboBoxSetModelText combo
  return combo

--------------------
-- Methods

-- the text API

-- | Create a combo box that holds strings.
--
-- This function stores a 'Data.GI.Gtk.ModelView.SeqStore' with the
-- widget and sets the model to the list store. The widget can contain only
-- strings. The model can be retrieved with 'comboBoxGetModel'. The list
-- store can be retrieved with 'comboBoxGetModelText'.
-- Any exisiting model or renderers are removed before setting the new text
-- model.
-- Note that the functions 'comboBoxAppendText', 'comboBoxInsertText',
-- 'comboBoxPrependText', 'comboBoxRemoveText' and 'comboBoxGetActiveText'
-- can be called on a combo box only once 'comboBoxSetModelText' is called.
--
comboBoxSetModelText :: (MonadIO m, IsComboBox self) => self -> m (SeqStore Text)
comboBoxSetModelText combo = liftIO $ do
  layout <- unsafeCastTo CellLayout combo
  cellLayoutClear layout
  store <- seqStoreNew ([] :: [Text])
  comboBoxSetModel combo (Just store)
  let colId = makeColumnIdString 0
  customStoreSetColumn store colId id
  comboBoxSetEntryTextColumn combo 0
  ren <- cellRendererTextNew
  cellLayoutPackStart layout ren True
  cellLayoutSetDataFunction layout ren store (setCellRendererTextText ren)
  objectSetAttribute combo comboQuark (Just store)
  return store

-- | Retrieve the model that was created with 'comboBoxSetModelText'.
--
comboBoxGetModelText :: (MonadIO m, IsComboBox self) => self -> m (SeqStore Text)
comboBoxGetModelText self = do
  maybeStore <- objectGetAttributeUnsafe self comboQuark
  case maybeStore of
    Just store -> return store
    Nothing -> error "Could not get required attribute"

-- | Appends @string@ to the list of strings stored in @comboBox@. Note that
-- you can only use this function with combo boxes constructed with
-- 'comboBoxNewText'. Returns the index of the appended text.
--
comboBoxAppendText :: (MonadIO m, IsComboBox self) => self -> Text -> m Int32
comboBoxAppendText self text = do
  store <- comboBoxGetModelText self
  seqStoreAppend store text

-- %hash c:41de d:8ab0
-- | Inserts @string@ at @position@ in the list of strings stored in
-- @comboBox@. Note that you can only use this function with combo boxes
-- constructed with 'comboBoxNewText'.
--
comboBoxInsertText :: (MonadIO m, IsComboBox self) => self
 -> Int32  -- ^ @position@ - An index to insert @text@.
 -> Text   -- ^ @text@ - A string.
 -> m ()
comboBoxInsertText self position text = do
  store <- comboBoxGetModelText self
  seqStoreInsert store position text

-- | Prepends @string@ to the list of strings stored in @comboBox@. Note that
-- you can only use this function with combo boxes constructed with
-- 'comboBoxNewText'.
--
comboBoxPrependText :: (Applicative m, MonadIO m, IsComboBox self) => self -> Text -> m ()
comboBoxPrependText self text = do
  store <- comboBoxGetModelText self
  seqStorePrepend store text

-- | Removes the string at @position@ from @comboBox@. Note that you can only
-- use this function with combo boxes constructed with 'comboBoxNewText'.
--
comboBoxRemoveText :: (MonadIO m, IsComboBox self) => self
 -> Int32 -- ^ @position@ - Index of the item to remove.
 -> m ()
comboBoxRemoveText self position = do
  store <- comboBoxGetModelText self
  seqStoreRemove store position

-- | Returns the currently active string in @comboBox@ or @Nothing@ if none is
-- selected. Note that you can only use this function with combo boxes
-- constructed with 'comboBoxNewText'.
--
comboBoxGetActiveText :: (MonadIO m, IsComboBox self) => self -> m (Maybe Text)
comboBoxGetActiveText self = do
  activeId <- comboBoxGetActive self
  if activeId < 0
    then return Nothing
    else do
      seqStore <- comboBoxGetModelText self
      seqStoreSafeGetValue seqStore (fromIntegral activeId)

