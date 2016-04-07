{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 11 Feburary 2006
--
--  Copyright (C) 2005 Duncan Coutts, Axel Simon
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
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Standard model to store list data.
--
module Graphics.UI.Gtk.ModelView.ListStore (

-- * Types
  ListStore(..),

-- * Constructors
  listStoreNew,
  listStoreNewDND,

-- * Implementation of Interfaces
  listStoreDefaultDragSourceIface,
  listStoreDefaultDragDestIface,

-- * Methods
  listStoreIterToIndex,
  listStoreGetValue,
  listStoreSafeGetValue,
  listStoreSetValue,
  listStoreToList,
  listStoreGetSize,
  listStoreInsert,
  listStorePrepend,
  listStoreAppend,
  listStoreRemove,
  listStoreClear,
  ) where

import Control.Monad (when)
import Control.Monad.Trans ( liftIO )
import Data.IORef
import Data.Ix (inRange)

import Foreign.ForeignPtr (ForeignPtr)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Foldable as F
import Data.Int (Int32)

import Graphics.UI.Gtk.ModelView.Types
import Graphics.UI.Gtk.ModelView.CustomStore
       (customStoreGetStamp, customStoreGetPrivate,
        TreeModelIface(..), customStoreNew, DragDestIface(..),
        DragSourceIface(..), CustomStore(..))
import Data.GI.Base.BasicTypes (GObject(..), GObject)
import Data.GI.Base.Overloading (ParentTypes)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import GI.Gtk.Interfaces.TreeModel
       (treeModelRowDeleted, treeModelRowInserted,
        treeModelRowChanged, toTreeModel, TreeModel(..))
import GI.GObject.Objects.Object (Object(..))
import GI.Gtk.Functions (treeGetRowDragData, treeSetRowDragData)
import GI.Gtk.Flags (TreeModelFlags(..))
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk.Structs.TreeIter (TreeIter(..))

newtype ListStore a = ListStore (ForeignPtr (CustomStore (IORef (Seq a)) a))

mkListStore :: CustomStore (IORef (Seq a)) a -> ListStore a
mkListStore (CustomStore ptr) = ListStore ptr

type instance ParentTypes (ListStore a) = ListStoreParentTypes
type ListStoreParentTypes = '[TreeModel, Object]

instance GObject (ListStore a) where
    gobjectIsInitiallyUnowned _ = False
    gobjectType _ = gobjectType (undefined :: TreeModel)

instance TypedTreeModelClass ListStore

-- | Create a new 'TreeModel' that contains a list of elements.
listStoreNew :: MonadIO m => [a] -> m (ListStore a)
listStoreNew xs = listStoreNewDND xs (Just listStoreDefaultDragSourceIface)
                                     (Just listStoreDefaultDragDestIface)

-- | Create a new 'TreeModel' that contains a list of elements. In addition, specify two
--   interfaces for drag and drop.
--
listStoreNewDND :: MonadIO m
  => [a] -- ^ the initial content of the model
  -> Maybe (DragSourceIface ListStore a) -- ^ an optional interface for drags
  -> Maybe (DragDestIface ListStore a) -- ^ an optional interface to handle drops
  -> m (ListStore a) -- ^ the new model
listStoreNewDND xs mDSource mDDest = do
  rows <- liftIO $ newIORef (Seq.fromList xs)

  customStoreNew rows mkListStore TreeModelIface {
      treeModelIfaceGetFlags      = return [TreeModelFlagsListOnly],
      treeModelIfaceGetIter       = \path -> treePathGetIndices' path >>= \[n] -> readIORef rows >>= \rows ->
                                     return (if Seq.null rows then Nothing else
                                             Just (TreeIterRaw 0 (fromIntegral n) 0 0)),
      treeModelIfaceGetPath       = \(TreeIterRaw _ n _ _) -> treePathNewFromIndices' [fromIntegral n],
      treeModelIfaceGetRow        = \(TreeIterRaw _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral n)
                                   then return (rows `Seq.index` fromIntegral n)
                                   else fail "ListStore.getRow: iter does not refer to a valid entry",

      treeModelIfaceIterNext      = \(TreeIterRaw _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral (n+1))
                                   then return (Just (TreeIterRaw 0 (n+1) 0 0))
                                   else return Nothing,
      treeModelIfaceIterChildren  = \_ -> return Nothing,
      treeModelIfaceIterHasChild  = \_ -> return False,
      treeModelIfaceIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Seq.length rows
                                             _       -> return 0,
      treeModelIfaceIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIterRaw 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      treeModelIfaceIterParent    = \_ -> return Nothing,
      treeModelIfaceRefNode       = \_ -> return (),
      treeModelIfaceUnrefNode     = \_ -> return ()
    } mDSource mDDest


-- | Convert a 'TreeIterRaw' to an an index into the 'ListStore'. Note that this
--   function merely extracts the second element of the 'TreeIterRaw'.
listStoreIterToIndex :: MonadIO m => TreeIter -> m Int32
listStoreIterToIndex i = (\(TreeIterRaw _ n _ _) -> fromIntegral n) <$> treeIterToRaw i

-- | Default drag functions for 'Graphics.UI.Gtk.ModelView.ListStore'. These
-- functions allow the rows of the model to serve as drag source. Any row is
-- allowed to be dragged and the data set in the 'SelectionDataM' object is
-- set with 'treeSetRowDragData', i.e. it contains the model and the
-- 'TreePath' to the row.
listStoreDefaultDragSourceIface :: DragSourceIface ListStore row
listStoreDefaultDragSourceIface = DragSourceIface {
    treeDragSourceRowDraggable = \_ _-> return True,
    treeDragSourceDragDataGet = \model path sel -> treeSetRowDragData sel model path,
    treeDragSourceDragDataDelete = \model path -> treePathGetIndices' path >>= \(dest:_) -> do
            liftIO $ listStoreRemove model (fromIntegral dest)
            return True

  }

-- | Default drop functions for 'Graphics.UI.Gtk.ModelView.ListStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
listStoreDefaultDragDestIface :: DragDestIface ListStore row
listStoreDefaultDragDestIface = DragDestIface {
    treeDragDestRowDropPossible = \model path sel -> do
      dest <- treePathGetIndices' path
      mModelPath <- treeGetRowDragData sel
      case mModelPath of
        (True, Just model', source) -> do
            tm <- toTreeModel model
            withManagedPtr tm $ \m ->
                withManagedPtr model' $ \m' -> return (m==m')
        _ -> return False,
    treeDragDestDragDataReceived = \model path sel -> do
      (dest:_) <- treePathGetIndices' path
      mModelPath <- treeGetRowDragData sel
      case mModelPath of
        (True, Just model', Just path) -> do
          (source:_) <- treePathGetIndices' path
          tm <- toTreeModel model
          withManagedPtr tm $ \m ->
            withManagedPtr model' $ \m' ->
              if m/=m' then return False
              else do
                row <- listStoreGetValue model source
                listStoreInsert model dest row
                return True
        _ -> return False
  }

-- | Extract the value at the given index.
--
listStoreGetValue :: MonadIO m => ListStore a -> Int32 -> m a
listStoreGetValue (ListStore model) index =
  (`Seq.index` fromIntegral index) <$> liftIO (readIORef (customStoreGetPrivate (CustomStore model)))

-- | Extract the value at the given index.
--
listStoreSafeGetValue :: MonadIO m => ListStore a -> Int32 -> m (Maybe a)
listStoreSafeGetValue (ListStore model) index' = do
  let index = fromIntegral index'
  seq <- liftIO $ readIORef (customStoreGetPrivate (CustomStore model))
  return $ if index >=0 && index < Seq.length seq
                then Just $ seq `Seq.index` index
                else Nothing

-- | Update the value at the given index. The index must exist.
--
listStoreSetValue :: MonadIO m => ListStore a -> Int32 -> a -> m ()
listStoreSetValue (ListStore model) index value = do
  liftIO $ modifyIORef (customStoreGetPrivate (CustomStore model)) (Seq.update (fromIntegral index) value)
  stamp <- customStoreGetStamp (CustomStore model)
  path <- treePathNewFromIndices' [index]
  i <- treeIterNew stamp (fromIntegral index) 0 0
  treeModelRowChanged (CustomStore model) path i

-- | Extract all data from the store.
--
listStoreToList :: MonadIO m => ListStore a -> m [a]
listStoreToList (ListStore model) =
  F.toList <$> liftIO (readIORef (customStoreGetPrivate (CustomStore model)))

-- | Query the number of elements in the store.
listStoreGetSize :: MonadIO m => ListStore a -> m Int32
listStoreGetSize (ListStore model) =
  fromIntegral . Seq.length <$> liftIO (readIORef (customStoreGetPrivate (CustomStore model)))

-- | Insert an element in front of the given element. The element is appended
-- if the index is greater or equal to the size of the list.
listStoreInsert :: MonadIO m => ListStore a -> Int32 -> a -> m ()
listStoreInsert (ListStore model) index value = liftIO $ do
  seq <- readIORef (customStoreGetPrivate (CustomStore model))
  when (index >= 0) $ do
    let index' | fromIntegral index > Seq.length seq = Seq.length seq
               | otherwise                           = fromIntegral $ index
    writeIORef (customStoreGetPrivate (CustomStore model)) (insert index' value seq)
    stamp <- customStoreGetStamp (CustomStore model)
    p <- treePathNewFromIndices' [fromIntegral index']
    i <- treeIterNew stamp (fromIntegral index') 0 0
    treeModelRowInserted (CustomStore model) p i

  where insert :: Int -> a -> Seq a -> Seq a
        insert i x xs = front Seq.>< x Seq.<| back
          where (front, back) = Seq.splitAt i xs

-- | Prepend the element to the store.
listStorePrepend :: MonadIO m => ListStore a -> a -> m ()
listStorePrepend (ListStore model) value = do
  liftIO $ modifyIORef (customStoreGetPrivate (CustomStore model))
              (\seq -> value Seq.<| seq)
  stamp <- customStoreGetStamp (CustomStore model)
  p <- treePathNewFromIndices' [0]
  i <- treeIterNew stamp 0 0 0
  treeModelRowInserted (CustomStore model) p i

---- | Prepend a list to the store. Not implemented yet.
--listStorePrependList :: MonadIO m => ListStore a -> [a] -> m ()
--listStorePrependList store list =
--  mapM_ (listStoreInsert store 0) (reverse list)

-- | Append an element to the store. Returns the index of the inserted
-- element.
listStoreAppend :: MonadIO m => ListStore a -> a -> m Int32
listStoreAppend (ListStore model) value = do
  index <- liftIO $ atomicModifyIORef (customStoreGetPrivate (CustomStore model))
                             (\seq -> (seq Seq.|> value, Seq.length seq))
  stamp <- customStoreGetStamp (CustomStore model)
  p <- treePathNewFromIndices' [fromIntegral index]
  i <- treeIterNew stamp (fromIntegral index) 0 0
  treeModelRowInserted (CustomStore model) p i
  return $ fromIntegral index

{-
listStoreAppendList :: MonadIO m => ListStore a -> [a] -> m ()
listStoreAppendList (ListStore model) values = do
  seq <- readIORef (customStoreGetPrivate model)
  let seq' = Seq.fromList values
      startIndex = Seq.length seq
      endIndex = startIndex + Seq.length seq' - 1
  writeIORef (customStoreGetPrivate model) (seq Seq.>< seq')
  stamp <- customStoreGetStamp model
  flip mapM [startIndex..endIndex] $ \index ->
    treeModelRowInserted model [index] (TreeIterRaw stamp (fromIntegral index) 0 0)
-}

-- | Remove the element at the given index.
--
listStoreRemove :: MonadIO m => ListStore a -> Int32 -> m ()
listStoreRemove (ListStore model) index' = liftIO $ do
  seq <- readIORef (customStoreGetPrivate (CustomStore model))
  when (index >=0 && index < Seq.length seq) $ do
    writeIORef (customStoreGetPrivate (CustomStore model)) (delete index seq)
    p <- treePathNewFromIndices' [fromIntegral index]
    treeModelRowDeleted (CustomStore model) p
  where delete :: Int -> Seq a -> Seq a
        delete i xs = front Seq.>< Seq.drop 1 back
          where (front, back) = Seq.splitAt i xs
        index = fromIntegral index'

-- | Empty the store.
listStoreClear :: MonadIO m => ListStore a -> m ()
listStoreClear (ListStore model) = liftIO $

  -- Since deleting rows can cause callbacks (eg due to selection changes)
  -- we have to make sure the model is consitent with the view at each
  -- intermediate step of clearing the store. Otherwise at some intermediate
  -- stage when the view has only been informed about some delections, the
  -- user might query the model expecting to find the remaining rows are there
  -- but find them deleted. That'd be bad.
  --
  let loop (-1) Seq.EmptyR = return ()
      loop n (seq Seq.:> _) = do
        writeIORef (customStoreGetPrivate (CustomStore model)) seq
        p <- treePathNewFromIndices' [fromIntegral n]
        treeModelRowDeleted (CustomStore model) p
        loop (n-1) (Seq.viewr seq)

   in do seq <- readIORef (customStoreGetPrivate (CustomStore model))
         loop (Seq.length seq - 1) (Seq.viewr seq)

---- | Permute the rows of the store. Not yet implemented.
--listStoreReorder :: MonadIO m => ListStore a -> [Int] -> m ()
--listStoreReorder store = undefined
--
---- | Swap two rows of the store. Not yet implemented.
--listStoreSwap :: MonadIO m => ListStore a -> Int -> Int -> m ()
--listStoreSwap store = undefined
--
---- | Move the element at the first index in front of the element denoted by
---- the second index. Not yet implemented.
--listStoreMoveBefore :: MonadIO m => ListStore a -> Int -> Int -> m ()
--listStoreMoveBefore store = undefined
--
---- | Move the element at the first index past the element denoted by the
---- second index. Not yet implemented.
--listStoreMoveAfter :: MonadIO m => ListStore a -> Int -> Int -> m ()
--listStoreMoveAfter store = undefined

