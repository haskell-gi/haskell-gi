{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 11 Feburary 2006
--
--  Copyright (C) 2005-2016 Duncan Coutts, Axel Simon, Hamish Mackenzie
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
-- Standard model to store list data.
--
module Data.GI.Gtk.ModelView.SeqStore (

-- * Types
  SeqStore(..),

-- * Constructors
  seqStoreNew,
  seqStoreNewDND,

-- * Implementation of Interfaces
  seqStoreDefaultDragSourceIface,
  seqStoreDefaultDragDestIface,

-- * Methods
  seqStoreIterToIndex,
  seqStoreGetValue,
  seqStoreSafeGetValue,
  seqStoreSetValue,
  seqStoreToList,
  seqStoreGetSize,
  seqStoreInsert,
  seqStoreInsertBefore,
  seqStoreInsertAfter,
  seqStorePrepend,
  seqStoreAppend,
  seqStoreRemove,
  seqStoreClear,
  ) where

import Prelude ()
import Prelude.Compat
import Control.Monad (when)
import Control.Monad.Trans ( liftIO )
import Data.IORef
import Data.Ix (inRange)

import Foreign.ForeignPtr (ForeignPtr)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Foldable as F
import Data.Int (Int32)

import Data.GI.Gtk.ModelView.Types
import Data.GI.Gtk.ModelView.CustomStore
       (customStoreGetStamp, customStoreGetPrivate,
        TreeModelIface(..), customStoreNew, DragDestIface(..),
        DragSourceIface(..), CustomStore(..))
import Data.GI.Base.BasicTypes
       (TypedObject(..), ManagedPtr(..), GObject)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import GI.Gtk.Interfaces.TreeModel
       (treeModelRowDeleted, treeModelRowInserted,
        treeModelRowChanged, toTreeModel, TreeModel(..), IsTreeModel(..))
import GI.GObject.Objects.Object (Object(..))
import GI.Gtk.Functions (treeGetRowDragData, treeSetRowDragData)
import GI.Gtk.Flags (TreeModelFlags(..))
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk.Structs.TreeIter
       (setTreeIterUserData3, setTreeIterUserData2, setTreeIterStamp,
        setTreeIterUserData, getTreeIterUserData, TreeIter(..))
import Data.GI.Base (get, new)
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)
import Foreign.Ptr (nullPtr)

seqStoreIterNew :: MonadIO m => Int32 -> Int32 -> m TreeIter
seqStoreIterNew s u1 = do
    i <- new TreeIter []
    setTreeIterStamp     i s
    setTreeIterUserData  i $ unsafeCoerce u1
    setTreeIterUserData2 i nullPtr
    setTreeIterUserData3 i nullPtr
    return i

newtype SeqStore a = SeqStore (ManagedPtr (CustomStore (IORef (Seq a)) a))

mkSeqStore :: CustomStore (IORef (Seq a)) a -> SeqStore a
mkSeqStore (CustomStore ptr) = SeqStore ptr

instance HasParentTypes (SeqStore a)
type instance ParentTypes (SeqStore a) = '[TreeModel]

instance TypedObject (SeqStore a) where
  glibType = glibType @TreeModel

instance GObject (SeqStore a)

instance IsTypedTreeModel SeqStore

-- | Create a new 'TreeModel' that contains a list of elements.
seqStoreNew :: (Applicative m, MonadIO m) => [a] -> m (SeqStore a)
seqStoreNew xs = seqStoreNewDND xs (Just seqStoreDefaultDragSourceIface)
                                     (Just seqStoreDefaultDragDestIface)

-- | Create a new 'TreeModel' that contains a list of elements. In addition, specify two
--   interfaces for drag and drop.
--
seqStoreNewDND :: (Applicative m, MonadIO m)
  => [a] -- ^ the initial content of the model
  -> Maybe (DragSourceIface SeqStore a) -- ^ an optional interface for drags
  -> Maybe (DragDestIface SeqStore a) -- ^ an optional interface to handle drops
  -> m (SeqStore a) -- ^ the new model
seqStoreNewDND xs mDSource mDDest = do
  rows <- liftIO $ newIORef (Seq.fromList xs)

  customStoreNew rows mkSeqStore TreeModelIface {
      treeModelIfaceGetFlags      = return [TreeModelFlagsListOnly],
      treeModelIfaceGetIter       = \path -> treePathGetIndices' path >>= \[n] -> readIORef rows >>= \rows ->
                                     if inRange (0, Seq.length rows - 1) (fromIntegral n)
                                                 then Just <$> seqStoreIterNew 0 (fromIntegral n)
                                                 else return Nothing,
      treeModelIfaceGetPath       = \i -> do
                            n <- seqStoreIterToIndex i
                            treePathNewFromIndices' [fromIntegral n],
      treeModelIfaceGetRow        = \i -> do
                            n <- seqStoreIterToIndex i
                            readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral n)
                                   then return (rows `Seq.index` fromIntegral n)
                                   else fail "SeqStore.getRow: iter does not refer to a valid entry",

      treeModelIfaceIterNext      = \i -> do
                            n <- seqStoreIterToIndex i
                            readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral (n+1))
                                   then Just <$> seqStoreIterNew 0 (n+1)
                                   else return Nothing,
      treeModelIfaceIterChildren  = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing | not (Seq.null rows) -> Just <$> seqStoreIterNew 0 0
                                             _    -> return Nothing,
      treeModelIfaceIterHasChild  = \_ -> return False,
      treeModelIfaceIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Seq.length rows
                                             _       -> return 0,
      treeModelIfaceIterNthChild  = \index n -> case index of
                                               Nothing -> Just <$> seqStoreIterNew 0 (fromIntegral n)
                                               _       -> return Nothing,
      treeModelIfaceIterParent    = \_ -> return Nothing,
      treeModelIfaceRefNode       = \_ -> return (),
      treeModelIfaceUnrefNode     = \_ -> return ()
    } mDSource mDDest


-- | Convert a 'TreeIterRaw' to an an index into the 'SeqStore'. Note that this
--   function merely extracts the second element of the 'TreeIterRaw'.
seqStoreIterToIndex :: (Applicative m, MonadIO m) => TreeIter -> m Int32
seqStoreIterToIndex i = unsafeCoerce <$> getTreeIterUserData i

-- | Default drag functions for 'Data.GI.Gtk.ModelView.SeqStore'. These
-- functions allow the rows of the model to serve as drag source. Any row is
-- allowed to be dragged and the data set in the 'SelectionDataM' object is
-- set with 'treeSetRowDragData', i.e. it contains the model and the
-- 'TreePath' to the row.
seqStoreDefaultDragSourceIface :: DragSourceIface SeqStore row
seqStoreDefaultDragSourceIface = DragSourceIface {
    customDragSourceRowDraggable = \_ _-> return True,
    customDragSourceDragDataGet = \model path sel -> treeSetRowDragData sel model path,
    customDragSourceDragDataDelete = \model path -> treePathGetIndices' path >>= \(dest:_) -> do
            liftIO $ seqStoreRemove model (fromIntegral dest)
            return True

  }

-- | Default drop functions for 'Data.GI.Gtk.ModelView.SeqStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
seqStoreDefaultDragDestIface :: DragDestIface SeqStore row
seqStoreDefaultDragDestIface = DragDestIface {
    customDragDestRowDropPossible = \model path sel -> do
      dest <- treePathGetIndices' path
      mModelPath <- treeGetRowDragData sel
      case mModelPath of
        (True, Just model', source) -> do
            tm <- toTreeModel model
            withManagedPtr tm $ \m ->
                withManagedPtr model' $ \m' -> return (m==m')
        _ -> return False,
    customDragDestDragDataReceived = \model path sel -> do
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
                row <- seqStoreGetValue model source
                seqStoreInsert model dest row
                return True
        _ -> return False
  }

-- | Extract the value at the given index.
--
seqStoreGetValue :: (Applicative m, MonadIO m) => SeqStore a -> Int32 -> m a
seqStoreGetValue (SeqStore model) index =
  (`Seq.index` fromIntegral index) <$> liftIO (readIORef (customStoreGetPrivate (CustomStore model)))

-- | Extract the value at the given index.
--
seqStoreSafeGetValue :: MonadIO m => SeqStore a -> Int32 -> m (Maybe a)
seqStoreSafeGetValue (SeqStore model) index' = do
  let index = fromIntegral index'
  seq <- liftIO $ readIORef (customStoreGetPrivate (CustomStore model))
  return $ if index >=0 && index < Seq.length seq
                then Just $ seq `Seq.index` index
                else Nothing

-- | Update the value at the given index. The index must exist.
--
seqStoreSetValue :: MonadIO m => SeqStore a -> Int32 -> a -> m ()
seqStoreSetValue (SeqStore model) index value = do
  liftIO $ modifyIORef (customStoreGetPrivate (CustomStore model)) (Seq.update (fromIntegral index) value)
  stamp <- customStoreGetStamp (CustomStore model)
  path <- treePathNewFromIndices' [index]
  i <- seqStoreIterNew stamp (fromIntegral index)
  treeModelRowChanged (CustomStore model) path i

-- | Extract all data from the store.
--
seqStoreToList :: (Applicative m, MonadIO m) => SeqStore a -> m [a]
seqStoreToList (SeqStore model) =
  F.toList <$> liftIO (readIORef (customStoreGetPrivate (CustomStore model)))

-- | Query the number of elements in the store.
seqStoreGetSize :: (Applicative m, MonadIO m) => SeqStore a -> m Int32
seqStoreGetSize (SeqStore model) =
  fromIntegral . Seq.length <$> liftIO (readIORef (customStoreGetPrivate (CustomStore model)))

-- | Insert an element in front of the given element. The element is appended
-- if the index is greater or equal to the size of the list.
seqStoreInsert :: MonadIO m => SeqStore a -> Int32 -> a -> m ()
seqStoreInsert (SeqStore model) index value = liftIO $ do
  seq <- readIORef (customStoreGetPrivate (CustomStore model))
  when (index >= 0) $ do
    let index' | fromIntegral index > Seq.length seq = Seq.length seq
               | otherwise                           = fromIntegral $ index
    writeIORef (customStoreGetPrivate (CustomStore model)) (insert index' value seq)
    stamp <- customStoreGetStamp (CustomStore model)
    p <- treePathNewFromIndices' [fromIntegral index']
    i <- seqStoreIterNew stamp (fromIntegral index')
    treeModelRowInserted (CustomStore model) p i

  where insert :: Int -> a -> Seq a -> Seq a
        insert i x xs = front Seq.>< x Seq.<| back
          where (front, back) = Seq.splitAt i xs

-- | Insert an element in front of the given element.
seqStoreInsertBefore :: (Applicative m, MonadIO m) => SeqStore a -> TreeIter -> a -> m ()
seqStoreInsertBefore store iter value = do
    n <- seqStoreIterToIndex iter
    seqStoreInsert store n value

-- | Insert an element after the given element.
seqStoreInsertAfter :: (Applicative m, MonadIO m) => SeqStore a -> TreeIter -> a -> m ()
seqStoreInsertAfter store iter value = do
    n <- seqStoreIterToIndex iter
    seqStoreInsert store (n + 1) value

-- | Prepend the element to the store.
seqStorePrepend :: (Applicative m, MonadIO m) => SeqStore a -> a -> m ()
seqStorePrepend (SeqStore model) value = do
  liftIO $ modifyIORef (customStoreGetPrivate (CustomStore model))
              (\seq -> value Seq.<| seq)
  stamp <- customStoreGetStamp (CustomStore model)
  p <- treePathNewFromIndices' [0]
  i <- seqStoreIterNew stamp 0
  treeModelRowInserted (CustomStore model) p i

---- | Prepend a list to the store. Not implemented yet.
--seqStorePrependList :: MonadIO m => SeqStore a -> [a] -> m ()
--seqStorePrependList store list =
--  mapM_ (seqStoreInsert store 0) (reverse list)

-- | Append an element to the store. Returns the index of the inserted
-- element.
seqStoreAppend :: MonadIO m => SeqStore a -> a -> m Int32
seqStoreAppend (SeqStore model) value = do
  index <- liftIO $ atomicModifyIORef (customStoreGetPrivate (CustomStore model))
                             (\seq -> (seq Seq.|> value, Seq.length seq))
  stamp <- customStoreGetStamp (CustomStore model)
  p <- treePathNewFromIndices' [fromIntegral index]
  i <- seqStoreIterNew stamp (fromIntegral index)
  treeModelRowInserted (CustomStore model) p i
  return $ fromIntegral index

{-
seqStoreAppendList :: MonadIO m => SeqStore a -> [a] -> m ()
seqStoreAppendList (SeqStore model) values = do
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
seqStoreRemove :: MonadIO m => SeqStore a -> Int32 -> m ()
seqStoreRemove (SeqStore model) index' = liftIO $ do
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
seqStoreClear :: MonadIO m => SeqStore a -> m ()
seqStoreClear (SeqStore model) = liftIO $

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
--seqStoreReorder :: MonadIO m => SeqStore a -> [Int] -> m ()
--seqStoreReorder store = undefined
--
---- | Swap two rows of the store. Not yet implemented.
--seqStoreSwap :: MonadIO m => SeqStore a -> Int -> Int -> m ()
--seqStoreSwap store = undefined
--
---- | Move the element at the first index in front of the element denoted by
---- the second index. Not yet implemented.
--seqStoreMoveBefore :: MonadIO m => SeqStore a -> Int -> Int -> m ()
--seqStoreMoveBefore store = undefined
--
---- | Move the element at the first index past the element denoted by the
---- second index. Not yet implemented.
--seqStoreMoveAfter :: MonadIO m => SeqStore a -> Int -> Int -> m ()
--seqStoreMoveAfter store = undefined

