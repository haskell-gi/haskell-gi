{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- Standard model to store hierarchical data.
--
module Data.GI.Gtk.ModelView.ForestStore (

-- * Types
  ForestStore(..),

-- * Constructors
  forestStoreNew,
  forestStoreNewDND,

-- * Implementation of Interfaces
  forestStoreDefaultDragSourceIface,
  forestStoreDefaultDragDestIface,

-- * Methods
  forestStoreGetValue,
  forestStoreGetTree,
  forestStoreGetForest,
  forestStoreLookup,

  forestStoreSetValue,

  forestStoreInsert,
  forestStoreInsertTree,
  forestStoreInsertForest,

  forestStoreRemove,
  forestStoreClear,

  forestStoreChange,
  forestStoreChangeM,
  ) where

import Prelude ()
import Prelude.Compat
import Data.Bits
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Maybe ( fromMaybe, isJust )
import Data.Tree
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (assert)
import Data.IORef

import Foreign.ForeignPtr (ForeignPtr)

import Data.GI.Base.BasicTypes
       (TypedObject(..), ManagedPtr(..), GObject)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)

import Data.GI.Gtk.ModelView.Types
import Data.GI.Gtk.ModelView.CustomStore
       (customStoreGetStamp, customStoreGetPrivate,
        TreeModelIface(..), customStoreNew, DragDestIface(..),
        DragSourceIface(..), CustomStore(..), customStoreInvalidateIters)
import GI.GObject.Objects.Object (Object(..))
import GI.Gtk.Interfaces.TreeModel
       (treeModelRowDeleted, treeModelRowInserted,
        treeModelRowChanged, toTreeModel, TreeModel(..), IsTreeModel(..),
        treeModelRowHasChildToggled)
import GI.Gtk.Functions (treeSetRowDragData, treeGetRowDragData)
import GI.Gtk.Structs.TreePath
       (TreePath)
import GI.Gtk.Structs.TreeIter
       (getTreeIterUserData3, getTreeIterUserData2, getTreeIterUserData,
        getTreeIterStamp, setTreeIterUserData3, setTreeIterUserData2,
        setTreeIterUserData, setTreeIterStamp, TreeIter(..))
import Data.GI.Base (get, new)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------
-- internal model data types
--

data ForestStoreIter = ForestStoreIter Int32 Word32 Word32 Word32

fromForestStoreIter :: MonadIO m => ForestStoreIter -> m TreeIter
fromForestStoreIter (ForestStoreIter s u1 u2 u3) = do
    i <- new TreeIter []
    setTreeIterStamp     i s
    setTreeIterUserData  i $ unsafeCoerce u1
    setTreeIterUserData2 i $ unsafeCoerce u2
    setTreeIterUserData3 i $ unsafeCoerce u3
    return i

toForestStoreIter :: MonadIO m => TreeIter -> m ForestStoreIter
toForestStoreIter iter = do
    stamp <- getTreeIterStamp iter
    u1    <- getTreeIterUserData iter
    u2    <- getTreeIterUserData2 iter
    u3    <- getTreeIterUserData3 iter
    return $ ForestStoreIter stamp (unsafeCoerce u1) (unsafeCoerce u2) (unsafeCoerce u3)

forestStoreIterSetStamp :: ForestStoreIter -> Int32 -> ForestStoreIter
forestStoreIterSetStamp (ForestStoreIter _ a b c) s = ForestStoreIter s a b c


-- | A store for hierarchical data.
--
newtype ForestStore a = ForestStore (ManagedPtr (CustomStore (IORef (Store a)) a))

mkForestStore :: CustomStore (IORef (Store a)) a -> ForestStore a
mkForestStore (CustomStore ptr) = ForestStore ptr

instance HasParentTypes (ForestStore a)
type instance ParentTypes (ForestStore a) = '[TreeModel]

instance TypedObject (ForestStore a) where
  glibType = glibType @TreeModel

instance GObject (ForestStore a)

instance IsTypedTreeModel ForestStore

-- | Maximum number of nodes on each level.
--
-- * These numbers determine how many bits in a 'TreeIter' are devoted to
--   each level. Hence, these numbers reflect log2 of the maximum number
--   of nodes at a level, rounded up.
--
type Depth = [Int]

data Store a = Store {
  depth :: Depth,
  content :: Cache a
}

-- | Create a new list store.
--
-- * The given rose tree determines the initial content and may be the empty
--   list. Each 'Tree' in the forest corresponds to one top-level node.
--
-- * The ForestStore maintains the initially given Forest and aligns the 'TreePath'
--   bits to fit in 96-bit length 'TreeIter' storage.
--
-- * Additionally, a cache is used to achieve higher performance if operating on
--   recently used TreePaths.
--
-- * __Note:__ due to the limited amount of bits available in TreeIter storage, only
--   limited depth forests can be used with this implementation, the result of too deep
--   Forests is an undefined behaviour while trying to retrieve the deeply nested nodes.
--   For example: assuming the average requiement is 8 bits per tree level (max number of
--   children at the level is 255), then we can only use 12 levels deep trees (96/8) -
--   any further levels in a TreePath will not be encoded in the corresponding TreeIter
--   storage.
--
forestStoreNew :: MonadIO m => Forest a -> m (ForestStore a)
forestStoreNew forest = forestStoreNewDND forest
                        (Just forestStoreDefaultDragSourceIface)
                        (Just forestStoreDefaultDragDestIface)

-- | Create a new list store.
--
-- * In addition to 'forestStoreNew', this function takes an two interfaces
--   to implement user-defined drag-and-drop functionality.
--
forestStoreNewDND :: MonadIO m => Forest a -- ^ the inital tree stored in this model
  -> Maybe (DragSourceIface ForestStore a) -- ^ an optional interface for drags
  -> Maybe (DragDestIface ForestStore a) -- ^ an optional interface to handle drops
  -> m (ForestStore a)
forestStoreNewDND forest mDSource mDDest = liftIO $ do
  (storeRef :: IORef (Store a)) <- newIORef Store {
      depth = calcForestDepth forest,
      content = storeToCache forest
    }
  let withStore :: (Store a -> IO result) -> IO result
      withStore f = readIORef storeRef >>= f
      withStoreUpdateCache :: (Store a -> (result, Cache a)) -> IO result
      withStoreUpdateCache f = do
        store <- readIORef storeRef
        let (result, cache') = f store
        writeIORef storeRef store { content = cache' }
        return result

  customStoreNew storeRef mkForestStore TreeModelIface {
    treeModelIfaceGetFlags = return [],

    treeModelIfaceGetIter = \path -> withStore $
      \Store { depth = d } -> fromPath d <$> treePathGetIndices' path >>= mapM fromForestStoreIter,

    treeModelIfaceGetPath = toForestStoreIter >=> \iter -> withStore $
      \Store { depth = d } -> treePathNewFromIndices' $ toPath d iter,

    treeModelIfaceGetRow  = toForestStoreIter >=> \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
        case checkSuccess d iter cache of
          (True, cache'@((_, (Node { rootLabel = val }:_)):_)) ->
            (val, cache')
          _ -> error "ForestStore.getRow: iter does not refer to a valid entry",

    treeModelIfaceIterNext = toForestStoreIter >=> \iter -> withStoreUpdateCache (
      \Store { depth = d, content = cache } -> iterNext d iter cache) >>= mapM fromForestStoreIter,

    treeModelIfaceIterChildren = \mIter -> do
        iter <- maybe (return invalidIter) toForestStoreIter mIter
        withStoreUpdateCache (
          \Store { depth = d, content = cache } ->
            iterNthChild d 0 iter cache) >>= mapM fromForestStoreIter,

    treeModelIfaceIterHasChild = toForestStoreIter >=> \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
       let (mIter, cache') = iterNthChild d 0 iter cache
        in (isJust mIter, cache'),

    treeModelIfaceIterNChildren = mapM toForestStoreIter >=> \mIter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNChildren d iter cache,

    treeModelIfaceIterNthChild = \mIter idx  -> do
        iter <- maybe (return invalidIter) toForestStoreIter mIter
        withStoreUpdateCache (
          \Store { depth = d, content = cache } ->
            iterNthChild d idx iter cache) >>= mapM fromForestStoreIter,

    treeModelIfaceIterParent = toForestStoreIter >=> \iter -> withStore $
      \Store { depth = d } -> mapM fromForestStoreIter (iterParent d iter),

    treeModelIfaceRefNode = \_ -> return (),
    treeModelIfaceUnrefNode = \_ -> return ()
   } mDSource mDDest


-- | Default drag functions for
-- 'Data.GI.Gtk.ModelView.ForestStore'. These functions allow the rows of
-- the model to serve as drag source. Any row is allowed to be dragged and the
-- data set in the 'SelectionDataM' object is set with 'treeSetRowDragData',
-- i.e. it contains the model and the 'TreePath' to the row.
forestStoreDefaultDragSourceIface :: DragSourceIface ForestStore row
forestStoreDefaultDragSourceIface = DragSourceIface {
    customDragSourceRowDraggable = \_ _-> return True,
    customDragSourceDragDataGet = \model path sel -> treeSetRowDragData sel model path,
    customDragSourceDragDataDelete = \model path -> treePathGetIndices' path >>= \dest@(_:_) -> do
            liftIO $ forestStoreRemove model path
            return True

  }

-- | Default drop functions for 'Data.GI.Gtk.ModelView.ForestStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
forestStoreDefaultDragDestIface :: DragDestIface ForestStore row
forestStoreDefaultDragDestIface = DragDestIface {
    customDragDestRowDropPossible = \model path sel -> do
      mModelPath <- treeGetRowDragData sel
      case mModelPath of
        (True, Just model', source) -> do
            tm <- toTreeModel model
            withManagedPtr tm $ \m ->
                withManagedPtr model' $ \m' -> return (m==m')
        _ -> return False,
    customDragDestDragDataReceived = \model path sel -> do
      dest@(_:_) <- treePathGetIndices' path
      mModelPath <- treeGetRowDragData sel
      case mModelPath of
        (True, Just model', Just path) -> do
          source@(_:_) <- treePathGetIndices' path
          tm <- toTreeModel model
          withManagedPtr tm $ \m ->
            withManagedPtr model' $ \m' ->
              if m/=m' then return False
              else do
                row <- forestStoreGetTree model =<< treePathNewFromIndices' source
                initPath <- treePathNewFromIndices' (init dest)
                forestStoreInsertTree model initPath (fromIntegral $ last dest) row
                return True
        _ -> return False
  }

--------------------------------------------
-- low level bit-twiddling utility functions
--

bitsNeeded :: Word32 -> Int
bitsNeeded n = bitsNeeded' 0 n
  where bitsNeeded' b 0 = b
        bitsNeeded' b n = bitsNeeded' (b+1) (n `shiftR` 1)

getBitSlice :: ForestStoreIter -> Int -> Int -> Word32
getBitSlice (ForestStoreIter _ a b c) off count =
      getBitSliceWord a  off     count
  .|. getBitSliceWord b (off-32) count
  .|. getBitSliceWord c (off-64) count

  where getBitSliceWord :: Word32 -> Int -> Int -> Word32
        getBitSliceWord word off count =
          word `shift` (-off) .&. (1 `shiftL` count - 1)

setBitSlice :: ForestStoreIter -> Int -> Int -> Word32 -> ForestStoreIter
setBitSlice (ForestStoreIter stamp a b c) off count value =
  assert (value < 1 `shiftL` count) $
  ForestStoreIter stamp
                (setBitSliceWord a  off     count value)
                (setBitSliceWord b (off-32) count value)
                (setBitSliceWord c (off-64) count value)

  where setBitSliceWord :: Word32 -> Int -> Int -> Word32 -> Word32
        setBitSliceWord word off count value =
          let mask = (1 `shiftL` count - 1) `shift` off
           in (word .&. complement mask) .|. (value `shift` off)


--iterPrefixEqual :: TreeIter -> TreeIter -> Int -> Bool
--iterPrefixEqual (TreeIter _ a1 b1 c1) (TreeIter _ a2 b2 c2) pos
--  | pos>64 = let mask = 1 `shiftL` (pos-64) - 1 in
--             a1==a2 && b1==b2 && (c1 .&. mask) == (c2 .&. mask)
--  | pos>32 = let mask = 1 `shiftL` (pos-32) - 1 in
--             a1==a2 && (b1 .&. mask) == (b2 .&. mask)
--  | otherwise = let mask = 1 `shiftL` pos - 1 in
--                (a1 .&. mask) == (a2 .&. mask)

-- | The invalid tree iterator.
--
invalidIter :: ForestStoreIter
invalidIter = ForestStoreIter 0 0 0 0

--showIterBits (TreeIter _ a b c) = [showBits a, showBits b, showBits c]
--
--showBits :: Bits a => a -> String
--showBits a = [ if testBit a i then '1' else '0' | i <- [0..bitSize a - 1] ]

-- | Calculate the maximum number of nodes on a per-level basis.
--
calcForestDepth :: Forest a -> Depth
calcForestDepth f = map bitsNeeded $
                    takeWhile (/=0) $
                    foldr calcTreeDepth (repeat 0) f
  where
  calcTreeDepth Node { subForest = f } (d:ds) =
      (d+1): zipWith max ds (foldr calcTreeDepth (repeat 0) f)


-- | Convert an iterator into a path.
--
toPath :: Depth -> ForestStoreIter -> [Int32]
toPath d iter = gP 0 d
  where
  gP pos [] = []
  gP pos (d:ds) = let idx = getBitSlice iter pos d in
                  if idx==0 then [] else fromIntegral (idx-1) : gP (pos+d) ds

-- | Try to convert a path into a 'TreeIter'.
--
fromPath :: Depth -> [Int32] -> Maybe ForestStoreIter
fromPath = fP 0 invalidIter
  where
  fP pos ti _ [] = Just ti -- the remaining bits are zero anyway
  fP pos ti [] _ = Nothing
  fP pos ti (d:ds) (p:ps) = let idx = fromIntegral (p+1) in
    if idx >= bit d then Nothing else
    fP (pos+d) (setBitSlice ti pos d idx) ds ps


-- | The 'Cache' type synonym is only used iternally. What it represents
--   the stack during a (fictional) lookup operations.
--   The topmost frame is the node
--   for which this lookup was started and the innermost frame (the last
--   element of the list) contains the root of the tree.
--
type Cache a = [(ForestStoreIter, Forest a)]


-- | Create a traversal structure that allows a pre-order traversal in linear
--   time.
--
-- * The returned structure points at the root of the first level which doesn't
--   really exist, but serves to indicate that it is before the very first
--   node.
--
storeToCache :: Forest a -> Cache a
storeToCache [] = []
storeToCache forest = [(invalidIter, [Node root forest])]
  where
  root = error "ForestStore.storeToCache: accessed non-exitent root of tree"

-- | Extract the store from the cache data structure.
cacheToStore :: Cache a -> Forest a
cacheToStore [] = []
cacheToStore cache = case last cache of (_, [Node _ forest]) -> forest

-- | Advance the traversal structure to the given 'TreeIter'.
--
advanceCache :: Depth -> ForestStoreIter -> Cache a -> Cache a
advanceCache depth goal [] = []
advanceCache depth goal cache@((rootIter,_):_) =
  moveToSameLevel 0 depth
  where
  moveToSameLevel pos [] = cache
  moveToSameLevel pos (d:ds) =
    let
      goalIdx = getBitSlice goal pos d
      curIdx = getBitSlice rootIter pos d
      isNonZero pos d (ti,_) = getBitSlice ti pos d/=0
    in
    if goalIdx==curIdx then moveToSameLevel (pos+d) ds else
    if goalIdx==0 then dropWhile (isNonZero pos d) cache else
    if curIdx==0 then moveToChild pos (d:ds) cache else
    if goalIdx<curIdx then
      moveToChild pos (d:ds) (dropWhile (isNonZero pos d) cache)
    else let
      -- advance the current iterator to coincide with the goal iterator
      -- at this level
      moveWithinLevel pos d ((ti,forest):parents) = let
          diff = fromIntegral (goalIdx-curIdx)
          (dropped, remain) = splitAt diff forest
          advance = length dropped
          ti' = setBitSlice ti pos d (curIdx+fromIntegral advance)
        in
        if advance==diff then moveToChild (pos+d) ds ((ti',remain):parents)
        else (ti',remain):parents -- node not found
    in moveWithinLevel pos d $ case ds of
        [] -> cache
        (d':_) -> dropWhile (isNonZero (pos+d) d') cache

  -- Descend into the topmost forest to find the goal iterator. The position
  -- and the remainding depths specify the index in the cache that is zero.
  -- All indices in front of pos coincide with that of the goal iterator.
  moveToChild :: Int -> Depth -> Cache a -> Cache a
  moveToChild pos [] cache = cache -- we can't set more than the leaf
  moveToChild pos (d:ds) cache@((ti,forest):parents)
    | getBitSlice goal pos d == 0 = cache
    | otherwise = case forest of
      [] -> cache -- impossible request
      Node { subForest = children }:_ ->
        let
          childIdx :: Int
          childIdx = fromIntegral (getBitSlice goal pos d)-1
          (dropped, remain) = splitAt childIdx children
          advanced = length dropped
          ti' = setBitSlice ti pos d (fromIntegral advanced+1)
        in if advanced<childIdx then ((ti',remain):cache) else
           moveToChild (pos+d) ds ((ti',remain):cache)

-- | Advance to the given iterator and return weather this was successful.
--
checkSuccess :: Depth -> ForestStoreIter -> Cache a -> (Bool, Cache a)
checkSuccess depth iter cache = case advanceCache depth iter cache of
    cache'@((cur,sibs):_) -> (cmp cur iter && not (null sibs), cache')
    [] -> (False, [])
  where
  cmp (ForestStoreIter _ a1 b1 c1) (ForestStoreIter _ a2 b2 c2) =
      a1==a2 && b1==b2 && c2==c2

-- | Get the leaf index of this iterator.
--
-- * Due to the way we construct the 'TreeIter's, we can check which the last
--   level of an iterator is: The bit sequence of level n is zero if n is
--   greater or equal to the level that the iterator refers to. The returned
--   triple is (pos, leaf, zero) such that pos..pos+leaf denotes the leaf
--   index and pos+leaf..pos+leaf+zero denotes the bit field that is zero.
--
getTreeIterLeaf :: Depth -> ForestStoreIter -> (Int, Int, Int)
getTreeIterLeaf ds ti = gTIL 0 0 ds
  where
  gTIL pos dCur (dNext:ds)
    | getBitSlice ti (pos+dCur) dNext==0 = (pos,dCur,dNext)
    | otherwise = gTIL (pos+dCur) dNext ds
  gTIL pos d [] = (pos, d, 0)

-- | Move an iterator forwards on the same level.
--
iterNext :: Depth -> ForestStoreIter -> Cache a -> (Maybe ForestStoreIter, Cache a)
iterNext depth iter cache = let
    (pos,leaf,_child) = getTreeIterLeaf depth iter
    curIdx = getBitSlice iter pos leaf
    nextIdx = curIdx+1
    nextIter = setBitSlice iter pos leaf nextIdx
  in
  if nextIdx==bit leaf then (Nothing, cache) else
  case checkSuccess depth nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Move down to the child of the given iterator.
--
iterNthChild :: Depth -> Int -> ForestStoreIter -> Cache a  ->
                (Maybe ForestStoreIter, Cache a)
iterNthChild depth childIdx_ iter cache = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
    childIdx = fromIntegral childIdx_+1
    nextIter = setBitSlice iter (pos+leaf) child childIdx
  in
  if childIdx>=bit child then (Nothing, cache) else
  case checkSuccess depth nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Descend to the first child.
--
iterNChildren :: Depth -> ForestStoreIter -> Cache a -> (Int, Cache a)
iterNChildren depth iter cache = case checkSuccess depth iter cache of
  (True, cache@((_,Node { subForest = forest}:_):_)) -> (length forest, cache)
  (_, cache) -> (0, cache)


-- | Ascend to parent.
--
iterParent :: Depth -> ForestStoreIter -> Maybe ForestStoreIter
iterParent depth iter = let
    (pos,leaf,_child) = getTreeIterLeaf depth iter
  in if pos==0 then Nothing else
     if getBitSlice iter pos leaf==0 then Nothing else
     Just (setBitSlice iter pos leaf 0)

-- | Insert nodes into the store.
--
-- * The given list of nodes is inserted into given parent at @pos@.
--   If the parent existed, the function returns @Just path@ where @path@
--   is the position of the newly inserted elements. If @pos@ is negative
--   or greater or equal to the number of children of the node at @path@,
--   the new nodes are appended to the list.
--
forestStoreInsertForest :: MonadIO m
 => ForestStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> Forest a    -- ^ the list of trees to be inserted
 -> m ()
forestStoreInsertForest (ForestStore model) path pos nodes = liftIO $ do
  ipath <- treePathGetIndices' path
  customStoreInvalidateIters $ CustomStore model
  (idx, toggle) <- atomicModifyIORef (customStoreGetPrivate $ CustomStore model) $
    \store@Store { depth = d, content = cache } ->
    case insertIntoForest (cacheToStore cache) nodes ipath pos of
      Nothing -> error ("forestStoreInsertForest: path does not exist " ++ show ipath)
      Just (newForest, idx, toggle) ->
       let depth = calcForestDepth newForest
        in (Store { depth = depth,
                    content = storeToCache newForest },
           (idx, toggle))
  Store { depth = depth } <- readIORef (customStoreGetPrivate $ CustomStore model)
  let rpath = reverse ipath
  stamp <- customStoreGetStamp $ CustomStore model
  sequence_ [ let p' = reverse p
                  Just iter = fromPath depth p'
               in do
                  p'' <- treePathNewFromIndices' p'
                  treeModelRowInserted (CustomStore model) p'' =<< fromForestStoreIter (forestStoreIterSetStamp iter stamp)
            | (i, node) <- zip [idx..] nodes
            , p <- paths (fromIntegral i : rpath) node ]
  let Just iter = fromPath depth ipath
  when toggle $ treeModelRowHasChildToggled (CustomStore model) path
                =<< fromForestStoreIter (forestStoreIterSetStamp iter stamp)

  where paths :: [Int32] -> Tree a -> [[Int32]]
        paths path Node { subForest = ts } =
          path : concat [ paths (n:path) t | (n, t) <- zip [0..] ts ]

-- | Insert a node into the store.
--
forestStoreInsertTree :: MonadIO m
 => ForestStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> Tree a      -- ^ the value to be inserted
 -> m ()
forestStoreInsertTree store path pos node =
  forestStoreInsertForest store path pos [node]

-- | Insert a single node into the store.
--
-- * This function inserts a single node without children into the tree.
--   Its arguments are similar to those of 'forestStoreInsert'.
--
forestStoreInsert :: MonadIO m
 => ForestStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> a           -- ^ the value to be inserted
 -> m ()
forestStoreInsert store path pos node =
  forestStoreInsertForest store path pos [Node node []]

-- | Insert nodes into a forest.
--
-- * If the parent was found, returns the new tree, the child number
--   and a flag denoting if these new nodes were the first children
--   of the parent.
--
insertIntoForest :: Forest a -> Forest a -> [Int32] -> Int ->
                    Maybe (Forest a, Int, Bool)
insertIntoForest forest nodes [] pos
  | pos<0 = Just (forest++nodes, length forest, null forest)
  | otherwise = Just (prev++nodes++next, length prev, null forest)
    where (prev, next) = splitAt pos forest
insertIntoForest forest nodes (p:ps) pos = case splitAt (fromIntegral p) forest of
  (prev, []) -> Nothing
  (prev, Node { rootLabel = val,
                subForest = for}:next) ->
    case insertIntoForest for nodes ps pos of
      Nothing -> Nothing
      Just (for, pos, toggle) -> Just (prev++Node { rootLabel = val,
                                                    subForest = for }:next,
                                       pos, toggle)

-- | Remove a node from the store.
--
-- * The node denoted by the path is removed, along with all its children.
--   The function returns @True@ if the given node was found.
--
forestStoreRemove :: MonadIO m => ForestStore a -> TreePath -> m Bool
forestStoreRemove model path = treePathGetIndices' path >>= forestStoreRemoveImpl model path

forestStoreRemoveImpl :: MonadIO m => ForestStore a -> TreePath -> [Int32] -> m Bool
  --TODO: eliminate this special case without segfaulting!
forestStoreRemoveImpl (ForestStore model) _ [] = return False
forestStoreRemoveImpl (ForestStore model) path ipath = liftIO $ do
  customStoreInvalidateIters (CustomStore model)
  (found, toggle) <- atomicModifyIORef (customStoreGetPrivate (CustomStore model)) $
    \store@Store { depth = d, content = cache } ->
    if null cache then (store, (False, False)) else
    case deleteFromForest (cacheToStore cache) ipath of
      Nothing -> (store, (False, False))
      Just (newForest, toggle) ->
        (Store { depth = d, -- this might be a space leak
                 content = storeToCache newForest }, (True, toggle))
  when found $ do
    when (toggle && not (null ipath)) $ do
      Store { depth = depth } <- readIORef (customStoreGetPrivate (CustomStore model))
      let iparent = init ipath
          Just iter = fromPath depth iparent
      parent <- treePathNewFromIndices' iparent
      treeModelRowHasChildToggled (CustomStore model) parent =<< fromForestStoreIter iter
    treeModelRowDeleted (CustomStore model) path
  return found

forestStoreClear :: MonadIO m => ForestStore a -> m ()
forestStoreClear (ForestStore model) = liftIO $ do
  customStoreInvalidateIters (CustomStore model)
  Store { content = cache } <- readIORef (customStoreGetPrivate (CustomStore model))
  let forest = cacheToStore cache
  writeIORef (customStoreGetPrivate (CustomStore model)) Store {
      depth = calcForestDepth [],
      content = storeToCache []
    }
  let loop (-1) = return ()
      loop   n  = treePathNewFromIndices' [fromIntegral n] >>= treeModelRowDeleted (CustomStore model) >> loop (n-1)
  loop (length forest - 1)

-- | Remove a node from a rose tree.
--
-- * Returns the new tree if the node was found. The returned flag is
--   @True@ if deleting the node left the parent without any children.
--
deleteFromForest :: Forest a -> [Int32] -> Maybe (Forest a, Bool)
deleteFromForest forest [] = Just ([], False)
deleteFromForest forest (p:ps) =
  case splitAt (fromIntegral p) forest of
    (prev, kill@Node { rootLabel = val,
                       subForest = for}:next) ->
      if null ps then Just (prev++next, null prev && null next) else
      case deleteFromForest for ps of
        Nothing -> Nothing
        Just (for,toggle) -> Just (prev++Node {rootLabel = val,
                                               subForest = for }:next, toggle)
    (prev, []) -> Nothing


-- | Set a node in the store.
--
forestStoreSetValue :: MonadIO m => ForestStore a -> TreePath -> a -> m ()
forestStoreSetValue store path value = forestStoreChangeM store path (\_ -> return value)
                                  >> return ()


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a monadic version, see
--   'forestStoreChangeM'.
--
forestStoreChange :: MonadIO m => ForestStore a -> TreePath -> (a -> a) -> m Bool
forestStoreChange store path func = forestStoreChangeM store path (return . func)


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a purely functional version, see
--   'forestStoreChange'.
--
forestStoreChangeM :: MonadIO m => ForestStore a -> TreePath -> (a -> m a) -> m Bool
forestStoreChangeM (ForestStore model) path act = do
  ipath <- treePathGetIndices' path
  customStoreInvalidateIters (CustomStore model)
  store@Store { depth = d, content = cache } <-
      liftIO $ readIORef (customStoreGetPrivate (CustomStore model))
  (store'@Store { depth = d, content = cache }, found) <- do
    mRes <- changeForest (cacheToStore cache) act ipath
    return $ case mRes of
      Nothing -> (store, False)
      Just newForest -> (Store { depth = d,
                                 content = storeToCache newForest }, True)
  liftIO $ writeIORef (customStoreGetPrivate (CustomStore model)) store'
  let Just iter = fromPath d ipath
  stamp <- customStoreGetStamp (CustomStore model)
  when found $ treeModelRowChanged (CustomStore model) path =<< fromForestStoreIter (forestStoreIterSetStamp iter stamp)
  return found

-- | Change a node in the forest.
--
-- * Returns @True@ if the given node was found.
--
changeForest :: MonadIO m => Forest a -> (a -> m a) -> [Int32] -> m (Maybe (Forest a))
changeForest forest act [] = return Nothing
changeForest forest act (p:ps) = case splitAt (fromIntegral p) forest of
  (prev, []) -> return Nothing
  (prev, Node { rootLabel = val,
                subForest = for}:next) ->
    if null ps then do
      val' <- act val
      return (Just (prev++Node { rootLabel = val',
                                 subForest = for }:next))
    else do
      mFor <- changeForest for act ps
      case mFor of
        Nothing -> return Nothing
        Just for -> return $ Just (prev++Node { rootLabel = val,
                                                subForest = for }:next)

-- | Extract one node from the current model. Fails if the given
--   'TreePath' refers to a non-existent node.
--
forestStoreGetValue :: (Applicative m, MonadIO m) => ForestStore a -> TreePath -> m a
forestStoreGetValue model path = rootLabel <$> forestStoreGetTree model path

-- | Extract a subtree from the current model. Fails if the given
--   'TreePath' refers to a non-existent node.
--
forestStoreGetTree :: MonadIO m => ForestStore a -> TreePath -> m (Tree a)
forestStoreGetTree (ForestStore model) path = liftIO $ do
  ipath <- treePathGetIndices' path
  store@Store { depth = d, content = cache } <-
      readIORef (customStoreGetPrivate (CustomStore model))
  case fromPath d ipath of
    (Just iter) -> do
      let (res, cache') = checkSuccess d iter cache
      writeIORef (customStoreGetPrivate (CustomStore model)) store { content = cache' }
      case cache' of
        ((_,node:_):_) | res -> return node
        _ -> fail ("forestStoreGetTree: path does not exist " ++ show ipath)
    _ -> fail ("forestStoreGetTree: path does not exist " ++ show ipath)

-- | Extract the forest from the current model.
--
forestStoreGetForest :: MonadIO m => ForestStore a -> m (Forest a)
forestStoreGetForest (ForestStore model) = liftIO $ do
  store@Store { depth = d, content = cache } <-
      readIORef (customStoreGetPrivate (CustomStore model))
  return $ cacheToStore cache

-- | Extract a subtree from the current model. Like 'forestStoreGetTree'
--   but returns @Nothing@ if the path refers to a non-existant node.
--
forestStoreLookup :: MonadIO m => ForestStore a -> TreePath -> m (Maybe (Tree a))
forestStoreLookup (ForestStore model) path = liftIO $ do
  ipath <- treePathGetIndices' path
  store@Store { depth = d, content = cache } <-
      readIORef (customStoreGetPrivate (CustomStore model))
  case fromPath d ipath of
    (Just iter) -> do
      let (res, cache') = checkSuccess d iter cache
      writeIORef (customStoreGetPrivate (CustomStore model)) store { content = cache' }
      case cache' of
        ((_,node:_):_) | res -> return (Just node)
        _ -> return Nothing
    _ -> return Nothing
