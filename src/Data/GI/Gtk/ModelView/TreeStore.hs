{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- Standard model to store hierarchical data.
--
module Graphics.UI.Gtk.ModelView.TreeStore (

-- * Types
  TreeStore(..),

-- * Constructors
  treeStoreNew,
  treeStoreNewDND,

-- * Implementation of Interfaces
  treeStoreDefaultDragSourceIface,
  treeStoreDefaultDragDestIface,

-- * Methods
  treeStoreGetValue,
  treeStoreGetTree,
  treeStoreLookup,

  treeStoreSetValue,

  treeStoreInsert,
  treeStoreInsertTree,
  treeStoreInsertForest,

  treeStoreRemove,
  treeStoreClear,

  treeStoreChange,
  treeStoreChangeM,
  ) where

import Data.Bits
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Maybe ( fromMaybe, isJust )
import Data.Tree
import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (assert)
import Data.IORef

import Foreign.ForeignPtr (ForeignPtr)

import Data.GI.Base.BasicTypes (GObject(..), GObject)
import Data.GI.Base.Overloading (ParentTypes)
import Data.GI.Base.ManagedPtr (withManagedPtr)

import Graphics.UI.Gtk.ModelView.Types
import Graphics.UI.Gtk.ModelView.CustomStore
       (customStoreGetStamp, customStoreGetPrivate,
        TreeModelIface(..), customStoreNew, DragDestIface(..),
        DragSourceIface(..), CustomStore(..), customStoreInvalidateIters)
import GI.GObject.Objects.Object (Object(..))
import GI.Gtk.Interfaces.TreeModel
       (treeModelRowDeleted, treeModelRowInserted,
        treeModelRowChanged, toTreeModel, TreeModel(..),
        treeModelRowHasChildToggled)
import GI.Gtk.Functions (treeSetRowDragData, treeGetRowDragData)
import GI.Gtk.Structs.TreePath
       (TreePath)

--------------------------------------------
-- internal model data types
--

-- | A store for hierarchical data.
--
newtype TreeStore a = TreeStore (ForeignPtr (CustomStore (IORef (Store a)) a))

mkTreeStore :: CustomStore (IORef (Store a)) a -> TreeStore a
mkTreeStore (CustomStore ptr) = TreeStore ptr

type instance ParentTypes (TreeStore a) = TreeStoreParentTypes
type TreeStoreParentTypes = '[TreeModel, Object]

instance GObject (TreeStore a) where
    gobjectIsInitiallyUnowned _ = False
    gobjectType _ = gobjectType (undefined :: TreeModel)

instance TypedTreeModelClass TreeStore

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
-- * The TreeStore maintains the initially given Forest and aligns the 'TreePath'
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
treeStoreNew :: MonadIO m => Forest a -> m (TreeStore a)
treeStoreNew forest = treeStoreNewDND forest
                        (Just treeStoreDefaultDragSourceIface)
                        (Just treeStoreDefaultDragDestIface)

-- | Create a new list store.
--
-- * In addition to 'treeStoreNew', this function takes an two interfaces
--   to implement user-defined drag-and-drop functionality.
--
treeStoreNewDND :: MonadIO m => Forest a -- ^ the inital tree stored in this model
  -> Maybe (DragSourceIface TreeStore a) -- ^ an optional interface for drags
  -> Maybe (DragDestIface TreeStore a) -- ^ an optional interface to handle drops
  -> m (TreeStore a)
treeStoreNewDND forest mDSource mDDest = liftIO $ do
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

  customStoreNew storeRef mkTreeStore TreeModelIface {
    treeModelIfaceGetFlags = return [],

    treeModelIfaceGetIter = \path -> withStore $
      \Store { depth = d } -> fromPath d <$> treePathGetIndices' path,

    treeModelIfaceGetPath = \iter -> withStore $
      \Store { depth = d } -> treePathNewFromIndices' $ toPath d iter,

    treeModelIfaceGetRow  = \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
        case checkSuccess d iter cache of
          (True, cache'@((_, (Node { rootLabel = val }:_)):_)) ->
            (val, cache')
          _ -> error "TreeStore.getRow: iter does not refer to a valid entry",

    treeModelIfaceIterNext = \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } -> iterNext d iter cache,

    treeModelIfaceIterChildren = \mIter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNthChild d 0 iter cache,

    treeModelIfaceIterHasChild = \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
       let (mIter, cache') = iterNthChild d 0 iter cache
        in (isJust mIter, cache'),

    treeModelIfaceIterNChildren = \mIter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNChildren d iter cache,

    treeModelIfaceIterNthChild = \mIter idx  -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNthChild d idx iter cache,

    treeModelIfaceIterParent = \iter -> withStore $
      \Store { depth = d } -> return $ iterParent d iter,

    treeModelIfaceRefNode = \_ -> return (),
    treeModelIfaceUnrefNode = \_ -> return ()
   } mDSource mDDest


-- | Default drag functions for
-- 'Graphics.UI.Gtk.ModelView.TreeStore'. These functions allow the rows of
-- the model to serve as drag source. Any row is allowed to be dragged and the
-- data set in the 'SelectionDataM' object is set with 'treeSetRowDragData',
-- i.e. it contains the model and the 'TreePath' to the row.
treeStoreDefaultDragSourceIface :: DragSourceIface TreeStore row
treeStoreDefaultDragSourceIface = DragSourceIface {
    treeDragSourceRowDraggable = \_ _-> return True,
    treeDragSourceDragDataGet = \model path sel -> treeSetRowDragData sel model path,
    treeDragSourceDragDataDelete = \model path -> treePathGetIndices' path >>= \dest@(_:_) -> do
            liftIO $ treeStoreRemove model path
            return True

  }

-- | Default drop functions for 'Graphics.UI.Gtk.ModelView.TreeStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
treeStoreDefaultDragDestIface :: DragDestIface TreeStore row
treeStoreDefaultDragDestIface = DragDestIface {
    treeDragDestRowDropPossible = \model path sel -> do
      mModelPath <- treeGetRowDragData sel
      case mModelPath of
        (True, Just model', source) -> do
            tm <- toTreeModel model
            withManagedPtr tm $ \m ->
                withManagedPtr model' $ \m' -> return (m==m')
        _ -> return False,
    treeDragDestDragDataReceived = \model path sel -> do
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
                row <- treeStoreGetTree model =<< treePathNewFromIndices' source
                initPath <- treePathNewFromIndices' (init dest)
                treeStoreInsertTree model initPath (fromIntegral $ last dest) row
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

getBitSlice :: TreeIterRaw -> Int -> Int -> Word32
getBitSlice (TreeIterRaw _ a b c) off count =
      getBitSliceWord a  off     count
  .|. getBitSliceWord b (off-32) count
  .|. getBitSliceWord c (off-64) count

  where getBitSliceWord :: Word32 -> Int -> Int -> Word32
        getBitSliceWord word off count =
          word `shift` (-off) .&. (1 `shiftL` count - 1)

setBitSlice :: TreeIterRaw -> Int -> Int -> Word32 -> TreeIterRaw
setBitSlice (TreeIterRaw stamp a b c) off count value =
  assert (value < 1 `shiftL` count) $
  TreeIterRaw stamp
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
invalidIter :: TreeIterRaw
invalidIter = TreeIterRaw 0 0 0 0

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
toPath :: Depth -> TreeIterRaw -> [Int32]
toPath d iter = gP 0 d
  where
  gP pos [] = []
  gP pos (d:ds) = let idx = getBitSlice iter pos d in
                  if idx==0 then [] else fromIntegral (idx-1) : gP (pos+d) ds

-- | Try to convert a path into a 'TreeIter'.
--
fromPath :: Depth -> [Int32] -> Maybe TreeIterRaw
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
type Cache a = [(TreeIterRaw, Forest a)]


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
  root = error "TreeStore.storeToCache: accessed non-exitent root of tree"

-- | Extract the store from the cache data structure.
cacheToStore :: Cache a -> Forest a
cacheToStore [] = []
cacheToStore cache = case last cache of (_, [Node _ forest]) -> forest

-- | Advance the traversal structure to the given 'TreeIter'.
--
advanceCache :: Depth -> TreeIterRaw -> Cache a -> Cache a
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
checkSuccess :: Depth -> TreeIterRaw -> Cache a -> (Bool, Cache a)
checkSuccess depth iter cache = case advanceCache depth iter cache of
    cache'@((cur,sibs):_) -> (cmp cur iter && not (null sibs), cache')
    [] -> (False, [])
  where
  cmp (TreeIterRaw _ a1 b1 c1) (TreeIterRaw _ a2 b2 c2) =
      a1==a2 && b1==b2 && c2==c2

-- | Get the leaf index of this iterator.
--
-- * Due to the way we construct the 'TreeIter's, we can check which the last
--   level of an iterator is: The bit sequence of level n is zero if n is
--   greater or equal to the level that the iterator refers to. The returned
--   triple is (pos, leaf, zero) such that pos..pos+leaf denotes the leaf
--   index and pos+leaf..pos+leaf+zero denotes the bit field that is zero.
--
getTreeIterLeaf :: Depth -> TreeIterRaw -> (Int, Int, Int)
getTreeIterLeaf ds ti = gTIL 0 0 ds
  where
  gTIL pos dCur (dNext:ds)
    | getBitSlice ti (pos+dCur) dNext==0 = (pos,dCur,dNext)
    | otherwise = gTIL (pos+dCur) dNext ds
  gTIL pos d [] = (pos, d, 0)

-- | Move an iterator forwards on the same level.
--
iterNext :: Depth -> TreeIterRaw -> Cache a -> (Maybe TreeIterRaw, Cache a)
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
iterNthChild :: Depth -> Int -> TreeIterRaw -> Cache a  ->
                (Maybe TreeIterRaw, Cache a)
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
iterNChildren :: Depth -> TreeIterRaw -> Cache a -> (Int, Cache a)
iterNChildren depth iter cache = case checkSuccess depth iter cache of
  (True, cache@((_,Node { subForest = forest}:_):_)) -> (length forest, cache)
  (_, cache) -> (0, cache)


-- | Ascend to parent.
--
iterParent :: Depth -> TreeIterRaw -> Maybe TreeIterRaw
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
treeStoreInsertForest :: MonadIO m
 => TreeStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> Forest a    -- ^ the list of trees to be inserted
 -> m ()
treeStoreInsertForest (TreeStore model) path pos nodes = liftIO $ do
  ipath <- treePathGetIndices' path
  customStoreInvalidateIters $ CustomStore model
  (idx, toggle) <- atomicModifyIORef (customStoreGetPrivate $ CustomStore model) $
    \store@Store { depth = d, content = cache } ->
    case insertIntoForest (cacheToStore cache) nodes ipath pos of
      Nothing -> error ("treeStoreInsertForest: path does not exist " ++ show ipath)
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
                  treeModelRowInserted (CustomStore model) p'' =<< treeIterFromRaw (treeIterSetStamp iter stamp)
            | (i, node) <- zip [idx..] nodes
            , p <- paths (fromIntegral i : rpath) node ]
  let Just iter = fromPath depth ipath
  when toggle $ treeModelRowHasChildToggled (CustomStore model) path
                =<< treeIterFromRaw (treeIterSetStamp iter stamp)

  where paths :: [Int32] -> Tree a -> [[Int32]]
        paths path Node { subForest = ts } =
          path : concat [ paths (n:path) t | (n, t) <- zip [0..] ts ]

-- | Insert a node into the store.
--
treeStoreInsertTree :: MonadIO m
 => TreeStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> Tree a      -- ^ the value to be inserted
 -> m ()
treeStoreInsertTree store path pos node =
  treeStoreInsertForest store path pos [node]

-- | Insert a single node into the store.
--
-- * This function inserts a single node without children into the tree.
--   Its arguments are similar to those of 'treeStoreInsert'.
--
treeStoreInsert :: MonadIO m
 => TreeStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> a           -- ^ the value to be inserted
 -> m ()
treeStoreInsert store path pos node =
  treeStoreInsertForest store path pos [Node node []]

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
treeStoreRemove :: MonadIO m => TreeStore a -> TreePath -> m Bool
treeStoreRemove model path = treePathGetIndices' path >>= treeStoreRemoveImpl model path

treeStoreRemoveImpl :: MonadIO m => TreeStore a -> TreePath -> [Int32] -> m Bool
  --TODO: eliminate this special case without segfaulting!
treeStoreRemoveImpl (TreeStore model) _ [] = return False
treeStoreRemoveImpl (TreeStore model) path ipath = liftIO $ do
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
      treeModelRowHasChildToggled (CustomStore model) parent =<< treeIterFromRaw iter
    treeModelRowDeleted (CustomStore model) path
  return found

treeStoreClear :: MonadIO m => TreeStore a -> m ()
treeStoreClear (TreeStore model) = liftIO $ do
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
treeStoreSetValue :: MonadIO m => TreeStore a -> TreePath -> a -> m ()
treeStoreSetValue store path value = treeStoreChangeM store path (\_ -> return value)
                                  >> return ()


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a monadic version, see
--   'treeStoreChangeM'.
--
treeStoreChange :: MonadIO m => TreeStore a -> TreePath -> (a -> a) -> m Bool
treeStoreChange store path func = treeStoreChangeM store path (return . func)


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a purely functional version, see
--   'treeStoreChange'.
--
treeStoreChangeM :: MonadIO m => TreeStore a -> TreePath -> (a -> m a) -> m Bool
treeStoreChangeM (TreeStore model) path act = do
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
  when found $ treeModelRowChanged (CustomStore model) path =<< treeIterFromRaw (treeIterSetStamp iter stamp)
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
treeStoreGetValue :: MonadIO m => TreeStore a -> TreePath -> m a
treeStoreGetValue model path = fmap rootLabel (treeStoreGetTree model path)

-- | Extract a subtree from the current model. Fails if the given
--   'TreePath' refers to a non-existent node.
--
treeStoreGetTree :: MonadIO m => TreeStore a -> TreePath -> m (Tree a)
treeStoreGetTree (TreeStore model) path = liftIO $ do
  ipath <- treePathGetIndices' path
  store@Store { depth = d, content = cache } <-
      readIORef (customStoreGetPrivate (CustomStore model))
  case fromPath d ipath of
    (Just iter) -> do
      let (res, cache') = checkSuccess d iter cache
      writeIORef (customStoreGetPrivate (CustomStore model)) store { content = cache' }
      case cache' of
        ((_,node:_):_) | res -> return node
        _ -> fail ("treeStoreGetTree: path does not exist " ++ show ipath)
    _ -> fail ("treeStoreGetTree: path does not exist " ++ show ipath)

-- | Extract a subtree from the current model. Like 'treeStoreGetTree'
--   but returns @Nothing@ if the path refers to a non-existant node.
--
treeStoreLookup :: MonadIO m => TreeStore a -> TreePath -> m (Maybe (Tree a))
treeStoreLookup (TreeStore model) path = liftIO $ do
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
