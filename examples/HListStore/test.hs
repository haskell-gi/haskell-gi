{-# LANGUAGE OverloadedRecordDot, OverloadedLabels, LambdaCase #-}
{- cabal:
build-depends: base >= 4.16, haskell-gi-base, gi-gio, gi-gobject
ghc-options: -Wall
-}
import Data.GI.Base

import Control.Monad (forM)
import Data.Typeable (Typeable)

import HListStore (HListStore(..))

printList :: (Show a, Typeable a) => HListStore a -> IO ()
printList l = do
  nitems <- l `get` #nItems
  items <- forM [0..nitems-1] $ \n -> do
    l.getItem n >>= \case
      Nothing -> error "Missing item!?"
      Just x -> return x

  print items

main :: IO ()
main = do
  hlist <- new HListStore [] :: IO (HListStore Int)

  hlist.append 3
  hlist.append 12
  hlist.append 71
  hlist.append 31
  _ <- hlist.insertSorted 19

  printList hlist

  hlist.splice 2 2 [21, 13, 144]

  printList hlist

  hlist.sortBy $ \x y -> case compare x y of
    GT -> LT
    LT -> GT
    EQ -> EQ

  printList hlist

  putStrLn "done!"
