{-# LANGUAGE OverloadedRecordDot, OverloadedLabels, LambdaCase #-}

import Data.GI.Base (new)

import HListStore (HListStore(..))

import Control.Monad ((>=>))

-- | Return the values contained in the HListStore, in order.
items :: HListStore a -> IO [a]
items l = collectItems 0 []
  where collectItems n acc = l.getItem n >>= \case
          Just item -> collectItems (n+1) (item : acc)
          Nothing -> return $ reverse acc

printList :: Show a => HListStore a -> IO ()
printList = items >=> print

main :: IO ()
main = do
  hlist <- new HListStore []

  hlist.append (3 :: Int)
  hlist.append 12
  hlist.append 71
  hlist.append 31
  printList hlist

  -- Insert 19 so that every element before it is smaller than it.
  _ <- hlist.insertSorted 19
  printList hlist

  -- Replace the two element starting from the second one (so 12 and
  -- 19) with 21, 13 and 144.
  hlist.splice 1 2 [21, 13, 144]
  printList hlist

 -- Reverse sort the list.
  hlist.sortBy (flip compare)
  printList hlist

  putStrLn "done!"
