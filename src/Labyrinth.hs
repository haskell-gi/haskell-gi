module Labyrinth(Labyrinth) where

import Data.Array.ST
import Rectangle

data Labyrinth s = Labyrinth {
  labyrinthBoxState :: STUArray s (Int, Int) Int
}


