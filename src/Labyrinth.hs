module Labyrinth(Labyrinth) where

import Data.Array.ST
import Grid

data Labyrinth a = Labyrinth {
  labyBoxState :: Maybe (STUArray a (Int, Int) Int),
  labyGrid :: Maybe (Grid Int)
}

type MaybeLabyrinth a = Maybe (Labyrinth a)

