module Grid(Grid(..)) where 

import Rectangle

data (Num a, Ord a) => Grid a = Grid {
  grScreenSize :: (a, a),
  grRectangle :: Rectangle a,
  grBoxSize :: a,
  grXBoxCnt :: a,
  grYBoxCnt :: a
}


