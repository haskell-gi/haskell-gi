module Grid(
  Grid(..),
  grPixelToBox
) where 

import Rectangle

newtype PointInGridCoordinates a = PtGrid { grPtGrid :: Point a }
newtype PointInScreenCoordinates a = PtScreen { grPtScreen :: Point a }

data (Num a, Ord a) => Grid a = Grid {
  grScreenSize :: (a, a),
  grRectangle :: Rectangle a,
  grBoxSize :: a,
  grXBoxCnt :: a,
  grYBoxCnt :: a
}

grPixelToBox :: (Integral a, Ord a) => Grid a -> PointInScreenCoordinates a -> Maybe (PointInGridCoordinates a)
grPixelToBox grid PtScreen { grPtScreen = Point (x,y) } = let rectangle = grRectangle grid
                                                              boxSize = grBoxSize grid
                                                              maxX = grXBoxCnt grid - 1
                                                              maxY = grYBoxCnt grid - 1
                                                          in (if rIsInside rectangle (Point (x,y)) then (
                                                                 let grX = min (quot (x - rTopLeftX rectangle ) boxSize) maxX
                                                                     grY = min (quot (y - rTopLeftY rectangle ) boxSize) maxY
                                                                 in Just PtGrid { grPtGrid = Point ( grX, grY ) } )
                                                               else Nothing)
                                                                  


