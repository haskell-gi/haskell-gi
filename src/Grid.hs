module Grid(
  Grid(..),
  PointInGridCoordinates(..),
  PointInScreenCoordinates(..),
  grPixelToBox,
  grAxesList,
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
            
grAxesList :: Rectangle Int -> Grid Int -> [Rectangle Int]                                                                 
grAxesList area grid = grAxesListX area grid ++ grAxesListY area grid

grAxesListX :: Rectangle Int -> Grid Int -> [Rectangle Int]  
grAxesListX area grid = 
  let gridRectangle = grRectangle grid
      gridBoxSize = grBoxSize grid
      startXCnt = quot (rTopLeftX area - rTopLeftX gridRectangle + gridBoxSize - 1) gridBoxSize
      endXCnt = quot (rBottomRightX area - rTopLeftX gridRectangle + gridBoxSize - 1) gridBoxSize
      startY = max  (rTopLeftY area) (rTopLeftY gridRectangle)
      getLine xCnt = Rectangle (gridBoxSize * xCnt + rTopLeftX gridRectangle) startY 0 (rHeight gridRectangle)
  in  [ r | cnt <- [startXCnt .. (min endXCnt (grXBoxCnt grid + 1))], Just r <- [rIntersect (getLine cnt) area]]
    
grAxesListY :: Rectangle Int -> Grid Int -> [Rectangle Int]  
grAxesListY area grid = 
  let gridRectangle = grRectangle grid
      gridBoxSize = grBoxSize grid
      startYCnt = quot (rTopLeftY area - rTopLeftY gridRectangle + gridBoxSize - 1) gridBoxSize
      endYCnt = quot (rBottomRightY area - rTopLeftY gridRectangle + gridBoxSize - 1) gridBoxSize
      startX = max (rTopLeftX area) (rTopLeftX gridRectangle)
      getLine yCnt = Rectangle startX (gridBoxSize * yCnt + rTopLeftY gridRectangle) (rWidth gridRectangle) 0
  in [ r | cnt <- [startYCnt .. (min endYCnt (grYBoxCnt grid + 1))], Just r <- [rIntersect (getLine cnt) area]]

