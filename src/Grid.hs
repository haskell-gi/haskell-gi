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
  grYBoxCnt :: a,
  grBorderSize :: a
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
            
data BuildAxesDimension = XDim | YDim

grAxesList :: Rectangle Int -> Grid Int -> [Rectangle Int]                                                                 
grAxesList area grid = xAxes ++ yAxes
  where xAxes = grBuildAxesList XDim area grid rTopLeftX rBottomRightX rTopLeftY (grXBoxCnt grid)
        yAxes = grBuildAxesList YDim area grid rTopLeftY rBottomRightY rTopLeftX (grYBoxCnt grid)

grBuildAxesList :: BuildAxesDimension -> Rectangle Int 
                                      -> Grid Int 
                                      -> (Rectangle Int -> Int) 
                                      -> (Rectangle Int -> Int) 
                                      -> (Rectangle Int -> Int)
                                      -> Int 
                                      -> [Rectangle Int]
grBuildAxesList dim area grid topLeftFn bottomRightFn topLeftOtherDim boxCnt =
  [ r | cnt <- [startCnt .. (min endCnt (boxCnt + 1))], Just r <- [rIntersect (buildAxis cnt) area]]
  where gridRectangle = grRectangle grid
        gridBoxSize = grBoxSize grid
        borderSize = grBorderSize grid
        startCnt = quot (topLeftFn area - topLeftFn gridRectangle + gridBoxSize - 1) gridBoxSize
        endCnt = quot (bottomRightFn area - topLeftFn gridRectangle + gridBoxSize - 1) gridBoxSize
        fixCoord = max (topLeftOtherDim area) (topLeftOtherDim gridRectangle)
        buildAxis :: Int -> Rectangle Int
        buildAxis cnt = buildRectangle dim fixCoord (gridBoxSize * cnt + topLeftFn gridRectangle)
        buildRectangle XDim fixCoord runCoord = Rectangle runCoord fixCoord borderSize (rHeight gridRectangle)
        buildRectangle YDim fixCoord runCoord = Rectangle fixCoord runCoord (rWidth gridRectangle) borderSize