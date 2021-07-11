module Grid(
  Grid(..),
  PointInGridCoordinates(..),
  PointInScreenCoordinates(..),
  RectangleInGridCoordinates(..),
  RectangleInScreenCoordinates(..),
  grPixelToBox,
  grBoxToPixel,
  grPixelAreaToBoxArea,
  grAxesList,
) where 

import Data.SafeCopy(SafeCopy(..), contain, safePut, safeGet)  
import Data.Typeable
import Rectangle

type PointInGridCoordinates a = Point a
type PointInScreenCoordinates a = Point a
type RectangleInGridCoordinates a = Rectangle a
type RectangleInScreenCoordinates a = Rectangle a

data Grid a = Grid {
  grScreenSize :: (a, a),
  grRectangle :: Rectangle a,
  grLegendRectangle :: Rectangle a,
  grBoxSize :: a,
  grXBoxCnt :: a,
  grYBoxCnt :: a,
  grBorderSize :: a
} deriving(Eq, Show)

instance (Typeable a, SafeCopy a) => SafeCopy (Grid a) where 
  putCopy (Grid ss r lr bs xbc ybc bos) = contain $ do safePut ss; safePut r; safePut lr; safePut bs;
                                                       safePut xbc; safePut ybc; safePut bos;
  getCopy = contain $ Grid <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet 
                                       <*> safeGet <*> safeGet


grPixelToBox :: (Integral a, Ord a) => Grid a -> PointInScreenCoordinates a -> Maybe (PointInGridCoordinates a)
grPixelToBox grid (x,y) = let rectangle = grRectangle grid
                              boxSize = grBoxSize grid
                              maxX = grXBoxCnt grid - 1
                              maxY = grYBoxCnt grid - 1
                          in (if rIsInside rectangle (x,y) then (
                            let grX = min (quot (x - rTopLeftX rectangle ) boxSize) maxX
                                grY = min (quot (y - rTopLeftY rectangle ) boxSize) maxY
                            in Just (grX, grY) )
                            else Nothing)

grBoxToPixel :: (Integral a, Ord a) => Grid a -> PointInGridCoordinates a -> Maybe (RectangleInScreenCoordinates a)
grBoxToPixel grid (x,y) = 
  if x < 0 || x >= grXBoxCnt grid || y < 0 || y >= grYBoxCnt grid then Nothing
  else Just $ Rectangle topLeftX topLeftY width height 
  where rectangle = grRectangle grid
        topLeftX = rTopLeftX rectangle + grBoxSize grid * x + grBorderSize grid
        topLeftY = rTopLeftY rectangle + grBoxSize grid * y + grBorderSize grid
        width = grBoxSize grid - grBorderSize grid
        height = width


grPixelAreaToBoxArea ::(Integral a, Ord a) => Grid a -> RectangleInScreenCoordinates a 
                                                     -> Maybe (RectangleInGridCoordinates a)
grPixelAreaToBoxArea grid area = 
  case rIntersect (grRectangle grid) area of 
    Just intersection -> let topLeft = grPixelToBox grid (rTopLeft intersection)
                             bottomRight = grPixelToBox grid (rBottomRight intersection)
                         in case (topLeft, bottomRight) of 
                            (Just (x1, y1), Just (x2,y2)) -> Just $ Rectangle x1 y1 (x2 - x1) (y2 - y1)
                            _ -> Nothing
    Nothing -> Nothing
         
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
        buildAxis cnt = buildRectangle dim fixCoord (gridBoxSize * cnt + topLeftFn gridRectangle)
        buildRectangle XDim fixCoord runCoord = Rectangle runCoord fixCoord borderSize (rHeight gridRectangle)
        buildRectangle YDim fixCoord runCoord = Rectangle fixCoord runCoord (rWidth gridRectangle) borderSize
