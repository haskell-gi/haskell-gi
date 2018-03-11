module Labyrinth(
  Labyrinth(..), 
  BoxState(..), 
  RedrawInfo(..),
  labyConstruct, 
  labyMarkBox,
  labyGetRedrawInfo,
  labyStateToColor) where

import Control.Error.Util(hoistMaybe)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM(STM)
import Control.Concurrent.STM.TArray(TArray)
import Data.Array.MArray(newArray,writeArray,readArray)

import Rectangle
import Grid

data BoxState = Empty | Border | Start | End deriving(Eq, Show)

type LabyArray = TArray (Int, Int) BoxState

data Labyrinth = Labyrinth {
  labyBoxState :: LabyArray,
  labyGrid :: Grid Int
}

data RedrawInfo = RedrawInfo {
  labyRedrIntersect :: Rectangle Int,
  labyRedrGrid :: Grid Int,
  labyRedrBoxes :: [ (BoxState, RectangleInScreenCoordinates Int) ]
} deriving(Show)

marginFactor :: Int
marginFactor = 32

labyConstruct :: Int -> Int -> (Int, Int) -> STM Labyrinth
labyConstruct boxSize borderSize (totalWidth, totalHeight) = do
  let leftMargin     = quot totalWidth marginFactor
      topMargin      = quot totalHeight marginFactor
      xBoxCnt        = quot (totalWidth - 2 * leftMargin) boxSize
      yBoxCnt        = quot (totalHeight - 2 * topMargin) boxSize
      width          = xBoxCnt * boxSize + borderSize
      height         = yBoxCnt * boxSize + borderSize
      arrayDimension = ((0, 0), (xBoxCnt - 1, yBoxCnt - 1))
  array <- newArray arrayDimension Empty
  return Labyrinth
    { labyBoxState = array
    , labyGrid     = Grid
      { grScreenSize = (totalWidth, totalHeight)
      , grRectangle  = Rectangle (quot (totalWidth - width) 2)
                                 (quot (totalHeight - height) 2)
                                 width
                                 height
      , grBoxSize    = boxSize
      , grXBoxCnt    = xBoxCnt
      , grYBoxCnt    = yBoxCnt
      , grBorderSize = borderSize
      }
    }

labyMarkBox :: PointInScreenCoordinates Int -> BoxState -> Maybe Labyrinth -> MaybeT STM (Labyrinth, Rectangle Int)
labyMarkBox _     _        Nothing          = MaybeT $ return Nothing
labyMarkBox point boxState (Just labyrinth) = 
  do
    let grid = labyGrid labyrinth
    box <- hoistMaybe $ grPixelToBox grid point
    repaintArea <- hoistMaybe $ grBoxToPixel grid box
    lift $ writeArray (labyBoxState labyrinth) box boxState
    hoistMaybe $ Just (labyrinth, repaintArea)

labyGetRedrawInfo :: Maybe Labyrinth -> Rectangle Int -> MaybeT STM RedrawInfo
labyGetRedrawInfo Nothing _ = MaybeT $ return Nothing
labyGetRedrawInfo (Just labyrinth) area =
  do let rectangle = grRectangle $ labyGrid labyrinth
     intersection <- hoistMaybe $ rIntersect area rectangle
     boxes <- lift $ labyGetBoxesInsideArea intersection labyrinth
     return RedrawInfo { 
        labyRedrIntersect = intersection,
        labyRedrGrid = labyGrid labyrinth,
        labyRedrBoxes = boxes
    } 

labyGetBoxesInsideArea :: RectangleInScreenCoordinates Int -> Labyrinth 
                                                           -> STM [ (BoxState, RectangleInScreenCoordinates Int)]                                                        
labyGetBoxesInsideArea area labyrinth =
  do
    let  grid = labyGrid labyrinth
         boxArea = grPixelAreaToBoxArea grid area
         array = labyBoxState labyrinth
    case boxArea of 
      Just boxes -> sequence [ boxDatum | 
                      x <- [(rTopLeftX boxes)..(rBottomRightX boxes)],
                      y <- [(rTopLeftY boxes)..(rBottomRightY boxes)],
                      Just boxDatum <- [labyGetBoxData array grid (x,y)] ] 
      Nothing -> return []

labyGetBoxData :: LabyArray -> Grid Int 
                            -> PointInGridCoordinates Int 
                            -> Maybe (STM (BoxState, RectangleInScreenCoordinates Int))
labyGetBoxData array grid point =
  do pixel <- grBoxToPixel grid point
     return $ labyGetBoxTuple array point pixel

labyGetBoxTuple :: LabyArray -> PointInGridCoordinates Int 
                             -> Rectangle Int 
                             -> STM (BoxState, RectangleInScreenCoordinates Int)
labyGetBoxTuple array point rectangle =
  do
    boxState <- readArray array point
    return (boxState, rectangle)

labyStateToColor :: BoxState -> (Double, Double, Double)
labyStateToColor Empty = (255, 255, 255)
labyStateToColor Border = (0, 0, 255)
labyStateToColor _ = (255, 0, 0)


