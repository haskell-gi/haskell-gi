module Labyrinth(
  Labyrinth(..), 
  BoxState(..), 
  RedrawInfo(..),
  labyConstruct, 
  labyMarkBox,
  labyGetRedrawInfo,
  labyStateToColor) where

import Data.Maybe(isJust)
import Data.Array.MArray(newArray,writeArray,readArray)

import Control.Error.Util(hoistMaybe)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT), runMaybeT)
import Control.Concurrent.STM(STM)
import Control.Concurrent.STM.TArray(TArray)

import Rectangle
import Grid

data BoxState = Empty | Border | Start | End deriving(Eq, Show)

type LabyArray = TArray (Int, Int) BoxState

data Labyrinth = Labyrinth {
  labyBoxState :: LabyArray,
  labyGrid :: Grid Int
}

data RedrawInfo = RedrawInfo {
  labyRedrIntersect :: Maybe (Rectangle Int),  -- intersection with playing area
  labyRedrLegend :: Bool,                   
  labyRedrGrid :: Grid Int,
  labyRedrBoxes :: [ (BoxState, RectangleInScreenCoordinates Int) ]
} deriving(Show)

marginFactor :: Int
marginFactor = 32

legendBottomMargin :: Int 
legendBottomMargin = 5

legendLeftMargin :: Int 
legendLeftMargin = 10

labyConstruct :: Int -> Int -> (Int, Int) -> (Int, Int) -> STM Labyrinth
labyConstruct boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight) = do
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
      , grLegendRectangle = Rectangle legendLeftMargin
                                      (totalHeight - legendHeight - legendBottomMargin)
                                      legendWidth
                                      legendHeight
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

labyGetRedrawInfo :: Maybe Labyrinth -> Rectangle Int -> STM (Maybe RedrawInfo)
labyGetRedrawInfo Nothing _             = return Nothing
labyGetRedrawInfo (Just labyrinth) area =
  do
    let grid = labyGrid labyrinth
        rectangle = grRectangle grid
        intersection = rIntersect area rectangle
        redrawLegend = isJust $ rIntersect area (grLegendRectangle grid)
    boxes <- case intersection of 
      Just intersection -> labyGetBoxesInsideArea intersection labyrinth
      Nothing -> return []
    return $ Just RedrawInfo { 
      labyRedrIntersect = intersection,
      labyRedrGrid = grid,
      labyRedrBoxes = boxes,
      labyRedrLegend = redrawLegend
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
labyStateToColor Empty = (1.0, 1.0, 1.0)
labyStateToColor Border = (0, 0, 1.0)
labyStateToColor _ = (1.0, 0.0, 0.0)


