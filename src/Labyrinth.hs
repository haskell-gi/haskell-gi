module Labyrinth(Labyrinth(..), labyConstruct, BoxState(..), labyMarkBox) where

import Control.Concurrent.STM(STM)
import Control.Concurrent.STM.TArray(TArray)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Array.MArray(newArray,writeArray,readArray)
import Rectangle
import Grid

data BoxState = Empty | Border | Start | End

type LabyArray = TArray (Int, Int) BoxState

data Labyrinth = Labyrinth {
  labyBoxState :: LabyArray,
  labyGrid :: Grid Int
}

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

labyMarkBox :: PointInScreenCoordinates Int -> BoxState -> Maybe Labyrinth -> STM (Maybe Labyrinth)
labyMarkBox _     _        Nothing          = return Nothing
labyMarkBox point boxState (Just labyrinth) = 
  do
    let grid = labyGrid labyrinth
        box  = grPixelToBox grid point
    case box of
      Just pt ->
        writeArray (labyBoxState labyrinth) pt boxState
      Nothing -> return ()
    return $ Just labyrinth

-- labyGetBoxesInsideArea :: RectangleInScreenCoordinates Int -> Maybe Labyrinth 
--                                                            -> STM ( [ (BoxState, RectangleInScreenCoordinates Int)] )
-- labyGetBoxesInsideArea _    Nothing = return []                                                          
-- labyGetBoxesInsideArea area (Just labyrinth) =
--   do
--     let  grid = labyGrid labyrinth
--          boxArea = grPixelAreaToBoxArea grid area
--          array = labyBoxState labyrinth
--     case boxArea of 
--       Just boxes -> sequence $ [ boxDatum | 
--                       x <- [(rTopLeftX boxes)..(rBottomRightX boxes)],
--                       y <- [(rTopLeftY boxes)..(rBottomRightY boxes)],
--                       Just boxDatum <- [(labyGetBoxData array grid (x,y))] ] 
--       Nothing -> return []

labyGetBoxData :: LabyArray -> Grid Int 
                            -> PointInGridCoordinates Int 
                            -> MaybeT STM (BoxState, RectangleInScreenCoordinates Int)
labyGetBoxData array grid (x,y) =
  do
    let pixel = grBoxToPixel grid (x,y)
    boxState <- lift $ readArray array (x,y)
    case pixel of 
      Just p -> lift $ return (boxState, p)
      Nothing -> MaybeT $ return Nothing

  -- in do boxS
  -- in case pixel of 
  --     Just p -> do boxState <- readArray array (x,y)
  --                  lift $ return (boxState, p)
  --     Nothing -> lift $ return Nothing

