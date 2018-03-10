module Labyrinth(Labyrinth(..), labyConstruct, BoxState(..), labyMarkBox) where

import Control.Concurrent.STM(STM)
import Control.Concurrent.STM.TArray(TArray)
import Data.Array.MArray(newArray,writeArray)
import Rectangle
import Grid

data BoxState = Empty | Border | Start | End

data Labyrinth = Labyrinth {
  labyBoxState :: TArray (Int, Int) BoxState,
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

labyMarkBox
  :: PointInScreenCoordinates Int
  -> BoxState
  -> Maybe Labyrinth
  -> STM (Maybe Labyrinth)
labyMarkBox _     _        Nothing          = return Nothing
labyMarkBox point boxState (Just labyrinth) = do
  let grid = labyGrid labyrinth
      box  = grPixelToBox grid point
  case box of
    Just pt ->
      writeArray (labyBoxState labyrinth) pt boxState
    Nothing -> return ()
  return $ Just labyrinth

