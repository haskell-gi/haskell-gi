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

labyConstruct :: Int -> (Int, Int) -> STM Labyrinth
labyConstruct boxSize (totalWidth, totalHeight) =  
  do let leftMargin = quot totalWidth marginFactor 
         topMargin = quot totalHeight marginFactor
         width = (quot (totalWidth - 2 * leftMargin) boxSize ) * boxSize
         height = (quot (totalHeight - 2 * topMargin) boxSize ) * boxSize
         xBoxCnt = quot width boxSize
         yBoxCnt = quot height boxSize
         arrayDimension = ((0,0),(xBoxCnt-1, yBoxCnt-1))
     array <- newArray arrayDimension Empty
     return Labyrinth {
        labyBoxState = array,
        labyGrid = Grid {
          grScreenSize = (totalWidth, totalHeight),
          grRectangle = Rectangle  (quot totalWidth 2 - quot width 2)
                                   (quot totalHeight 2 - quot height 2)
                                   (width + 1)
                                   (height + 1),
          grBoxSize = boxSize,
          grXBoxCnt = xBoxCnt,
          grYBoxCnt = yBoxCnt
        }
     }

labyMarkBox :: PointInScreenCoordinates Int -> BoxState -> Maybe Labyrinth -> STM (Maybe Labyrinth)
labyMarkBox _      _        Nothing          = return Nothing
labyMarkBox point  boxState (Just labyrinth) = 
  do  let grid = labyGrid labyrinth 
          box = grPixelToBox grid point
      case box of
        Just PtGrid { grPtGrid = Point pt } -> writeArray (labyBoxState labyrinth) pt boxState
        Nothing -> return ()
      return $ Just labyrinth
      
