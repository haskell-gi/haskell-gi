{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Labyrinth(
  Labyrinth(..), 
  BoxState(..), 
  RedrawInfo(..),
  NextAction(..),
  ActionType(..),
  FrozenLabyrinth(..),
  labyConstruct, 
  labyMarkBox,
  labyGetRedrawInfo,
  labySetNextAction,
  labyStateToColor,
  labyClear,
  labyFreeze,
  labyThaw) where

import GHC.Generics
import Data.Binary(Binary)

import Data.Maybe(isJust, catMaybes)
import Data.Array.MArray(newArray,writeArray,readArray,mapArray,getElems,freeze,thaw)
import Data.Array.IArray(Array)

import Control.Error.Util(hoistMaybe)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT), runMaybeT)
import Control.Concurrent.STM(STM,TVar,readTVar,newTVar,modifyTVar,writeTVar)
import Control.Concurrent.STM.TArray(TArray)

import Rectangle
import Grid

data BoxState = Empty | Border | StartField | TargetField deriving(Eq, Show, Generic)
data NextAction = SetBorder | SetStartField | SetTargetField deriving(Eq, Show, Generic)
data ActionType = SetAction | UnSetAction deriving(Eq, Show, Generic)

type LabyArray = TArray (Int, Int) BoxState 
type FrozenLabyArray = Array (Int, Int) BoxState 

data Labyrinth = Labyrinth {
  labyBoxState :: LabyArray,
  labyGrid :: Grid Int,
  labyNextAction :: TVar NextAction,
  labyStartField :: TVar (Maybe (Int, Int)),
  labyTargetField :: TVar (Maybe (Int, Int))
}  

instance Binary NextAction 
instance Binary BoxState
instance Binary ActionType

data FrozenLabyrinth = FrozenLabyrinth {
  frLabyBoxState :: FrozenLabyArray,
  frLabyGrid :: Grid Int,
  frLabyNextAction :: NextAction,
  frLabyStartField :: Maybe (Int, Int),
  frLabyTargetField :: Maybe (Int, Int)
} deriving (Generic)

instance Binary FrozenLabyrinth

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

labyConstruct :: Maybe Labyrinth -> Int -> Int -> (Int, Int) -> (Int, Int) -> STM Labyrinth 
labyConstruct Nothing boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight) = 
  labyConstructNew boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight)
labyConstruct (Just labyrinth) boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight) = 
  do isEmpty <- labyIsEmpty labyrinth 
     if isEmpty then labyConstructNew boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight)
     else labyConstructFrom labyrinth boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight)
  
labyConstructNew :: Int -> Int -> (Int, Int) -> (Int, Int) -> STM Labyrinth 
labyConstructNew boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight) = 
  do let leftMargin     = quot totalWidth marginFactor
         topMargin      = quot totalHeight marginFactor
         xBoxCnt        = quot (totalWidth - 2 * leftMargin) boxSize
         yBoxCnt        = quot (totalHeight - 2 * topMargin) boxSize
         width          = xBoxCnt * boxSize + borderSize
         height         = yBoxCnt * boxSize + borderSize
     array <- labyNewArray xBoxCnt yBoxCnt
     nextAction <- newTVar SetBorder
     startField <- newTVar Nothing
     targetField <- newTVar Nothing
     return Labyrinth
       { labyBoxState = array
       , labyNextAction = nextAction
       , labyStartField = startField
       , labyTargetField = targetField
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

labyConstructFrom :: Labyrinth -> Int -> Int -> (Int, Int) -> (Int, Int) -> STM Labyrinth  
labyConstructFrom labyrinth boxSize borderSize (legendWidth, legendHeight) (totalWidth, totalHeight) =
  do let oldGrid        = labyGrid labyrinth 
         (ow, oh)       = grScreenSize oldGrid
         xBoxCnt        = grXBoxCnt oldGrid
         yBoxCnt        = grYBoxCnt oldGrid 
         leftMargin     = quot totalWidth marginFactor
         topMargin      = quot totalHeight marginFactor 
         maxWidth       = totalWidth - 2 * leftMargin - borderSize
         maxHeight      = totalHeight - 2 * topMargin - borderSize
         newBoxSize     = min (quot maxWidth xBoxCnt) (quot maxHeight yBoxCnt)
         newBorderSize  = min borderSize (newBoxSize - 1)
         width          = newBoxSize * xBoxCnt + newBorderSize
         height         = newBoxSize * yBoxCnt + newBorderSize
     return Labyrinth
       { labyBoxState = labyBoxState labyrinth
       , labyNextAction = labyNextAction labyrinth
       , labyStartField = labyStartField labyrinth
       , labyTargetField = labyTargetField labyrinth
       , labyGrid     = Grid
         { grScreenSize = (totalWidth, totalHeight)
         , grRectangle  = Rectangle (quot (totalWidth - width) 2)
                                   (quot (totalHeight - height) 2)
                                   width
                                   height
         , grBoxSize    = newBoxSize
         , grXBoxCnt    = xBoxCnt
         , grYBoxCnt    = yBoxCnt
         , grBorderSize = newBorderSize
         , grLegendRectangle = Rectangle legendLeftMargin
                                         (totalHeight - legendHeight - legendBottomMargin)
                                         legendWidth
                                         legendHeight
         }
       }   

labyNewArray :: Int -> Int -> STM LabyArray
labyNewArray xBoxCnt yBoxCnt = let arrayDimension = ((0,0), (xBoxCnt - 1, yBoxCnt - 1)) 
                               in newArray arrayDimension Empty 

labyMarkBox :: PointInScreenCoordinates Int -> ActionType -> Maybe Labyrinth 
                                            -> MaybeT STM (Labyrinth, [Rectangle Int], Bool)
labyMarkBox _     _        Nothing          = hoistMaybe Nothing
labyMarkBox point actionType (Just labyrinth) = 
  do let grid = labyGrid labyrinth 
     box <- hoistMaybe $ grPixelToBox grid point 
     (boxesToBeRedrawn, resetCursor) <- lift $ labyMarkBoxDo box
     return (labyrinth, boxesToBeRedrawn, resetCursor)
  where
    labyMarkBoxDo :: PointInGridCoordinates Int -> STM ([Rectangle Int], Bool)
    labyMarkBoxDo box = do nextAction <- readTVar (labyNextAction labyrinth)
                           currentBoxState <- readArray (labyBoxState labyrinth) box
                           writeTVar (labyNextAction labyrinth) SetBorder
                           let targetState = getTargetState actionType nextAction
                           boxesToBeRedrawn <- labySetBoxState labyrinth box currentBoxState targetState 
                           return (boxesToBeRedrawn, nextAction /= SetBorder)
    getTargetState UnSetAction _              = Empty
    getTargetState SetAction   SetBorder      = Border
    getTargetState SetAction   SetStartField  = StartField
    getTargetState SetAction   SetTargetField = TargetField

labySetBoxState :: Labyrinth -> PointInGridCoordinates Int -> BoxState -> BoxState -> STM [Rectangle Int]
labySetBoxState _         _   currentState targetState | currentState == targetState = return []
labySetBoxState labyrinth box currentState targetState = 
  do redrawOld <- labyEnforceOnlyOne labyrinth targetState
     labyNewlySetBox labyrinth box currentState targetState
     let redrawInGridCoords   = box : redrawOld
         redrawInScreenCoords = map (grBoxToPixel $ labyGrid labyrinth) redrawInGridCoords
     return $ catMaybes redrawInScreenCoords

labyEnforceOnlyOne :: Labyrinth -> BoxState -> STM [PointInGridCoordinates Int]
labyEnforceOnlyOne labyrinth targetState
  | targetState `elem` [Empty, Border] = return []
  | targetState == StartField      = labyResetStartOrTarget labyrinth ( labyStartField labyrinth )
  | targetState == TargetField     = labyResetStartOrTarget labyrinth ( labyTargetField labyrinth )
  
labyNewlySetBox :: Labyrinth -> PointInGridCoordinates Int -> BoxState -> BoxState -> STM ()                                  
labyNewlySetBox labyrinth box currentState targetState =
  do unsetPrevious currentState
     writeArray (labyBoxState labyrinth) box targetState
     setStartOrTarget targetState
  where unsetPrevious StartField  = labyResetStartOrTarget labyrinth ( labyStartField labyrinth )
        unsetPrevious TargetField = labyResetStartOrTarget labyrinth ( labyTargetField labyrinth )
        unsetPrevious _           = return []
        setStartOrTarget StartField  = modifyTVar (labyStartField labyrinth) (const $ Just box)
        setStartOrTarget TargetField = modifyTVar (labyTargetField labyrinth) (const $ Just box)
        setStartOrTarget _           = return ()

labyResetStartOrTarget :: Labyrinth -> TVar (Maybe (Int, Int)) -> STM [PointInGridCoordinates Int]
labyResetStartOrTarget labyrinth tvar = do old <- readTVar tvar 
                                           case old of 
                                               Just field -> do modifyTVar tvar (const Nothing)
                                                                writeArray (labyBoxState labyrinth) field Empty
                                                                return [field]
                                               Nothing    -> return []

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

labySetNextAction :: NextAction -> Maybe Labyrinth -> STM (Maybe Labyrinth)
labySetNextAction _ Nothing = return Nothing
labySetNextAction action (Just labyrinth) = 
  do modifyTVar (labyNextAction labyrinth) (const action) 
     return $ Just labyrinth

labyStateToColor :: BoxState -> (Double, Double, Double)
labyStateToColor Empty = (1.0, 1.0, 1.0)
labyStateToColor Border = (0, 0, 1.0)
labyStateToColor StartField = (0.0, 1.0, 0.0)
labyStateToColor TargetField = (1.0, 0.0, 0.0) 

labyClear :: Maybe Labyrinth -> STM (Maybe Labyrinth)
labyClear Nothing = return Nothing
labyClear (Just labyrinth) = 
  do let grid = labyGrid labyrinth
     let Rectangle _ _ lw lh = grLegendRectangle grid
     new <- labyConstruct Nothing
                          (grBoxSize grid)
                          (grBorderSize grid)  
                          (lw, lh)
                          (grScreenSize grid)
     return $ Just new
     
labyIsEmpty :: Labyrinth -> STM Bool
labyIsEmpty labyrinth = 
  do elems <- getElems (labyBoxState labyrinth)
     let listEmpty = null [ e | e <- elems, e /= Empty ]
     return listEmpty

labyFreeze :: Maybe Labyrinth -> STM (Maybe FrozenLabyrinth)
labyFreeze Nothing   = return Nothing
labyFreeze (Just labyrinth) = 
  do array <- freeze (labyBoxState labyrinth)
     startField <- readTVar (labyStartField labyrinth)
     targetField <- readTVar (labyTargetField labyrinth)
     return $ Just FrozenLabyrinth {
        frLabyBoxState = array,
        frLabyGrid = labyGrid labyrinth,
        frLabyNextAction = SetBorder,
        frLabyStartField = startField,
        frLabyTargetField = targetField
    }

labyThaw :: FrozenLabyrinth -> STM Labyrinth
labyThaw labyrinth =
  do array <- thaw (frLabyBoxState labyrinth)
     nextAction <- newTVar (frLabyNextAction labyrinth)
     startField <- newTVar (frLabyStartField labyrinth)
     targetField <- newTVar (frLabyTargetField labyrinth)
     return Labyrinth {
        labyBoxState = array,
        labyGrid = frLabyGrid labyrinth,
        labyNextAction = nextAction,
        labyStartField = startField,
        labyTargetField = targetField
     }
