module MainWindow(CmdOptions(CmdOptions), run) where

import qualified Data.Text as Text
import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.Rendering.Cairo as Cairo
import System.Exit(exitSuccess)
import Control.Monad.Trans(liftIO)
import qualified Control.Concurrent.STM as STM

import Rectangle
import Labyrinth
import Grid

data CmdOptions = CmdOptions 
  { cmdBoxSize :: Int } 

run :: CmdOptions -> IO()
run option = do 
  GTK.initGUI
  window <- GTK.windowNew 
  canvas <- GTK.drawingAreaNew
  state <- STM.atomically $ STM.newTVar Nothing
  GTK.widgetAddEvents canvas [GTK.ButtonPressMask, 
                              GTK.PointerMotionMask, 
                              GTK.PointerMotionHintMask]
  GTK.containerAdd window canvas
  GTK.windowFullscreen window
  GTK.on window GTK.objectDestroy GTK.mainQuit 
  GTK.on window GTK.keyPressEvent ( keyPressHandler state )
  GTK.on canvas GTK.configureEvent ( sizeChangeHandler (cmdBoxSize option) state )
  GTK.on canvas GTK.draw ( drawCanvasHandler state )
  -- GTK.on canvas GTK.motionNotifyEvent ( motionNotifyHandler state )
  GTK.widgetShowAll window
  GTK.mainGUI

keyPressHandler:: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EKey Bool
keyPressHandler _ = GTK.tryEvent $ 
  do
    keyName <- GTK.eventKeyName
    liftIO $
      case Text.unpack keyName of 
        "Escape" -> GTK.mainQuit
  
sizeChangeHandler :: Int -> STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EConfigure Bool
sizeChangeHandler boxSize state =  
  do
    region <- GTK.eventSize
    liftIO $ STM.atomically $ 
      do labyrinth <- labyConstruct boxSize region 
         STM.writeTVar state ( Just labyrinth ) 
    return True 

drawCanvasHandler :: STM.TVar (Maybe Labyrinth) -> Cairo.Render ()
drawCanvasHandler state = 
  do
    extents <- Cairo.clipExtents
    let drawRectangle = rFromBoundingBox round extents
    grid <- liftIO $ getLabyrinthState state ( (rIntersect drawRectangle) . grRectangle . labyGrid )
    case grid of 
      Just (Just intersection, grid) -> drawLabyrinth intersection grid
      _ -> return ()

drawLabyrinth :: Rectangle Int -> Grid Int -> Cairo.Render ()
drawLabyrinth area grid = 
  do
    Cairo.setAntialias Cairo.AntialiasSubpixel
    drawAxes area grid

drawAxes :: Rectangle Int -> Grid Int -> Cairo.Render ()
drawAxes area grid =
  do
    Cairo.save
    Cairo.setSourceRGB 0 0 0
    sequence $ map drawLine (grAxesList area grid)
    Cairo.stroke
    Cairo.restore 

drawLine :: Rectangle Int -> Cairo.Render ()
drawLine rectangle = do let convertTuple (x,y) = (fromIntegral x, fromIntegral y)
                            (x1,y1) = convertTuple (rTopLeftX rectangle, rTopLeftY rectangle)
                            (x2,y2) = convertTuple (rBottomRightX rectangle, rBottomRightY rectangle)   
                        Cairo.moveTo x1 y1
                        Cairo.lineTo x2 y2

getLabyrinthState :: STM.TVar (Maybe Labyrinth) -> ( Labyrinth -> a ) -> IO (Maybe (a, Grid Int))
getLabyrinthState state f = 
  STM.atomically $ 
    do labyrinth <- STM.readTVar state
       return $ maybe Nothing (Just . (\x -> ( f x, labyGrid x))) labyrinth

-- motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EMotion Bool
-- motionNotifyHandler _ = 
--   do 
--     coordinates <- GTK.eventCoordinates
--     modifier <- GTK.eventModifierMouse
--     liftIO ( putStrLn ("Move: " ++ ( show coordinates ) ++ ( show modifier )))
--     GTK.eventRequestMotions
--     return False       
         

