module MainWindow(CmdOptions(CmdOptions), run) where


import Control.Monad.Trans(liftIO)
import Control.Arrow((&&&))
import System.Exit(exitSuccess)

import qualified Data.Text as Text
import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Control.Concurrent.STM as STM

import Rectangle
import Labyrinth
import Grid

newtype CmdOptions = CmdOptions 
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
  GTK.on canvas GTK.buttonPressEvent ( buttonPressHandler state )
  GTK.on canvas GTK.motionNotifyEvent ( motionNotifyHandler state )
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
    grid <- liftIO $ getLabyrinthState state ( rIntersect drawRectangle . grRectangle . labyGrid )
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
    mapM_ drawLine (grAxesList area grid)
    Cairo.stroke
    Cairo.restore 

drawLine :: Rectangle Int -> Cairo.Render ()
drawLine rectangle = do let (x1, y1, x2, y2) = rToBoundingBox fromIntegral rectangle
                        Cairo.moveTo x1 y1
                        Cairo.lineTo x2 y2

getLabyrinthState :: STM.TVar (Maybe Labyrinth) -> ( Labyrinth -> a ) -> IO (Maybe (a, Grid Int))
getLabyrinthState state f = 
  STM.atomically $ 
    do labyrinth <- STM.readTVar state
       return $ fmap (f &&& labyGrid) labyrinth

buttonPressHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EButton Bool
buttonPressHandler state = 
  GTK.tryEvent $ 
    do
      button <- GTK.eventButton
      coordinates <- GTK.eventCoordinates
      case button of 
        GTK.LeftButton -> liftIO $ handleMarkBox state coordinates Border
        GTK.RightButton -> liftIO $ handleMarkBox state coordinates Empty
      return ()

motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EMotion Bool
motionNotifyHandler _ = return True
--   do 
--     coordinates <- GTK.eventCoordinates
--     modifier <- GTK.eventModifierMouse
--     liftIO ( putStrLn ("Move: " ++ ( show coordinates ) ++ ( show modifier )))
--     GTK.eventRequestMotions
--     return False       
         
handleMarkBox :: STM.TVar (Maybe Labyrinth) -> (Double, Double) -> BoxState -> IO ()
handleMarkBox state (x,y) boxValue = 
  let point = PtScreen { grPtScreen = Point ( round x, round y ) }
  in STM.atomically $ 
    do labyrinth <- STM.readTVar state 
       STM.writeTVar state  =<< labyMarkBox point boxValue labyrinth
       
