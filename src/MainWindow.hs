module MainWindow(CmdOptions(CmdOptions), run) where

import qualified Data.Text as Text
import qualified Graphics.UI.Gtk as GTK
import System.Exit(exitSuccess)
import Control.Monad.Trans(liftIO)
import qualified Control.Concurrent.STM as STM
import Rectangle
import Labyrinth

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
  GTK.on canvas GTK.exposeEvent ( drawCanvasHandler state )
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

drawCanvasHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EExpose Bool
drawCanvasHandler state = 
  do 
    GTK.Rectangle x y width height <- GTK.eventArea 
    let rectangle = Rectangle { rTopLeftX = x,
                                rTopLeftY = y,
                                rWidth = width,
                                rHeight = height }
    return True  

motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EMotion Bool
motionNotifyHandler _ = 
  do 
    coordinates <- GTK.eventCoordinates
    modifier <- GTK.eventModifierMouse
    liftIO ( putStrLn ("Move: " ++ ( show coordinates ) ++ ( show modifier )))
    GTK.eventRequestMotions
    return False
