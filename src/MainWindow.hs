module MainWindow(CmdOptions(CmdOptions), run) where

import Data.Text
import Graphics.UI.Gtk
import System.Exit(exitSuccess)
import Control.Monad
import Control.Monad.Trans
import Labyrinth

data CmdOptions = CmdOptions 
  { cmdBoxSize :: Int } 

run :: CmdOptions -> IO()
run option = do 
  initGUI
  window <- windowNew 
  canvas <- drawingAreaNew
  widgetAddEvents canvas [ButtonPressMask, 
                          PointerMotionMask, 
                          PointerMotionHintMask]
  containerAdd window canvas
  windowFullscreen window
  on window objectDestroy mainQuit 
  on window keyPressEvent keyPressHandler 
  on canvas configureEvent sizeChangeHandler
  on canvas exposeEvent drawCanvasHandler
  on canvas motionNotifyEvent motionNotifyHandler
  widgetShowAll window
  mainGUI

keyPressHandler:: EventM EKey Bool
keyPressHandler = tryEvent $ 
  do
    keyName <- eventKeyName
    liftIO $
      case unpack keyName of 
        "Escape" -> mainQuit
  
 
sizeChangeHandler :: EventM EConfigure Bool
sizeChangeHandler =  
  do
    region <- eventSize
    liftIO ( putStrLn ("SizeChange: " ++ (show region)))
    return True 

drawCanvasHandler :: EventM EExpose Bool
drawCanvasHandler = return True  

motionNotifyHandler :: EventM EMotion Bool
motionNotifyHandler = 
  do 
    coordinates <- eventCoordinates
    modifier <- eventModifierMouse
    liftIO ( putStrLn ("Move: " ++ ( show coordinates ) ++ ( show modifier )))
    eventRequestMotions
    return False
          


