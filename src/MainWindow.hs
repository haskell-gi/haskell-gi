module MainWindow(CmdOptions(CmdOptions), run) where

import Data.Text
import Graphics.UI.Gtk
import System.Exit(exitSuccess)
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Labyrinth

data CmdOptions = CmdOptions 
  { cmdBoxSize :: Int } 

run :: CmdOptions -> IO()
run option = do 
  initGUI
  window <- windowNew 
  canvas <- drawingAreaNew
  state <- createStateVar
  widgetAddEvents canvas [ButtonPressMask, 
                          PointerMotionMask, 
                          PointerMotionHintMask]
  containerAdd window canvas
  windowFullscreen window
  on window objectDestroy mainQuit 
  on window keyPressEvent ( keyPressHandler state )
  on canvas configureEvent ( sizeChangeHandler (cmdBoxSize option) state )
  on canvas exposeEvent ( drawCanvasHandler state )
  on canvas motionNotifyEvent ( motionNotifyHandler state )
  widgetShowAll window
  mainGUI

createStateVar :: IO (TVar (Maybe Labyrinth))
createStateVar = atomically (newTVar Nothing)

keyPressHandler:: TVar (Maybe Labyrinth) -> EventM EKey Bool
keyPressHandler _ = tryEvent $ 
  do
    keyName <- eventKeyName
    liftIO $
      case unpack keyName of 
        "Escape" -> mainQuit
  
 
sizeChangeHandler :: Int -> TVar (Maybe Labyrinth) -> EventM EConfigure Bool
sizeChangeHandler boxSize state =  
  do
    region <- eventSize
    liftIO $ atomically $ 
      do labyrinth <- labyConstruct boxSize region 
         writeTVar state ( Just labyrinth ) 
    return True 

drawCanvasHandler :: TVar (Maybe Labyrinth) -> EventM EExpose Bool
drawCanvasHandler _ = return True  

motionNotifyHandler :: TVar (Maybe Labyrinth) -> EventM EMotion Bool
motionNotifyHandler _ = 
  do 
    coordinates <- eventCoordinates
    modifier <- eventModifierMouse
    liftIO ( putStrLn ("Move: " ++ ( show coordinates ) ++ ( show modifier )))
    eventRequestMotions
    return False
