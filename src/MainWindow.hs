module MainWindow(CmdOptions(CmdOptions), run) where

import Data.Text
import Graphics.UI.Gtk
import System.Exit(exitSuccess)
import Control.Monad
import Control.Monad.Trans

data CmdOptions = CmdOptions 
  { cmdBoxSize :: Int } 

run :: CmdOptions -> IO()
run option = do 
  initGUI
  window <- windowNew
  windowFullscreen window
  on window objectDestroy mainQuit 
  on window keyPressEvent keyPressHandler 
  widgetShowAll window
  mainGUI

keyPressHandler:: EventM EKey Bool
keyPressHandler = tryEvent $ 
  do let escText :: IO Text
         escText = return $ pack "Escape"
     escText <- eventKeyName
     liftIO mainQuit 
  
          


