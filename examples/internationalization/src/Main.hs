module Main where

import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           GI.Gtk hiding (main)
import           GI.Gio hiding (Application, applicationNew)

main :: IO ()
main = do
  mApp <- applicationNew (Just "com.example.i18n")
                         [ApplicationFlagsFlagsNone]
  maybe (return ()) runApplication mApp
 where runApplication app = do
           onApplicationActivate app (goExample app)
           applicationRun app (Just ["i18n"])
           return ()


goExample :: Application -> IO ()
goExample app = do
  mWindow <- loadWindow
  maybe showError showWindow mWindow
 where showError = do 
           putStrLn "Something seems to have gone wrong loading the window"
           putStrLn "Ending the example..."
       showWindow window = do
           applicationAddWindow app window
           widgetShowAll window


loadWindow :: IO (Maybe ApplicationWindow)
loadWindow = do
  builder <- builderNew
  builderAddFromFile builder "ui/i18n.ui"
  --builderSetTranslationDomain builder (Just "i18n")
  mWindow <- builderGetObject builder "window"
  maybe (return Nothing) (castTo ApplicationWindow) mWindow
