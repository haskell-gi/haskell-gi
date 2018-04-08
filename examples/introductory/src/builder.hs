{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

{- Packing buttons with GtkBuilder example of GTK+ documentation. For information please refer to README -}

import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import System.Environment (getArgs, getExecutablePath)
import System.FilePath ((</>), dropFileName)

import qualified GI.Gtk as Gtk
import Data.GI.Base

printHello :: Text -> IO ()
printHello t = T.putStrLn $ "Hello from " <> t <> "."

printQuit :: Text -> IO ()
printQuit t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  Gtk.mainQuit
  return ()

getBuilderObj :: forall o'
               . GObject o' 
               => Gtk.Builder 
               -> Text 
               -> (ManagedPtr o' -> o') 
               -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case 
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    T.putStrLn $ "Object named '" <> name <> "' could not be found."
    return Nothing

-- Be aware that this function silently ignores absent names
connectBtnClick :: Gtk.Builder -> Text -> IO () -> IO ()
connectBtnClick builder name handler = getBuilderObj builder name Gtk.Button >>= \case
  Just button -> do 
    on button #clicked $ do handler
    return ()
  Nothing -> return ()

main :: IO ()
main = do
  args <- getArgs
  let targs = map pack args

  Gtk.init $ Just targs

  filename <- case targs of
    [] -> do
      path <- (getExecutablePath >>= return . dropFileName)
      return $ pack $ path </> "builder.ui"
    arg:_ -> return arg
  T.putStrLn $ "filename=\"" <> filename <> "\""

  builder <- new Gtk.Builder []
  #addFromFile builder filename

  Just window <- getBuilderObj builder "window" Gtk.Window
  on window #destroy $ printQuit "windows close button"

  let name = "button1"
  connectBtnClick builder name $ do printHello name
  let name = "button2"
  connectBtnClick builder name $ do printHello name
  connectBtnClick builder "quit" $ printQuit "quit button"

  Gtk.main
