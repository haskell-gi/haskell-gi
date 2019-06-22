{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{- seach-bar of GTK+ examples. For info please see README. -}

-- import qualified Data.Text as Text
-- import           Data.Text (Text)
-- import           Foreign.C.Types
import           Foreign.Ptr (castPtr)

import           Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

toEvent :: Gdk.EventKey -> IO Gdk.Event
toEvent eventKey = withManagedPtr eventKey $ newBoxed Gdk.Event . castPtr

windowKeyPressEventHandler :: Gtk.SearchBar -> Gdk.EventKey -> IO Bool
windowKeyPressEventHandler searchBar eventKey = --do
  toEvent eventKey >>= #handleEvent searchBar

appActivateHandler :: Gtk.Application -> IO ()
appActivateHandler app = do
  window <- new Gtk.ApplicationWindow [ #application := app ]
  -- #show window

  searchBar <- new Gtk.SearchBar []
  #add window searchBar
  -- #show searchBar

  box <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                     , #spacing := 6
                     ] 
  #add searchBar box
  -- #show box

  entry <- new Gtk.Entry [ #hexpand := True ]
  #packStart box entry False False 0
  -- #show entry

  menuButton <- new Gtk.MenuButton []
  #packStart box menuButton False False 0
  -- #show menuButton

  #connectEntry searchBar entry

  on window #keyPressEvent (windowKeyPressEventHandler searchBar)
  #showAll window
  
  return ()

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.examples.search-bar"]
  on app #activate $ do appActivateHandler app

  #run app Nothing        
  return ()                    
