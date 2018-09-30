{-# LANGUAGE OverloadedLabels, OverloadedStrings, CPP #-}

module Main where

import           System.Exit  (die)
import qualified GI.Gtk       as Gtk
import           Data.GI.Base (new, on, AttrOp((:=)), unsafeCastTo)

#if !MIN_VERSION_base(4,10,0)
import Data.Monoid ((<>))
#endif

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "com.example.i18n" ]

  on app #activate (goExample app)
  #run app Nothing

  return ()

goExample :: Gtk.Application -> IO ()
goExample app = do
  window <- loadWindow

  #addWindow app window
  #showAll window

loadWindow :: IO Gtk.ApplicationWindow
loadWindow = do
  builder <- new Gtk.Builder []

  #addFromFile builder "ui/i18n.ui"
  mWindow <- #getObject builder "window"

  maybe showError (unsafeCastTo Gtk.ApplicationWindow) mWindow

  where
    showError = die ("Something seems to have gone wrong loading the window.\n"
                     <> "Ending the example...")
