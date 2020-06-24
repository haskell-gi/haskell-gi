{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import Control.Monad (void)
import System.Environment (getArgs, getProgName)

import qualified GI.Gtk as Gtk
import Data.GI.Base

onActivate :: Gtk.Application -> IO ()
onActivate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  adjustment <- new Gtk.Adjustment [#value := 50, #lower := 0, #upper := 100,
                                    #stepIncrement := 1]
  slider <- new Gtk.Scale[#adjustment := adjustment, #drawValue := True]
  #append box slider
  spinButton <- new Gtk.SpinButton [#adjustment := adjustment]
  #append box spinButton

  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Hello",
                                       #child := box]

  #show window

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.Gtk4.test"]
  on app #activate (onActivate app)

  -- If the application does not need to parse command line arguments
  -- just pass Nothing.
  args <- getArgs
  progName <- getProgName
  void $ #run app (Just $ progName : args)
