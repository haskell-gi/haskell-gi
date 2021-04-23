{-# LANGUAGE OverloadedStrings, OverloadedLabels, ImplicitParams #-}

import Control.Monad (void)
import System.Environment (getArgs, getProgName)

import Data.Int (Int32)

import qualified GI.Gtk as Gtk
import Data.GI.Base

-- | An example of a signal callback accessing the ?self parameter
-- (that is, the object raising the callback). See
-- https://github.com/haskell-gi/haskell-gi/issues/346
-- for why this is necessary when dealing with even controllers in gtk4.
pressedCB :: (?self :: Gtk.GestureClick) => Int32 -> Double -> Double -> IO ()
pressedCB nPress x y = do
    button <- #getCurrentButton ?self
    putStrLn $ "Button pressed: " <> show nPress <> " "
      <> show x <> " " <> show y <> " button: " <> show button

onActivate :: Gtk.Application -> IO ()
onActivate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  adjustment <- new Gtk.Adjustment [#value := 50, #lower := 0, #upper := 100,
                                    #stepIncrement := 1]
  slider <- new Gtk.Scale[#adjustment := adjustment, #drawValue := True]
  #append box slider
  spinButton <- new Gtk.SpinButton [#adjustment := adjustment]
  #append box spinButton

  controller <- new Gtk.GestureClick []
  after controller #pressed pressedCB
  #addController slider controller

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
