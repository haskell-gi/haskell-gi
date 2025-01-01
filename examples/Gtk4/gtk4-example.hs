{-# LANGUAGE OverloadedStrings, OverloadedLabels, ImplicitParams,
PackageImports #-}

import Control.Monad (void)
import System.Environment (getArgs, getProgName)

import Data.Int (Int32)

import qualified GI.Gtk as Gtk
-- We import Application explicitly to test imports from the
-- backwards-compatibility wrapper gi-gtk. In general it would be
-- simpler to use Gtk.Application directly.
import "gi-gtk" GI.Gtk.Objects.Application (Application(..))
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

activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  adjustment <- new Gtk.Adjustment [#value := 50, #lower := 0, #upper := 100,
                                    #stepIncrement := 1]
  slider <- new Gtk.Scale[#adjustment := adjustment, #drawValue := True]
  #append box slider
  spinButton <- new Gtk.SpinButton [#adjustment := adjustment]
  #append box spinButton

  controller <- new Gtk.GestureClick [After #pressed pressedCB]
  #addController slider controller

  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Hello",
                                       #child := box]
  #show window

main :: IO ()
main = do
  app <- new Application [#applicationId := "haskell-gi.Gtk4.test",
                          On #activate (activate ?self)]

  -- If the application does not need to parse command line arguments
  -- just pass Nothing.
  args <- getArgs
  progName <- getProgName
  void $ #run app (Just $ progName : args)

