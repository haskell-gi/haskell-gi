{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main = do
  Gtk.init
  window <- new Gtk.Window [#title := "Hello"]
  on window #destroy Gtk.mainQuit
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add window box
  adjustment <- new Gtk.Adjustment [#value := 50, #lower := 0, #upper := 100,
                                    #stepIncrement := 1]
  slider <- new Gtk.Scale[#adjustment := adjustment, #drawValue := True]
  #add box slider
  spinButton <- new Gtk.SpinButton [#adjustment := adjustment]
  #add box spinButton

  #show window
  Gtk.main
