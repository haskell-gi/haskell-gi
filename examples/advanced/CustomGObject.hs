{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import Data.GI.Base
import qualified GI.Gtk as Gtk

import Data.GI.Base.GObject (gobjectGetPrivateData, gobjectSetPrivateData)
import CustomButton (CustomButton(..))

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Custom GObject example"]

  _ <- on win #destroy Gtk.mainQuit

  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add win vbox

  button <- new CustomButton [#label := "Check private value"]
  _ <- on button #clicked $ do
    priv <- gobjectGetPrivateData button
    print priv
    gobjectSetPrivateData button (priv+1)

  #add vbox button

  #showAll win

  Gtk.main
