{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import Data.GI.Base
import qualified GI.Gtk as Gtk

import Data.GI.Base.GObject (gobjectGetPrivateData, gobjectSetPrivateData)
import CustomContainer (CustomContainer(..))

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Custom GObject example"]

  _ <- on win #destroy Gtk.mainQuit

  container <- new CustomContainer []
  #add win container

  button <- new Gtk.Button [#label := "Check private value"]
  _ <- on button #clicked $ do
    priv <- gobjectGetPrivateData container
    print $ length priv
    #remove container button

  #add container button

  #showAll win

  Gtk.main
