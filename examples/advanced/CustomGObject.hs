{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import Control.Monad (forM_, void)
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

import CustomContainer (CustomContainer(..))

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Custom GObject example"]

  _ <- on win #destroy Gtk.mainQuit

  container <- new CustomContainer [ #numColumns := Just 4 ]
  #add win container

  forM_ [1..7 :: Int] $ \n -> do
    button <- new Gtk.Button [#label := T.pack ("Button " <> show n)]
    void $ on button #clicked (#remove container button)
    #add container button

  #showAll win

  Gtk.main
