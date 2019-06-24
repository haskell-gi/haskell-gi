{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{- Hello World example of GTK+ documentation. For information please refer to README -}

module Main where

-- import qualified Data.Text as T
import Data.Text ()

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base

printHello :: IO ()
printHello = putStrLn "Hello, World!"

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Haskell Gi - Examples - Hello World"
                                 , #defaultHeight := 200
                                 , #defaultWidth := 200
                                 ]

  bbox <- new Gtk.ButtonBox [ #orientation := Gtk.OrientationHorizontal ]
  #add w bbox
  
  btn <- new Gtk.Button [ #label := "Hello World!"]
  on btn #clicked printHello
  on btn #clicked $ Gtk.widgetDestroy w
  #add bbox btn
  
  #showAll w
  return ()

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.hello-world"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  on app #activate $ activateApp app
  Gio.applicationRun app Nothing
  return ()
