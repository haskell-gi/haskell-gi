{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{- Packing example of GTK+ documentation. For information please refer to README -}

module Main where

import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base

printHello :: T.Text -> IO ()
printHello s = TIO.putStrLn $ "'Hello, World' from " <> s <> "!"

printQuit :: Gtk.ApplicationWindow -> IO ()
printQuit w = do
  TIO.putStrLn "Program terminated by quit button."
  Gtk.widgetDestroy w
  return ()

createAndAttachBtn :: T.Text -> Gtk.Grid -> ( Int32, Int32, Int32, Int32 ) -> IO () -> IO ()
createAndAttachBtn lbl grid ( col, row, cspan, rspan ) callback = do
  btn <- new Gtk.Button [ #label := lbl ]
  on btn #clicked callback
  #attach grid btn col row cspan rspan

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Haskell Gi - Examples - Packed Widgets"
                                 , #borderWidth := 10
                                 ]

  grid <- new Gtk.Grid [ #columnSpacing := 5
                       , #rowSpacing := 5 
                       ]
  #add w grid

  let lbl1 = "Button 1"
  createAndAttachBtn lbl1 grid ( 0, 0, 1, 1 ) $ do printHello lbl1

  let lbl2 = "Button 2"
  createAndAttachBtn lbl2 grid ( 1, 0, 1, 1 ) $ do printHello lbl2

  createAndAttachBtn "Quit" grid ( 0, 1, 2, 1 ) $ do printQuit w

  #showAll w

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.examples.packed-widgets"]
  on app #activate $ activateApp app
  Gio.applicationRun app Nothing

  return ()

  
