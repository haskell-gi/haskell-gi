{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{- Basic example of GTK+ documentation. For information please refer to README -}

module Main where
  
-- import Data.Int
-- import qualified Data.Text as T
import Data.Text ()
-- import Debug.Trace (trace, traceIO)
-- import System.Environment (getProgName, getArgs)

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Haskell Gi - Examples - Basic" 
                                 , #defaultWidth := 500
                                 , #defaultHeight := 200
                                 ]
  #showAll w 
  return ()

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.basic"
                             , #flags := [Gio.ApplicationFlagsFlagsNone] -- <-- this flag is the default. no need to pass
                             ]
  on app #activate $ do { activateApp app; return () }
  Gio.applicationRun app Nothing
  return ()

