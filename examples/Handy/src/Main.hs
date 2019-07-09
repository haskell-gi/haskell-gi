{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)
import GI.Gtk (AttrOp(..), new, on)
import System.Environment
import System.Exit

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Handy as Hdy

import DemoWindow
import Prelude

startup :: IO ()
startup = pure ()

showWindow :: Gtk.Application -> IO ()
showWindow app = do
    window <- demoWindow app
    #show window

main :: IO ()
main = do
    argv <- getArgs
    -- initialize libhandy
    _ <- Hdy.init (Just $ map pack argv)
    -- set up application
    app <- new Gtk.Application []
    on app #startup startup
    on app #activate (showWindow app)
    -- run
    status <- Gio.applicationRun app (Just argv)
    -- process exit status
    case status of
        0 -> exitSuccess
        n -> exitWith (ExitFailure $ fromIntegral n)
