module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Graphics.UI.Gtk
import System.Glib.GError

import GI.API

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    [name] <- getArgs
    apis <- loadAPI name
    forM_ apis $ \api -> print api
