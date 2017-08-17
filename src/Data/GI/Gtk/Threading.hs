-- -*-haskell-*-
--
-- Author : Brandon Sloane
--
-- Created: 16 August 2017
--
-- Copyright (C) 2017 Brandon Sloane
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Utility functions for threading
--
module Data.GI.Gtk.Threading
    (
-- | Utility functions to run IO actions on the GUI thread. You must call
-- 'Data.GI.Gtk.Threading.setGUIThread' or 'Data.GI.Gtk.Threading.setCurrentThreadAsGUIThread'
-- before using the synchronous options, or you risk deadlocking.
      setGUIThread
    , setCurrentThreadAsGUIThread
    , postGUISyncWithPriority
    , postGUISync
    , postGUIASyncWithPriority
    , postGUIASync
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Int (Int32)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (stderr, hPutStrLn)
import GI.Gdk (threadsAddIdle)
import GI.GLib.Constants


guiThread :: MVar (Maybe ThreadId)
{-# NOINLINE guiThread #-}
guiThread = unsafePerformIO $ newMVar Nothing

-- | Inform gi-gtk-hs what thread the GTK eventloop is running in.
setGUIThread :: ThreadId -> IO ()
setGUIThread t = swapMVar guiThread (Just t) >> return ()

-- | Inform gi-gtk-hs that the gtk mainloop is running in the current thread.
--
-- Equivalent to @'Control.Concurrent.myThreadId' >>= 'Data.GI.Gtk.Threading.setGUIThread'@
setCurrentThreadAsGUIThread = myThreadId >>= setGUIThread

-- | Queue an action to be run in the GTK event loop.
-- If called from the same process as the event loop, this runs the action directly.
-- Otherwise, this queues it in GTK's event loop and blocks until the action is complete
--
-- You must call 'Data.GI.Gtk.Threading.setGUIThread' or 'Data.GI.Gtk.Threading.setCurrentThreadAsGUIThread' before this.
--
-- Priority is typically between 'GI.GLib.Constants.PRIORITY_HIGH_IDLE' (100) and 'GI.GLib.Constants.PRIORITY_DEFAULT_IDLE' (200)
postGUISyncWithPriority :: Int32 -> IO a -> IO a
postGUISyncWithPriority priority action = runInBoundThread $ do
    guiThreadId <- readMVar guiThread
    case guiThreadId of
        Nothing -> do
            hPutStrLn stderr "WARNING Data.GI.Gtk.Threading Calling postGUISyncWithPriority before setGUIThread"
            run
        Just t -> do
            currentThread <- myThreadId
            (currentCapability,_) <- threadCapability currentThread
            (guiCapability,_) <- threadCapability t
            if currentCapability == guiCapability then
                action
            else
                run
    where
    run = do
        ans <- newEmptyMVar
        threadsAddIdle priority $ action >>= putMVar ans >> return False
        takeMVar ans

-- | Queue an action to be run in the GTK event loop.
-- If called from the same process as the event loop, this runs the action directly.
-- Otherwise, this queues it in GTK's event loop and blocks until the action is complete
--
-- You must call 'Data.GI.Gtk.Threading.setGUIThread' or 'Data.GI.Gtk.Threading.setCurrentThreadAsGUIThread' before this.
--
-- Equivalent to @'Data.GI.Gtk.Threading.postGUISyncWithPriority' 'GI.GLib.Constants.PRIORITY_DEFAULT_IDLE'@
postGUISync :: IO a -> IO a
postGUISync = postGUISyncWithPriority PRIORITY_DEFAULT_IDLE

-- | Queue an action to be run in the GTK event loop.
-- This function queues the event regardless of what process it is called from, and returns immidietly.
--
-- Priority is typically between 'GI.GLib.Constants.PRIORITY_HIGH_IDLE' (100) and 'GI.GLib.Constants.PRIORITY_DEFAULT_IDLE' (200)
postGUIASyncWithPriority :: Int32 -> IO () -> IO ()
postGUIASyncWithPriority priority action = threadsAddIdle priority (action >> return False) >> return ()

-- | Queue an action to be run in the GTK event loop.
-- This function queues the event regardless of what process it is called from, and returns immidietly.
--
-- Equivalent to @'Data.GI.Gtk.Threading.postGUIASyncWithPriority' 'GI.GLib.Constants.PRIORITY_DEFAULT_IDLE'@
postGUIASync :: IO () -> IO ()
postGUIASync = postGUIASyncWithPriority PRIORITY_DEFAULT_IDLE
