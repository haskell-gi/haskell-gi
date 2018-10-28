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
--
-- Note that the notion of "Thread" used by this module corresponds to operating system threads, not Haskell threads.
-- A single operating system thread may run multiple Haskell threads, and a Haskell thread may migrate between operating system threads.
-- In order for this nothing of "Thread" to make sense to a Haskell program, we must be working in a bound Haskell thread, which is tied to a single operating system thread.
-- Haskell's main function is automatically bound, and the postGUI functions will create a new bound thread if nessasary.
      setGUIThread
    , getGUIThread
    , setCurrentThreadAsGUIThread
    , postGUISyncWithPriority
    , postGUISync
    , postGUIASyncWithPriority
    , postGUIASync
    , compareThreads
    , isGUIThread
    , module GI.GLib --threadSelf and Thread
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Int (Int32)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (stderr, hPutStrLn)
import GI.Gdk (threadsAddIdle)
import GI.GLib.Constants
import GI.GLib (threadSelf, Thread(..))
import Data.GI.Base.ManagedPtr


guiThread :: MVar (Maybe Thread)
{-# NOINLINE guiThread #-}
guiThread = unsafePerformIO $ newMVar Nothing

-- | Inform gi-gtk-hs what thread is running the gtk main loop.
setGUIThread :: Thread -> IO ()
setGUIThread t = swapMVar guiThread (Just t) >> return ()

-- | Inform gi-gtk-hs that the current thread is, or will be, running the gtk main loop.
--
-- Equivalent to @'GI.GLib.threadSelf' >>= 'Data.GI.Gtk.Threading.setGUIThread'@
setCurrentThreadAsGUIThread :: IO ()
setCurrentThreadAsGUIThread = threadSelf >>= setGUIThread

-- | Get the Thread that is running the Gtk main loop, if it has been set.
getGUIThread :: IO (Maybe Thread)
getGUIThread = readMVar guiThread

-- | Queue an action to be run in the GTK event loop.
-- If called from the same process as the event loop, this runs the action directly.
-- Otherwise, this queues it in GTK's event loop and blocks until the action is complete
--
-- You must call 'Data.GI.Gtk.Threading.setGUIThread' or 'Data.GI.Gtk.Threading.setCurrentThreadAsGUIThread' before this.
--
-- Priority is typically between 'GI.GLib.Constants.PRIORITY_HIGH_IDLE' (100) and 'GI.GLib.Constants.PRIORITY_DEFAULT_IDLE' (200)
postGUISyncWithPriority :: Int32 -> IO a -> IO a
postGUISyncWithPriority priority action = runInBoundThread $ do
    b <- isGUIThread
    if b then
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

-- | Test if two 'GI.GLib.Structs.Thread.Thread's refer to the same OS thread.
-- A 'GI.GLib.Structs.Thread.Thread' can be gotten from 'GI.GLib.Structs.Thread.threadSelf'.
-- Note that 'GI.GLib.Structs.Thread.threadSelf' only makes sense from a bound thread.
compareThreads :: Thread -> Thread -> IO Bool
compareThreads (Thread mptr1) (Thread mptr2) =
    withManagedPtr mptr1 $ \ptr1 ->
    withManagedPtr mptr2 $ \ptr2 ->
    return $ ptr1 == ptr2

-- | Check if the current thread is the Gtk gui thread.
--
-- You must call 'Data.GI.Gtk.Threading.setGUIThread' or 'Data.GI.Gtk.Threading.setCurrentThreadAsGUIThread' before this.
-- This only makes sense when called from a bound thread.
isGUIThread :: IO Bool
isGUIThread = do
    guiThread <- getGUIThread
    case guiThread of
        Nothing -> hPutStrLn stderr "WARNING Data.GI.Gtk.Threading Calling isGUIThread before setGUIThread" >> return False
        Just t1 -> threadSelf >>= compareThreads t1

