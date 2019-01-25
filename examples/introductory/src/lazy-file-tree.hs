-- An example by Ömer Sinan Ağacan <omeragacan@gmail.com>
--
-- A file tree renderer that lazily load directory contents when the entry for
-- the directory is expanded.

{-# LANGUAGE LambdaCase, OverloadedLabels, OverloadedStrings #-}

module Main where

import Control.Monad
import Data.IORef
import qualified Data.Text as T
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath

import Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

main :: IO ()
main =
    getArgs >>= \case
      [] -> getCurrentDirectory >>= run
      [root] -> run root
      _ -> putStrLn "USAGE: file-tree [<root>]" >> exitFailure

run :: FilePath -> IO ()
run root = do
    app <- new Gtk.Application
      [ #applicationId := "haskell-gi.examples.file-tree"
      , #flags := [ Gio.ApplicationFlagsFlagsNone ]
      ]
    void (on app #activate (activateApp app root))
    void (Gio.applicationRun app Nothing)

activateApp :: Gtk.Application -> FilePath -> IO ()
activateApp app root = do
    w <- new Gtk.ApplicationWindow
      [ #application := app
      , #title := "Haskell Gi - Examples - File Tree"
      , #defaultHeight := 200
      , #defaultWidth := 200
      ]

    scrollable <- new Gtk.ScrolledWindow []

    root_list_box <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone ]
    fs <- dirContents root
    forM_ fs $ \f -> mkFileRow root f >>= #add root_list_box

    #add scrollable root_list_box
    #add w scrollable

    #showAll w

-- | Given a parent dir `parent` and a file name `file`, return a list row for
-- `parent/file`. The row will be another expander if the file is a directory.
-- Otherwise it'll just be a label.
mkFileRow
    :: FilePath -- ^ parent
    -> FilePath -- ^ file
    -> IO Gtk.ListBoxRow
mkFileRow parent file = do
    row <- new Gtk.ListBoxRow []
    list_box0 <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone ]
    #add row list_box0

    let
      full_path = parent </> file

      -- Current file is a directory, add an expander for it. The expander will
      -- lazily (when expanded for the first time) read the directory contents
      -- and add rows for child files/directories.
      add_dir_expander :: IO ()
      add_dir_expander = do
        -- Did we create the child node for expansion yet?
        child_created_ref <- newIORef False

        expander <- new Gtk.Expander [ #label := T.pack file ]
        #add list_box0 expander

        void $ on expander #activate $ do
          child_created <- readIORef child_created_ref
          unless child_created $ do
            list_box <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #marginLeft := 10 ]

            dir_contents <- dirContents full_path
            forM_ dir_contents $ \f -> do
              w <- mkFileRow full_path f
              #add list_box w
            #add expander list_box
            #showAll expander

            writeIORef child_created_ref True

      -- Current file is not a directory, add a label for it.
      add_file_label :: IO ()
      add_file_label = do
        lbl <- new Gtk.Label [ #label := T.pack file, #halign := Gtk.AlignStart, #marginLeft := 20 ]
        void (#add list_box0 lbl)

    is_dir <- doesDirectoryExist full_path
    if is_dir then add_dir_expander else add_file_label

    return row

--------------------------------------------------------------------------------
-- * Utilities

dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (\f -> f /= "." && f /= "..")) . getDirectoryContents
