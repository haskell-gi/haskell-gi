{-# LANGUAGE OverloadedStrings #-}
{-# CFILES hsgclosure.c #-}
import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as B

import Control.Monad (forM_)

import Foreign (nullPtr)

-- A fancy notation for making signal connections easier to read.
(<!>) obj cb = cb obj

testGio :: IO ()
testGio = do
  infos <- Gio.appInfoGetAll
  forM_ infos $ \info -> do
            name <- Gio.appInfoGetName info
            exe <- Gio.appInfoGetExecutable info
            putStrLn $ "name: " ++ name
            putStrLn $ "exe: " ++ exe
            putStrLn ""

testExceptions :: IO ()
testExceptions = do
  -- This should work fine, without emitting any exception
  (_,contents) <- GLib.fileGetContents "testGtk.hs"
  B.putStrLn contents

  -- Trying to read a file that does not exist should throw
  -- FileErrorNoent, in the FileError domain.
  _ <- GLib.catchFileError (GLib.fileGetContents "this file does not exist") $
       \code msg ->
           case code of
             GLib.FileErrorNoent -> do
                       putStrLn "<< Exception handled >>"
                       return $ (True, "")
             _ -> error $ "Unexpected error code : \"" ++ show code ++
                            "\" with message : \"" ++ msg ++ "\""

  return ()

main = do
	gtk_init nullPtr nullPtr

	win <- windowNew WindowTypeToplevel
        win <!> onWidgetDestroy $ do
                  putStrLn "Closing the program"
                  mainQuit

        grid <- gridNew
        orientableSetOrientation grid OrientationVertical
        containerAdd win grid

        label <- labelNew "Test"
        widgetShow label
        label <!> onLabelActivateLink $ \uri -> do
                      putStrLn $ uri ++ " clicked."
                      return True -- Link processed, do not open with
                                  -- the browser
        containerAdd grid label

        button <- buttonNewWithLabel "Click me!"
        button <!> onButtonClicked $
                labelSetMarkup label "This is <a href=\"http://www.gnome.org\">a test</a>"
        containerAdd grid button

        testGio
        testExceptions

	widgetShowAll win
	Gtk.main
