{-# LANGUAGE OverloadedStrings #-}
{-# CFILES hsgclosure.c #-}
import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib

import qualified Data.ByteString.Char8 as B

import Control.Monad (forM_, when)

import System.Environment (getProgName)

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
  contents <- GLib.fileGetContents "testGtk.hs"
  B.putStrLn contents

  -- Trying to read a file that does not exist should throw
  -- FileErrorNoent, in the FileError domain.
  _ <- GLib.catchFileError (GLib.fileGetContents "this file does not exist") $
       \code msg ->
           case code of
             GLib.FileErrorNoent -> do
                       putStrLn "<< Exception handled >>"
                       return ""
             _ -> error $ "Unexpected error code : \"" ++ show code ++
                            "\" with message : \"" ++ msg ++ "\""

  return ()

testNullableArgs = do
  uri <- GLib.filenameToUri "/usr/lib/test" Nothing
  when (uri /= "file:///usr/lib/test") $
       error $ "First nullable test failed : " ++ uri

  uri' <- GLib.filenameToUri "/usr/lib/test" (Just "gnome.org")
  when (uri' /= "file://gnome.org/usr/lib/test") $
       error $ "Second nullable test failed : " ++ uri'

testOutArgs = iconThemeGetDefault >>= iconThemeGetSearchPath >>= print

main = do
        -- Generally one should do the following to init Gtk:
        -- import System.Environment (getArgs, getProgName)
        -- ...
        -- args <- getArgs
        -- progName <- getProgName
        -- restArgs <- Gtk.init $ progName:args
        --
        -- Here we use synthetic arguments to test that we are
        -- handling InOut arguments properly.
        progName <- getProgName
	restArgs <- Gtk.init $ progName:["--g-fatal-warnings"]
        when (restArgs /= [progName]) $
             error $ "gtk_init did not process --g-fatal-warnings"

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
        testNullableArgs
        testOutArgs

	widgetShowAll win
	Gtk.main
