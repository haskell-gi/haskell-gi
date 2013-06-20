{-# LANGUAGE OverloadedStrings #-}
{-# CFILES hsgclosure.c #-}
import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib

import GI.Utils.Attributes
import GI.Utils.Properties (new)

import qualified Data.ByteString.Char8 as B
import Data.Word

import Control.Monad (forM_, when)

import System.Environment (getProgName)
import System.Random (randomRIO)

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

testBoxed = do
  forM_ [1..500] $ \_ -> do
    r <- randomRIO (0,6)
    let expected = toEnum $ (fromEnum GLib.DateWeekdayMonday) + r
    date <- GLib.dateNewDmy (fromIntegral $ 17 + r) GLib.DateMonthJune 2013
    weekday <- GLib.dateGetWeekday date
    when (weekday /= expected) $
         error $ show r ++ " -> Got wrong weekday! : " ++ show weekday

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

        grid <- new Grid [orientableOrientation := OrientationVertical]
        containerAdd win grid

        label <- new Label [labelLabel := "Test"]
        widgetShow label
        label <!> onLabelActivateLink $ \uri -> do
                      putStrLn $ uri ++ " clicked."
                      return True -- Link processed, do not open with
                                  -- the browser
        containerAdd grid label

        button <- new Button [buttonLabel := "Click me!",
                              buttonRelief := ReliefStyleNone]
        button <!> onButtonClicked $ do
                set label [labelLabel := "This is <a href=\"http://www.gnome.org\">a test</a>",
                           labelUseMarkup := True ]
                -- set button [widgetSensitive := False, ...] would be
                -- more natural, but this serves as a test of
                -- attribute updating functions.
                set button [widgetSensitive :~ not,
                            buttonLabel := "Thanks for clicking!"]
                sensitive <- get button widgetSensitive
                label <- get button buttonLabel
                putStrLn $ "New button text is "
                             ++ show label
                             ++ " and sensitive is " ++ show sensitive
        containerAdd grid button

        testGio
        testExceptions
        testNullableArgs
        testOutArgs
        testBoxed

	widgetShowAll win
	Gtk.main
