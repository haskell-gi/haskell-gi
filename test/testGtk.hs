{-# LANGUAGE OverloadedStrings #-}
import GI.Gtk hiding (main)
import GI.GtkLenses
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GioLenses as LGio
import qualified GI.GLib as GLib

import GI.Utils.Attributes
import GI.Utils.Properties (new)

import qualified Data.ByteString.Char8 as B

import Control.Monad (forM_, when)

import System.Environment (getProgName)
import System.Random (randomRIO)

-- A fancy notation for reverse application, making signal connections
-- a bit easier to read.
(<!>) :: a -> (a -> b) -> b
(<!>) obj cb = cb obj

testGio :: IO ()
testGio = do
  putStrLn "*** Gio test"

  infos <- Gio.appInfoGetAll
  forM_ infos $ \info -> do
            name <- Gio.appInfoGetName info
            exe <- Gio.appInfoGetExecutable info
            putStrLn $ "name: " ++ name
            putStrLn $ "exe: " ++ exe
            putStrLn ""

  putStrLn "+++ Gio test done"

testExceptions :: IO ()
testExceptions = do
  putStrLn "*** Exception test"
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

  putStrLn "+++ Exception test done"

testNullableArgs :: IO ()
testNullableArgs = do
  putStrLn "*** Nullable args test"
  uri <- GLib.filenameToUri "/usr/lib/test" Nothing
  when (uri /= "file:///usr/lib/test") $
       error $ "First nullable test failed : " ++ uri

  uri' <- GLib.filenameToUri "/usr/lib/test" (Just "gnome.org")
  when (uri' /= "file://gnome.org/usr/lib/test") $
       error $ "Second nullable test failed : " ++ uri'
  putStrLn "+++ Nullable args test done"

testOutArgs :: IO ()
testOutArgs = do
  putStrLn "*** Out args test"
  iconThemeGetDefault >>= iconThemeGetSearchPath >>= print
  putStrLn "+++ Out args test done"

testBoxed :: IO ()
testBoxed = do
  putStrLn "*** Boxed test"
  forM_ [1..500::Int] $ \_ -> do
    r <- randomRIO (0,6)
    let expected = toEnum $ r + fromEnum GLib.DateWeekdayMonday
    date <- GLib.dateNewDmy (fromIntegral $ 17 + r) GLib.DateMonthJune 2013
    weekday <- GLib.dateGetWeekday date
    when (weekday /= expected) $
         error $ show r ++ " -> Got wrong weekday! : " ++ show weekday
  putStrLn "+++ Boxed test done"

testImportedLenses :: IO ()
testImportedLenses = do
  putStrLn "*** Imported lenses test"
  address <- Gio.inetAddressNewFromString "173.194.40.51"
  print =<< address `get` LGio._family
  putStrLn "+++ Imported lenses test done"

testPolymorphicLenses :: Window -> String -> IO ()
testPolymorphicLenses parent message = do
  putStrLn "*** Polymorphic lenses test"
  messageBox <- new MessageDialog
                [ _buttons := ButtonsTypeYesNo, -- ConstructOnly
                  _text := message,
                  _title := "Important message",
                  _transientFor := parent,
                  _iconName := "dialog-information"]

  -- This should fail to compile with
  -- >> No instance for (HasPropertyButtons MessageDialog WritableAttr)
  -- set message [ _buttons := ButtonsTypeOk ]

  _ <- get messageBox _messageArea >>= castToBox

  -- Should fail to compile, with
  -- >> Couldn't match type `NonConstructibleAttr' with `ConstructibleAttr'
  -- set messageBox [_messageArea := undefined]

  -- This should fail to compile with
  -- >> Couldn't match type `NonReadableAttr' with `ReadableAttr'
  -- get messageArea _child

  result <- dialogRun messageBox
  putStrLn $ " >>> " ++ show ((toEnum . fromIntegral) result :: ResponseType)

  widgetDestroy messageBox
  putStrLn "+++ Polymorphic lenses test done"

main :: IO ()
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
             error "gtk_init did not process --g-fatal-warnings"

        -- Here "_type" is a polymorphic lens, valid for any object
        -- with a "type" (GObject) property. If one wants to be more
        -- specific it is possible to prefix the property with the
        -- type that declared it, for instance "windowType" in this
        -- case. Specifying also makes the type errors in case
        -- something goes wrong easier to understand.
	win <- new Window [_type := WindowTypeToplevel,
                           _iconName := "applications-haskell"]
        win <!> onWidgetDestroy $ do
                  putStrLn "Closing the program"
                  mainQuit

        grid <- new Grid [_orientation := OrientationVertical]
        set win [_child := grid]

        label <- new Label [_label := "Test"]
        widgetShow label
        label <!> onLabelActivateLink $ \uri -> do
          testPolymorphicLenses win "Link clicked, thanks!"
          return True -- Link processed, do not open with the browser
        containerAdd grid label

        button <- new Button [_label := "Click me!"]
        button <!> onButtonClicked $ do
                set label [_label := "This is <a href=\"http://www.gnome.org\">a test</a>",
                           _useMarkup := True ]
                -- set button [widgetSensitive := False, ...] would be
                -- more natural, but this serves as a test of
                -- attribute updating functions.
                set button [_sensitive :~ not,
                            _relief := ReliefStyleNone,
                            _label := "Thanks for clicking!"]
                sensitive <- get button _sensitive
                label <- get button _label
                putStrLn $ "New button text is "
                             ++ show label
                             ++ " and sensitive is " ++ show sensitive
        containerAdd grid button

        testGio
        testExceptions
        testNullableArgs
        testOutArgs
        testBoxed
        testImportedLenses

	widgetShowAll win
	Gtk.main
