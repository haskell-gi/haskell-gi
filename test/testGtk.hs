{-# LANGUAGE OverloadedStrings #-}
import GI.Gtk hiding (main)
import GI.GtkAttributes
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import GI.Properties

import GI.Utils.Attributes
import GI.Utils.Properties (new)
import GI.Utils.BasicTypes

import Foreign.C

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
  print =<< address `get` _family
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
{-
    Couldn't match type `AttrIsConstructible "Buttons" String'
                  with 'True
    Expected type: 'True
      Actual type: AttrIsConstructible "Buttons" String
-}
  -- set message [ _buttons := ButtonsTypeOk ]

  _ <- get messageBox _messageArea >>= castToBox

  -- Should fail to compile, with
{-
    Couldn't match type 'False with 'True
    Expected type: 'True
      Actual type: AttrIsConstructible "MessageArea" MessageDialog
-}
  -- set messageBox [_messageArea := undefined]

  -- This should fail to compile with
{-
    Couldn't match type 'False with 'True
    Expected type: 'True
      Actual type: AttrIsReadable "Child" MessageDialog

-}
  -- get messageBox _child

  result <- dialogRun messageBox
  putStrLn $ " >>> " ++ show ((toEnum . fromIntegral) result :: ResponseType)

  widgetDestroy messageBox
  putStrLn "+++ Polymorphic lenses test done"

testOutStructs :: IO ()
testOutStructs = do
  putStrLn "*** Out Structs test"
  (result, timeval) <- GLib.timeValFromIso8601 "2013-12-15T11:11:07Z"
  if result == True
  then do
    GLib.timeValAdd timeval (60*1000000)
    timevalStr <- GLib.timeValToIso8601 timeval
    if timevalStr /= "2013-12-15T11:12:07Z"
    then error $ "Time conversion failed, got " ++ timevalStr
    else putStrLn "+++ Out Structs test done"
  else error $ "timeValFromIso8601 failed!"

testOutBlockPacks :: IO ()
testOutBlockPacks = do
  putStrLn "*** Out Block packs test"
  (result, palette) <- colorSelectionPaletteFromString "BlanchedAlmond:RoyalBlue:#ffccaa"
  if result == True
  then do
    paletteStr <- colorSelectionPaletteToString palette
    if paletteStr /= "#FFEBCD:#4169E1:#FFCCAA"
    then error $ "Color conversion failed, got " ++ paletteStr
    else putStrLn "+++ Out Block packs test done"
  else error $ "colorSelecionPaletteFromString failed!"

testFlags :: IO ()
testFlags = do
  putStrLn "*** Flags test"
  let w = gflagsToWord [DebugFlagUpdates, DebugFlagNoPixelCache]
  when (w /= 65552) $
       error $ "Flags -> Word failed, got " ++ show w
  let fs = wordToGFlags (3072 :: CUInt)
  when (fs /= [DebugFlagPrinting, DebugFlagBuilder]) $
       error $ "Word -> Flags failed, got " ++ show fs
  putStrLn "+++ Flags test done"

-- ScopeTypeNotify callback test
testTimeout :: IO ()
testTimeout = do
  putStrLn "*** Timeout test"
  now <- GLib.getMonotonicTime
  putStrLn $ "Now is " ++ show now ++ " , adding timeout."
  _ <- GLib.timeoutAdd GLib.g_PRIORITY_DEFAULT 500 $ do
                now <- GLib.getMonotonicTime
                putStrLn $ "Timeout called @ " ++ show now
                return False
  putStrLn "+++ Timeout test done"

-- ScopeTypeAsync callback test
testMenuPopup :: IO ()
testMenuPopup = do
  putStrLn "*** ScopeTypeAsync test"
  menuitem <- new MenuItem [_label := "TestAsync"]
  menu <- new Menu []
  menuShellAppend menu menuitem
  curtime <- getCurrentEventTime
  widgetShowAll menu
  menuPopup menu (Nothing :: Maybe Label) (Nothing :: Maybe Label) (Just positionFunc) 0 curtime
  putStrLn "+++ ScopeTypeAsync test done"
      where positionFunc _ = do
                  posx <- GLib.randomIntRange 000 200
                  posy <- GLib.randomIntRange 000 200
                  return (posx, posy, False)

-- ScopeTypeCall callback test
testForeach :: ContainerK a => a -> IO ()
testForeach container = do
  putStrLn "*** ScopeTypeCall test"
  containerForeach container $ \widget -> do
           path <- widgetGetPath widget
           putStrLn =<< widgetPathToString path
  putStrLn "+++ ScopeTypeCall test done"

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
        label <!> onLabelActivateLink $ \uri -> do
          testPolymorphicLenses win "Link clicked, thanks!"
          return True -- Link processed, do not open with the browser
        containerAdd grid label

        button <- new Button [_label := "_Click me!",
                              _useUnderline := True]
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

        popupButton <- new Button [_label := "_Pop-up menu",
                                   _useUnderline := True]
        popupButton <!> onButtonClicked $ testMenuPopup
        containerAdd grid popupButton

        testGio
        testExceptions
        testNullableArgs
        testOutArgs
        testBoxed
        testImportedLenses
        testOutStructs
        testOutBlockPacks
        testFlags
        testTimeout
        testForeach grid

	widgetShowAll win
	Gtk.main
