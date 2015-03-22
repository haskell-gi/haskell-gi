{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import BasicPrelude hiding (on, error)
import qualified BasicPrelude as BP
import GI.Gtk hiding (main)
import GI.GtkAttributes () -- For various attribute instances
import GI.GtkSignals () -- For signal instances
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import GI.Properties
import GI.Signals

import GI.Utils.Base

import Foreign.C

import qualified Data.ByteString.Char8 as B
import Data.Text (pack, unpack)
import Data.Word
import Data.Int
import qualified Data.Map as M

import System.Environment (getProgName)
import System.Random (randomRIO)

error :: Text -> a
error = BP.error . unpack

testBoxedOutArgs :: IO ()
testBoxedOutArgs = do
  putStrLn "*** Boxed out args test"
  replicateM_ 100 $ do
    (success, color) <- Gdk.colorParse "green"
    when (success /= True) $
         error $ "Color parsing failed"
    colorString <- Gdk.colorToString color
    when (colorString /= "#000080800000") $
         error $ "Unexpected result from parsing : " ++ colorString
  putStrLn "+++ Boxed out args test done"

-- The allocation strategy is slightly different with GObjects which
-- are not initiallyUnowned, since we simply steal the memory (without
-- a g_object_ref_sink). Make sure that that there is no problem with
-- this.
testInitiallyOwned :: IO ()
testInitiallyOwned = do
  putStrLn "*** Initially owned allocation test"
  replicateM_ 100 $ do
    eb <- new EntryBuffer [_text := "Hello, this is a test"]
    t <- eb `get` _text
    when (t /= "Hello, this is a test") $
         error "Test text did not match!"
  putStrLn "*** Initially owned allocation test done"

testGio :: IO ()
testGio = do
  putStrLn "*** Gio test"

  infos <- Gio.appInfoGetAll
  putStrLn $ "(" ++ show (length infos) ++ " entries total, showing first 5)"
  forM_ (take 5 infos) $ \info -> do
            name <- Gio.appInfoGetName info
            exe <- Gio.appInfoGetExecutable info
            putStrLn $ "name: " ++ name
            putStrLn $ "exe: " ++ exe
  putStrLn "+++ Gio test done"

testExceptions :: IO ()
testExceptions = do
  putStrLn "*** Exception test"
  -- This should work fine, without emitting any exception
  contents <- GLib.fileGetContents "testGtk.hs"
  putStrLn $ "testGtk.hs is " ++ show (B.length contents)
               ++ " bytes long. First 5 lines follow:"
  forM_ ((take 5 . B.lines) contents) B.putStrLn

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
  replicateM_ 500 $ do
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

testPolymorphicLenses :: Window -> Text -> IO ()
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
    No instance for (HasAttr "Buttons" String)
      arising from a use of `:='
    ...
    (among others)
-}
  -- set message [ _buttons := ButtonsTypeOk ]

  get messageBox _messageArea >>= castTo Box >>= \case
    Just _ -> return ()
    Nothing -> error "Could not convert message area to Box"

  -- Should fail to compile, with
{-
    No instance for (ReadOnlyAttr 'AttrSet) arising from a use of `:='
    ...
-}
  -- set messageBox [_messageArea := undefined]

  -- Should fail to compile, with
{-
    No instance for (ConstructOnlyAttr 'AttrSet)
      arising from a use of `:='
    ...
-}
  -- set messageBox [_buttons := ButtonsTypeYesNo]

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
  replicateM_ 100 $ do
      (result, timeval) <- GLib.timeValFromIso8601 "2013-12-15T11:11:07Z"
      if result == True
      then do
        GLib.timeValAdd timeval (60*1000000)
        timevalStr <- GLib.timeValToIso8601 timeval
        when (timevalStr /= "2013-12-15T11:12:07Z") $
             error $ "Time conversion failed, got " ++ timevalStr
      else error $ "timeValFromIso8601 failed!"
  putStrLn "+++ Out Structs test done"

testOutBlockPacks :: IO ()
testOutBlockPacks = do
  putStrLn "*** Out Block packs test"
  replicateM_ 100 $ do
    (result, palette) <- colorSelectionPaletteFromString "BlanchedAlmond:RoyalBlue:#ffccaa"
    if result == True
    then do
      paletteStr <- colorSelectionPaletteToString palette
      when (paletteStr /= "#FFEBCD:#4169E1:#FFCCAA") $
           error $ "Color conversion failed, got " ++ paletteStr
    else error $ "colorSelecionPaletteFromString failed!"
  putStrLn "+++ Out Block packs test done"

testFlags :: IO ()
testFlags = do
  putStrLn "*** Flags test"
  replicateM_ 100 $ do
     let w = gflagsToWord [DebugFlagUpdates, DebugFlagNoPixelCache] :: Integer
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
  _ <- GLib.timeoutAdd GLib._PRIORITY_DEFAULT 500 $ do
                andNow <- GLib.getMonotonicTime
                putStrLn $ "Timeout called @ " ++ show andNow
                return False
  putStrLn "+++ Timeout test done"

-- ScopeTypeAsync callback test
testMenuPopup :: IO ()
testMenuPopup = do
  putStrLn "*** ScopeTypeAsync test"
  menuitem <- new MenuItem [_label := "TestAsync"]
  menu <- new Menu []
  menuShellAppend menu menuitem
  widgetShowAll menu
  putStrLn "*** Menu constructed"
  curtime <- getCurrentEventTime
  menuPopup menu noWidget noWidget (Just positionFunc) 0 curtime
  putStrLn "+++ ScopeTypeAsync test done"
      where positionFunc _ = do
                  putStrLn "+++ Pos func"
                  posx <- GLib.randomIntRange 000 200
                  posy <- GLib.randomIntRange 000 200
                  putStrLn "+++ Pos func done"
                  return (posx, posy, False)

-- ScopeTypeCall callback test
testForeach :: ContainerK a => a -> IO ()
testForeach container = do
  putStrLn "*** ScopeTypeCall test"
  containerForeach container $ \widget -> do
           path <- widgetGetPath widget
           putStrLn =<< widgetPathToString path
  putStrLn "+++ ScopeTypeCall test done"

roundtrip :: (IsGVariant a, Eq a, Show a) => a -> IO ()
roundtrip value =
  toGVariant value >>= fromGVariant >>= \case
    Nothing -> error $ "GVariant decoding for " ++ show value ++ " failed."
    Just r -> when (r /= value) $
              error $ "Got " ++ show r ++ " but expected "
                        ++ show value ++ "."

-- Test of proper marshalling of GVariants
testGVariant :: IO ()
testGVariant = do
  putStrLn "*** GVariant test"
  replicateM_ 100 $ do
       roundtrip (True :: Bool)
       roundtrip (11 :: Word8)
       roundtrip (12 :: Int16)
       roundtrip (13 :: Word16)
       roundtrip (14 :: Int32)
       roundtrip (15 :: Word32)
       roundtrip (16 :: Int64)
       roundtrip (17 :: Word64)
       roundtrip (GVariantHandle 18)
       roundtrip (1.23 :: Double)
       roundtrip ("Hello" :: Text)
       case newGVariantObjectPath "/org/testGtk/trial" of
         Nothing -> error "Error parsing object path."
         Just path -> roundtrip path
       case newGVariantSignature "s(ss)ia{dh}" of
         Nothing -> error "Error parsing signature."
         Just signature -> roundtrip signature
       let innerText = "Testing inner" :: Text
       gv <- toGVariant innerText >>= toGVariant
       fromGVariant gv >>= \case
          Nothing -> error "Could not decode GVariant container"
          Just innerGV -> fromGVariant innerGV >>= \case
                            Nothing -> error "Could not decode inner GVariant."
                            Just text -> when (text /= innerText) $
                                         error $ "Got wrong text : " ++ text
       roundtrip (Just (18 :: Word64))
       roundtrip ("ByteString test" :: ByteString)
       roundtrip (map Just [19..25 :: Int16])
       roundtrip ()
       roundtrip (GVariantSinglet (3.14 :: Double))
       roundtrip (0.3 :: Double, "Two-tuple" :: Text)
       roundtrip ("One" :: Text, "Two" :: ByteString, Just (3 :: Word32))
       roundtrip ((), GVariantSinglet (), True, Just False)
       roundtrip (21 :: Word8, 22 :: Word16, 23 :: Word32, 24 :: Word64,
                        ((), ((), ())))
       let dict :: M.Map Text Double
           dict = M.fromList [("One", 1), ("Two", 2.001), ("Three", 3)]
       roundtrip dict
  putStrLn "+++ GVariant test done"

testCast :: IO ()
testCast =  do
  putStrLn "*** castTo test"
  label <- new Label []
  castTo Widget label >>= \case
    Just w -> castTo Button w >>= \case
      Just _ -> error "Converted Label to Button successfully, this is an error!"
      Nothing -> castTo Label w >>= \case
        Just _ -> return ()
        Nothing -> error "Could not upcast back to a Label!"
    Nothing -> error "Downcast to Widget failed!"
  putStrLn "+++ castTo test done"

testArrayOfArrays :: IO ()
testArrayOfArrays = do
  putStrLn "*** Array of array test"
  Gio.desktopAppInfoSearch "text" >>= print
  putStrLn "+++ Array of array test done"

main :: IO ()
main = do
        -- Generally one should do the following to init Gtk:
        -- import System.Environment (getArgs, getProgName)
        -- ...
        -- args <- getArgs
        -- progName <- pack <$> getProgName
        -- restArgs <- Gtk.init $ progName:args
        --
        -- Here we use synthetic arguments to test that we are
        -- handling InOut arguments properly.
        progName <- pack <$> getProgName
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
        on win Destroy $ do
                  putStrLn "Closing the program"
                  mainQuit

        grid <- new Grid [_orientation := OrientationVertical]
        set win [_child := grid]

        label <- new Label [_label := "Test"]
        on label ActivateLink $ \uri -> do
          testPolymorphicLenses win ("Link " ++ uri ++ " clicked, thanks!")
          return True -- Link processed, do not open with the browser
        containerAdd grid label

        button <- new Button [_label := "_Click me!",
                              _useUnderline := True]
        on button Clicked $ do
                set label [_label := "This is <a href=\"http://www.gnome.org\">a test</a>",
                           _useMarkup := True ]
                -- set button [widgetSensitive := False, ...] would be
                -- more natural, but this serves as a test of
                -- attribute updating functions.
                set button [_sensitive :~ not,
                            _relief := ReliefStyleNone,
                            _label := "Thanks for clicking!"]
                sensitive <- get button _sensitive
                newLabel <- get button _label
                putStrLn $ "New button text is "
                             ++ show newLabel
                             ++ " and sensitive is " ++ show sensitive
        containerAdd grid button

        popupButton <- new Button [_label := "_Pop-up menu",
                                   _useUnderline := True]
        on popupButton Clicked $ testMenuPopup
        containerAdd grid popupButton

        testBoxedOutArgs
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
        testInitiallyOwned
        testGVariant
        testCast
        testArrayOfArrays

	widgetShowAll win

	Gtk.main
