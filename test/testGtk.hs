{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, OverloadedLabels, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Prelude hiding (error, (++), putStrLn, show)
import qualified Prelude as P

import Data.GI.Base.Signals (disconnectSignalHandler)

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib

import Foreign.C

import System.Mem (performGC)

import Control.Monad (when, replicateM_, forM_, forM)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack, Text)
import Data.Text.IO (putStrLn)
import Data.Word
import Data.Int
import qualified Data.Map as M

import System.Environment (getProgName)
import System.Random (randomRIO)

-- Some nicer definitions
error :: Text -> a
error = P.error . unpack

(++) :: Monoid a => a -> a -> a
(++) = (<>)

show :: Show a => a -> Text
show = pack . P.show

-- Make sure we use the right allocator/deallocator for zero-filled
-- structs.
testAllocations :: IO ()
testAllocations = do
  performGC
  putStrLn "*** Allocations test"
  replicateM_ 100 $ do
        _ <- Gdk.newZeroColor -- boxed
        _ <- GLib.newZeroTimeVal -- unboxed
        return ()
  performGC
  putStrLn "+++ Allocations test done"

-- Overloaded syntax for `new` for structs.
testConstructible :: IO ()
testConstructible = do
  performGC
  putStrLn "*** Constructible test"
  replicateM_ 100 $ do
    rgba <- new Gdk.RGBA [ #red := 0.7, #alpha := 1] -- boxed
    Gdk.rGBAToString rgba >>=
           \s -> when (s /= "rgb(179,0,0)") $
                 error $ "Unexpected RGBA: " <> show s
    -- We could also use "#red" here, but this serves as a test of the
    -- namespaced attributes.
    rgba `get` Gdk.rGBA_red >>=
         -- Here it is fine to compare doubles for equality, since we
         -- want to make sure that the roundtripping does not introduce
         -- errors.
             \r -> when (r /= 0.7) $
                   error $ "Unexpected \"red\" value: " <> show r
    _ <- new GLib.TimeVal [] -- unboxed
    return ()
  performGC
  putStrLn "+++ Constructible test done"

testBoxedOutArgs :: IO ()
testBoxedOutArgs = do
  performGC
  putStrLn "*** Boxed out args test"
  replicateM_ 100 $ do
    (success, color) <- Gdk.colorParse "green"
    when (success /= True) $
         error $ "Color parsing failed"
    colorString <- Gdk.colorToString color
    when (colorString /= "#000080800000") $
         error $ "Unexpected result from parsing : " ++ colorString
  performGC
  putStrLn "+++ Boxed out args test done"

-- The allocation strategy is slightly different with GObjects which
-- are not initiallyUnowned, since we simply steal the memory (without
-- a g_object_ref_sink). Make sure that that there is no problem with
-- this.
testInitiallyOwned :: IO ()
testInitiallyOwned = do
  performGC
  putStrLn "*** Initially owned allocation test"
  replicateM_ 100 $ do
    eb <- new EntryBuffer [ #text := "Hello, this is a test"]
    t <- eb `get` #text
    when (t /= "Hello, this is a test") $
         error "Test text did not match!"
  performGC
  putStrLn "*** Initially owned allocation test done"

testGio :: IO ()
testGio = do
  performGC
  putStrLn "*** Gio test"
  infos <- Gio.appInfoGetAll
  putStrLn $ "(" ++ show (length infos) ++ " entries total, showing first 5)"
  forM_ (take 5 infos) $ \info -> do
            name <- Gio.appInfoGetName info
            exe <- Gio.appInfoGetExecutable info
            putStrLn $ "name: " ++ name
            putStrLn $ "exe: " ++ pack exe
  performGC
  putStrLn "+++ Gio test done"

testExceptions :: IO ()
testExceptions = do
  performGC
  putStrLn "*** Exception test"
  -- This should work fine, without emitting any exception
  contents <- GLib.fileGetContents "testGtk.hs"
  putStrLn $ "testGtk.hs is " ++ show (B.length contents)
               ++ " bytes long. First 5 lines follow:"
  forM_ ((take 5 . B.lines) contents) B.putStrLn

  -- Trying to read a file that does not exist should throw
  -- FileErrorNoent, in the FileError domain.
  ret <- GLib.catchFileError (Just <$> GLib.fileGetContents "this file does not exist") $
         \code msg ->
           case code of
             GLib.FileErrorNoent -> return Nothing
             _ -> error $ "Unexpected error code : \"" ++ show code ++
                            "\" with message : \"" ++ msg ++ "\""

  case ret of
    Just buf -> error $ "Exception did not arise! Got the following contents: "
                ++ show (buf)
    Nothing -> putStrLn "<< Exception handled >>"

  performGC
  putStrLn "+++ Exception test done"

testNullableArgs :: IO ()
testNullableArgs = do
  performGC
  putStrLn "*** Nullable args test"
  uri <- GLib.filenameToUri "/usr/lib/test" Nothing
  when (uri /= "file:///usr/lib/test") $
       error $ "First nullable test failed : " ++ uri

  uri' <- GLib.filenameToUri "/usr/lib/test" (Just "gnome.org")
  when (uri' /= "file://gnome.org/usr/lib/test") $
       error $ "Second nullable test failed : " ++ uri'
  performGC
  putStrLn "+++ Nullable args test done"

testOutArgs :: IO ()
testOutArgs = do
  performGC
  putStrLn "*** Out args test"
  iconThemeGetDefault >>= iconThemeGetSearchPath >>= print
  performGC
  putStrLn "+++ Out args test done"

testBoxed :: IO ()
testBoxed = do
  performGC
  putStrLn "*** Boxed test"
  replicateM_ 500 $ do
    r <- randomRIO (0,6)
    let expected = toEnum $ r + fromEnum GLib.DateWeekdayMonday
    date <- GLib.dateNewDmy (fromIntegral $ 17 + r) GLib.DateMonthJune 2013
    weekday <- GLib.dateGetWeekday date
    when (weekday /= expected) $
         error $ show r ++ " -> Got wrong weekday! : " ++ show weekday
  performGC
  putStrLn "+++ Boxed test done"

testImportedLenses :: IO ()
testImportedLenses = do
  performGC
  putStrLn "*** Imported lenses test"
  Just address <- Gio.inetAddressNewFromString "173.194.40.51"
  print =<< address `get` #family
  performGC
  putStrLn "+++ Imported lenses test done"

testPolymorphicLenses :: Window -> Text -> IO ()
testPolymorphicLenses parent message = do
  performGC
  putStrLn "*** Polymorphic lenses test"
  messageBox <- new MessageDialog
                [ #buttons := ButtonsTypeYesNo, -- ConstructOnly
                  #text := message,
                  #title := "Important message",
                  #transientFor := parent,
                  #iconName := "dialog-information"]

  get messageBox #messageArea >>= castTo Box >>= \case
    Just _ -> return ()
    Nothing -> error "Could not convert message area to Box"

  -- Failed compilation tests follow, uncomment the lines to reproduce
  -- the errors. The given error messages are for ghc >= 8.0, for
  -- previous ghc versions the messages are more verbose.

  -- This should fail to compile with
{-
    • Unknown attribute "authors" for object "MessageDialog".
-}
  -- set messageBox [ #authors := undefined ]

  -- Should fail to compile, with
{-
    • Attribute "MessageDialog::message-area" is not settable.
-}
  -- set messageBox [ #messageArea := undefined]

  -- Should fail to compile, with
{-
    • Attribute "MessageDialog::buttons" is not settable.
-}
  -- set messageBox [ #buttons := ButtonsTypeYesNo]

  -- This should fail to compile with
{-
    • Attribute "Container::child" is not gettable.
-}
  -- get messageBox #child

  result <- dialogRun messageBox
  putStrLn $ " >>> " ++ show ((toEnum . fromIntegral) result :: ResponseType)

  widgetDestroy messageBox
  performGC
  putStrLn "+++ Polymorphic lenses test done"

testOutStructs :: IO ()
testOutStructs = do
  performGC
  putStrLn "*** Out Structs test"
  replicateM_ 100 $ do
      (result, timeval) <- GLib.timeValFromIso8601 "2013-12-15T11:11:07Z"
      if result == True
      then do
        GLib.timeValAdd timeval (60*1000000)
        timevalStr <- GLib.timeValToIso8601 timeval
        when (timevalStr /= Just "2013-12-15T11:12:07Z") $
             error $ "Time conversion failed, got " ++ show timevalStr
      else error $ "timeValFromIso8601 failed!"
  performGC
  putStrLn "+++ Out Structs test done"

testOutBlockPacks :: IO ()
testOutBlockPacks = do
  performGC
  putStrLn "*** Out Block packs test"
  replicateM_ 100 $ do
    (result, palette) <- colorSelectionPaletteFromString "BlanchedAlmond:RoyalBlue:#ffccaa"
    if result == True
    then do
      paletteStr <- colorSelectionPaletteToString palette
      when (paletteStr /= "#FFEBCD:#4169E1:#FFCCAA") $
           error $ "Color conversion failed, got " ++ paletteStr
    else error $ "colorSelecionPaletteFromString failed!"
  performGC
  putStrLn "+++ Out Block packs test done"

testFlags :: IO ()
testFlags = do
  performGC
  putStrLn "*** Flags test"
  replicateM_ 100 $ do
     let w = gflagsToWord [DebugFlagUpdates, DebugFlagNoPixelCache] :: Integer
     when (w /= 65552) $
          error $ "Flags -> Word failed, got " ++ show w
     let fs = wordToGFlags (3072 :: CUInt)
     when (fs /= [DebugFlagPrinting, DebugFlagBuilder]) $
          error $ "Word -> Flags failed, got " ++ show fs
  performGC
  putStrLn "+++ Flags test done"

-- ScopeTypeNotify callback test
testTimeout :: IO ()
testTimeout = do
  performGC
  putStrLn "*** Timeout test"
  now <- GLib.getMonotonicTime
  putStrLn $ "Now is " ++ show now ++ " , adding timeout."
  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 500 $ do
                andNow <- GLib.getMonotonicTime
                putStrLn $ "Timeout called @ " ++ show andNow
                return False
  performGC
  putStrLn "+++ Timeout test done"

-- Build the menu for the ScopeTypeAsync callback test. We build the
-- menu once and then reuse it every time we need to pop it up. In
-- addition to being more efficient this way, if we create a fresh
-- menu every time we need to show it (in testMenuPopup below) it
-- would get destroyed (and thus disappear suddenly without user
-- action) whenever the garbage collector runs, since there are no
-- references left to the menu on the Haskell side once testMenuPopup
-- exits.
buildPopupMenu :: IO Menu
buildPopupMenu = do
  performGC
  putStrLn "*** Building popup menu"
  menu <- new Menu []
  forM_ [1..3 :: Integer] $ \i -> do
    menuitem <- new MenuItem [ #label := "Menu entry " <> show i]
    on menuitem #activate $ putStrLn ("Menuitem " <> show i <> " activated.")
    #append menu menuitem
  widgetShowAll menu
  performGC
  putStrLn "+++ Menu constructed"
  return menu

testMenuPopup :: Gtk.IsWidget a => Menu -> a -> IO ()
testMenuPopup menu parent = do
  performGC
  #popupAtWidget menu parent Gdk.GravityNorthWest Gdk.GravityNorthWest Nothing
  performGC

-- ScopeTypeCall callback test
testForeach :: IsContainer a => a -> IO ()
testForeach container = do
  performGC
  putStrLn "*** ScopeTypeCall test"
  containerForeach container $ \widget -> do
           path <- widgetGetPath widget
           putStrLn =<< widgetPathToString path
  performGC
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
  performGC
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
  performGC
  putStrLn "+++ GVariant test done"

testCast :: IO ()
testCast =  do
  performGC
  putStrLn "*** castTo test"
  label <- new Label []
  _ <- toWidget label -- Safe cast, should always succeed.
  castTo Widget label >>= \case
    Just w -> castTo Button w >>= \case
      Just _ -> error "Converted Label to Button successfully, this is an error!"
      Nothing -> castTo Label w >>= \case
        Just _ -> return ()
        Nothing -> error "Could not upcast back to a Label!"
    Nothing -> error "Downcast to Widget failed!"
  performGC
  putStrLn "+++ castTo test done"

testArrayOfArrays :: IO ()
testArrayOfArrays = do
  performGC
  putStrLn "*** Array of array test"
  Gio.desktopAppInfoSearch "text" >>= print
  performGC
  putStrLn "+++ Array of array test done"

testConstantPatternMatching :: IO ()
testConstantPatternMatching = do
  performGC
  putStrLn "*** Constant pattern matching test"
  when (not $ checkDigits "0123456789") $
       error "Digit check failed!"
  when (checkDigits GLib.CSET_A_2_Z) $
       error "Digit check succeeded unexpectedly!"
  performGC
  putStrLn "+++ Constant pattern matching test done"
      where checkDigits :: Text -> Bool
            checkDigits GLib.CSET_DIGITS = True
            checkDigits _                = False

testOverloadedLabels :: IO ()
testOverloadedLabels = do
  performGC
  putStrLn "*** Overloaded labels test"
  Just address <- Gio.inetAddressNewFromString "173.194.40.51"
  -- Overloaded attributes
  family <- address `get` #family
  when (family /= Gio.SocketFamilyIpv4) $
       error $ "Got unexpected socket family from attr: " ++ show family
  -- Overloaded methods
  family <- #getFamily address
  when (family /= Gio.SocketFamilyIpv4) $
       error $ "Got unexpected socket family from method: " ++ show family
  performGC
  putStrLn "+++ Overloaded labels test done"

testSignalsDisconnect :: Button -> IO ()
testSignalsDisconnect button = do
  performGC
  putStrLn "*** Signals connect/disconnect test"
  handlers <- forM [1..100 :: Int] $ \id -> on button #clicked (print id)
  performGC
  mapM_ (disconnectSignalHandler button) handlers
  performGC
  putStrLn "+++ Signals connect/disconnect test done"

testTypedClosures :: Window -> IO ()
testTypedClosures win = do
  performGC
  putStrLn "*** Typed closures test"
  ag <- new Gtk.AccelGroup []
  closure <- genClosure_AccelGroupActivate
    (\_accelGroup _object keyval mods -> do
        maybeName <- Gdk.keyvalName keyval
        putStrLn $ "Got a key press for <" <> fromMaybe "(unknown)" maybeName
          <> ">, mods are " <> show mods
        return False)
  #connect ag Gdk.KEY_z [] [] closure

  #addAccelGroup win ag
  performGC
  putStrLn "+++ Typed closures test done"

main :: IO ()
main = do
        -- Generally one should do the following to init Gtk:
        -- import System.Environment (getArgs, getProgName)
        -- ...
        -- args <- getArgs
        -- progName <- pack <$> getProgName
        -- restArgs <- Gtk.init $ Just $ progName:args
        --
        -- or simply
        -- Gtk.init Nothing
        --
        -- if it is not important to give Gtk access to the
        -- commandline arguments.
        --
        -- Here we use synthetic arguments to test that we are
        -- handling InOut arguments properly.
        progName <- pack <$> getProgName
        restArgs <- Gtk.init $ Just $ progName:["--g-fatal-warnings"]
        when (restArgs /= Just [progName]) $
             error "gtk_init did not process --g-fatal-warnings"

        -- We periodically perform a GC, in order to test that the
        -- finalizers are not pointing to invalid regions.
        _ <- GLib.timeoutAdd 0 5000 $ do
               putStrLn "** (T) Going into GC"
               performGC
               putStrLn "** GC done"
               return True

        -- Here "#type" is a polymorphic lens, valid for any object
        -- with a "type" (GObject) property. If one wants to be more
        -- specific it is possible to prefix the property with the
        -- type that declared it, for instance "windowType" in this
        -- case. Specifying also makes the type errors in case
        -- something goes wrong easier to understand.
        win <- new Window [ #type := WindowTypeToplevel,
                            #iconName := "applications-haskell",
                            On #destroy $ do
                              putStrLn "Closing the program"
                              mainQuit
                          ]

        grid <- new Grid [ #orientation := OrientationVertical]
        set win [ #child := grid]

        label <- new Label [ #label := "Test",
                             On #activateLink $ \uri -> do
                               testPolymorphicLenses win
                                 ("Link " ++ uri ++ " clicked, thanks!")
                               return True -- Link processed, do not
                                           -- open with the browser
                           ]
        #add grid label

        button <- new Button [ #label := "_Click me!",
                               #useUnderline := True]
        on button #clicked $ do
                set label [ #label := "This is <a href=\"http://www.gnome.org\">a test</a>",
                            #useMarkup := True ]
                -- set button [widgetSensitive := False, ...] would be
                -- more natural, but this serves as a test of
                -- attribute updating functions.
                set button [widgetSensitive :~ not,
                            #relief := ReliefStyleNone,
                            #label := "Thanks for clicking!"]
                sensitive <- get button #sensitive
                newLabel <- get button #label
                putStrLn $ "New button text is "
                             ++ show newLabel
                             ++ " and sensitive is " ++ show sensitive
        #add grid button

        popupButton <- new Button [ #label := "_Pop-up menu",
                                    #useUnderline := True]
        menu <- buildPopupMenu
        on popupButton #clicked (testMenuPopup menu popupButton)
        #add grid popupButton

        testAllocations
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
        testConstantPatternMatching
        testOverloadedLabels
        testConstructible
        testSignalsDisconnect button
        testTypedClosures win

        #showAll win

        Gtk.main
