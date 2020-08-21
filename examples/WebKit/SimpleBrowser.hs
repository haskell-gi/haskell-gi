{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.WebKit2 as WK

import Data.GI.Base

import System.Mem (performGC)

import qualified Data.Text as T
import Data.Text (Text)
import Data.Int (Int64)
import Data.Word (Word32)
import System.Environment (getProgName, getArgs)

import DBusHelpers (registerDBusServer, execDBusMethod)
import DBusServersInfo (extensionServerInfo, browserServerInfo)

showLinkCount :: GVariant -> IO ()
showLinkCount gv = fromGVariant @(GVariantSinglet Word32) gv >>= \case
  Just (GVariantSinglet n) -> putStrLn $ "Found " <> show n <> " links."
  Nothing -> putStrLn "Could not decode return value!"

methodCall :: Gtk.Button -> Text -> GVariant -> IO (Maybe GVariant)
methodCall highlight methodName _params =
  case methodName of
    "extensionActivated" -> do
      highlight `set` [ #sensitive     := True
                      , #tooltipMarkup := "Highlight the links in the page" ]
      return Nothing
    _ -> return Nothing

-- | Create the interface of the browser.
createGUI :: Gio.DBusConnection -> Gtk.Button -> IO ()
createGUI sessionBus highlight = do
  win <- new Gtk.Window [#type := Gtk.WindowTypeToplevel,
                         #iconName := "applications-haskell",
                         #defaultWidth := 1024,
                         #defaultHeight := 768,
                         On #destroy Gtk.mainQuit]

  context <- new WK.WebContext []
  on context #initializeWebExtensions $ do
    -- [Note (installing the extension)]
    --
    -- This loads any extensions in the current directory. These are
    -- ".so" files in Linux. For an example of an extension, see
    -- "SimpleExtension.hs". After building it with
    --
    -- > cabal new-build simple-webextension
    --
    -- you can create a symlink in "." to the resulting .so file, which will
    -- be somewhere like
    --
    -- dist-newstyle/build/x86_64-linux/ghc-8.6.1/webkit-example-0.1/f/simple-webextension/build/simple-webextension/libsimple-webextension.so.-1.1.0
    --
    -- for new-style cabal, and WebKit will pick it up. Alternatively,
    -- one can set this to the directory where the extensions are
    -- built into.
    #setWebExtensionsDirectory context "."

    -- We can pass arbitrary data to the WebExtension (which runs into
    -- a different process), as long as it fits into a
    -- `GVariant`. Here we set the data to be passed. `toGVariant`
    -- works for any type which is an instance of `IsGVariant`:
    -- https://hackage.haskell.org/package/haskell-gi-base/docs/Data-GI-Base-GVariant.html#t:IsGVariant
    userData <- toGVariant ("Hi, extension!" :: T.Text, 57 :: Int64)
    #setWebExtensionsInitializationUserData context userData

  contentManager <- new WK.UserContentManager
    [On (#scriptMessageReceived ::: "haskell_gi_handler") $ \result -> do
      resultAsString <- #getJsValue result >>= #toString
      putStrLn $ "Got a message: " <> show resultAsString
    ]
  #registerScriptMessageHandler contentManager "haskell_gi_handler"

  view <- new WK.WebView [ #webContext := context
                         , #userContentManager := contentManager
                         , On #close (#destroy win)
                         , On #loadChanged $ \event -> do
                             putStrLn $ "Load: " <> show event
                         , On #loadFailed $ \_ uri error -> do
                             errMsg <- gerrorMessage error
                             putStrLn . T.unpack $ "Error when reading \""
                               <> uri <> "\": " <> errMsg
                             -- Keep processing, so WebKit shows the error page
                             return False
                         ]

  #loadUri view "http://www.haskell.org"

  #add win view

  uriEntry <- new Gtk.Entry [#placeholderText := "Type the address to load here",
                             #widthChars := 50]
  on uriEntry #activate $ do
    uri <- uriEntry `get` #text
    #loadUri view uri

  header <- new Gtk.HeaderBar [#showCloseButton := True,
                               #customTitle := uriEntry,
                               #title := "A simple WebKit browser"]
  #packStart header highlight
  on highlight #clicked (execDBusMethod sessionBus extensionServerInfo
                         "highlightLinks" Nothing (Just showLinkCount))

  #setTitlebar win (Just header)

  on view (PropertyNotify #estimatedLoadProgress) $ \_ -> do
    status <- view `get` #estimatedLoadProgress
    uriEntry `set` [#progressFraction := if status /= 1.0
                                         then status
                                         else 0]

  #showAll win

main :: IO ()
main = do
  progName <- T.pack <$> getProgName
  args <- map T.pack <$> getArgs

  -- We periodically perform a GC, in order to test that the
  -- finalizers are not pointing to invalid regions. This is only for
  -- testing, and not needed in production code.
  _ <- GLib.timeoutAdd 0 5000 $ do
         putStrLn "** (T) Going into GC"
         performGC
         putStrLn "** GC done"
         return True

  _ <- Gtk.init $ Just (progName : args)

  highlight <- new Gtk.Button [ #label := "Highlight links"
                              , #sensitive := False
                              , #tooltipMarkup := "DBus server for the extension could not be found,\nsee <b>[Note (installing the extension)]</b> in <i>SimpleBrowser.hs</i> for details."]

  -- We create the interface of the browser (and in particular, the
  -- WebView, which will trigger the loading of the extension) only
  -- once we have registered ourselves into the session bus, to avoid
  -- a race condition with the extension, which will attempt to call
  -- our 'extensionActivated' method as soon as it is ready.
  sessionBus <- Gio.busGetSync Gio.BusTypeSession (Nothing @Gio.Cancellable)
  registerDBusServer browserServerInfo (methodCall highlight)
                     (createGUI sessionBus highlight)

  Gtk.main
