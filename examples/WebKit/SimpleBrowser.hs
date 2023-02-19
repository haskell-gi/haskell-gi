{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.WebKit2 as WK
import qualified GI.Adw as Adw

import Data.GI.Base

import System.Mem (performGC)

import Control.Monad (void)
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

initializeExtensions :: WK.WebContext -> IO ()
initializeExtensions context = do
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

activate :: Adw.Application -> IO ()
activate app = do
  -- We periodically perform a GC, in order to test that the
  -- finalizers are not pointing to invalid regions. This is only for
  -- testing, and not needed in production code.
  _ <- GLib.timeoutAdd 0 5000 $ do
         putStrLn "** (T) Going into GC"
         performGC
         putStrLn "** GC done"
         return True

  highlight <- new Gtk.Button [ #label := "Highlight links"
                              , #sensitive := False
                              , #tooltipMarkup := "DBus server for the extension could not be found,\nsee <b>[Note (installing the extension)]</b> in <i>SimpleBrowser.hs</i> for details."]

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  uriEntry <- new Gtk.Entry [#placeholderText := "Type the address to load here",
                             #widthChars := 50]

  header <- new Adw.HeaderBar [#titleWidget := uriEntry]
  header.packStart highlight

  content.append header

  sessionBus <- Gio.busGetSync Gio.BusTypeSession (Nothing @Gio.Cancellable)

  context <- new WK.WebContext [On #initializeWebExtensions
                                 (initializeExtensions ?self)]

  contentManager <- new WK.UserContentManager
    [On (#scriptMessageReceived ::: "haskell_gi_handler") $ \result -> do
      resultAsString <- result.getJsValue >>= #toString
      putStrLn $ "Got a message: " <> show resultAsString
    ]
  #registerScriptMessageHandler contentManager "haskell_gi_handler"

  view <- new WK.WebView [ #webContext := context
                         , #userContentManager := contentManager
                         , On #loadChanged $ \event -> do
                             putStrLn $ "Load: " <> show event
                         , On #loadFailed $ \_ uri error -> do
                             errMsg <- gerrorMessage error
                             putStrLn . T.unpack $ "Error when reading \""
                               <> uri <> "\": " <> errMsg
                             -- Keep processing, so WebKit shows the error page
                             return False
                         , #vexpand := True
                         ]

  view.loadUri "http://www.haskell.org"

  content.append view

  on uriEntry #activate $ do
    uri <- uriEntry `get` #text
    view.loadUri uri

  on highlight #clicked (execDBusMethod sessionBus extensionServerInfo
                         "highlightLinks" Nothing (Just showLinkCount))

  on view (PropertyNotify #estimatedLoadProgress) $ \_ -> do
    status <- view `get` #estimatedLoadProgress
    uriEntry `set` [#progressFraction := if status /= 1.0
                                         then status
                                         else 0]

  registerDBusServer browserServerInfo (methodCall highlight) (return ())

  win <- new Adw.ApplicationWindow [#application := app,
                                    #content := content,
                                    #defaultWidth := 1024,
                                    #defaultHeight := 768]
  win.present

main :: IO ()
main = do
  app <- new Adw.Application [#applicationId := "haskell-gi.Gtk4.test",
                              On #activate (activate ?self)]

  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)
