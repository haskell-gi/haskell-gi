{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import qualified GI.WebKit2 as WK

import Data.GI.Base

import System.Mem (performGC)

import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Environment (getProgName, getArgs)

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

  win <- new Gtk.Window [#type := Gtk.WindowTypeToplevel,
                         #iconName := "applications-haskell",
                         #defaultWidth := 1024,
                         #defaultHeight := 768]
  on win #destroy Gtk.mainQuit

  context <- new WK.WebContext []
  on context #initializeWebExtensions $ do
    -- This loads any extensions in the current directory. These are
    -- ".so" files in Linux. For an example of an extension, see
    -- "SimpleExtension.hs". After building it with
    --
    -- > cabal new-build simple-webextension
    --
    -- you can link it to the current directory, and WebKit will pick
    -- it up. Alternatively, one can set this to the directory where
    -- the extensions are built into.
    #setWebExtensionsDirectory context "."

  view <- new WK.WebView [#webContext := context]
  on view #close $ #destroy win
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
  #setTitlebar win (Just header)

  on view (PropertyNotify #estimatedLoadProgress) $ \_ -> do
    status <- view `get` #estimatedLoadProgress
    uriEntry `set` [#progressFraction := if status /= 1.0
                                         then status
                                         else 0]

  on view #loadChanged $ \event -> do
    putStrLn $ "Load: " <> show event

  on view #loadFailed $ \_ uri error -> do
    errMsg <- gerrorMessage error
    putStrLn . T.unpack $ "Error when reading \"" <> uri <> "\": " <> errMsg
    -- Keep processing, so WebKit shows the error page
    return False

  #showAll win

  Gtk.main
