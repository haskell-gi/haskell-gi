{-# LANGUAGE OverloadedStrings #-}

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import GI.WebKit2

import GI.OverloadedLabels
import GI.Signals

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

  win <- new Window [_type := WindowTypeToplevel,
                     _iconName := "applications-haskell",
                     _defaultWidth := 1024,
                     _defaultHeight := 768]
  on win Destroy mainQuit

  view <- new WebView []
  on view Close $ _destroy win
  _loadUri view "http://www.haskell.org"

  _add win view

  uriEntry <- new Entry [_placeholderText := "Type the address to load here",
                         _widthChars := 50]
  on uriEntry Activate $ do
    uri <- uriEntry `get` _text
    _loadUri view uri

  header <- new HeaderBar [_showCloseButton := True,
                           _customTitle := uriEntry,
                           _title := "A simple WebKit browser"]
  _setTitlebar win (Just header)

  on view (PropertyNotify _estimatedLoadProgress) $ \_ -> do
    status <- view `get` _estimatedLoadProgress
    uriEntry `set` [_progressFraction := if status /= 1.0
                                         then status
                                         else 0]

  on view LoadChanged $ \event -> do
    putStrLn $ "Load: " <> show event

  on view LoadFailed $ \_ uri error -> do
    errMsg <- gerrorMessage error
    putStrLn . T.unpack $ "Error when reading \"" <> uri <> "\": " <> errMsg
    -- Keep processing, so WebKit shows the error page
    return False

  _showAll win

  Gtk.main
