import BasicPrelude hiding (on, error)

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import GI.GtkSignals ()
import GI.WebKit
import GI.WebKitSignals ()
import GI.WebKitAttributes ()
import qualified GI.GLib as GLib

import GI.Properties
import GI.Signals
import GI.Utils.Base

import Data.Text (pack)
import System.Environment (getProgName)

main :: IO ()
main = do
  progName <- pack <$> getProgName
  args <- getArgs
  _ <- Gtk.init $ Just (progName : args)

  win <- new Window [_type := WindowTypeToplevel,
                     _iconName := "applications-haskell",
                     _defaultWidth := 1024,
                     _defaultHeight := 768]
  on win Destroy mainQuit

  view <- new WebView []
  on view CloseWebView $ (widgetDestroy win >> return True)
  webViewLoadUri view "http://www.haskell.org"

  scroll <- new ScrolledWindow [_child := view]
  containerAdd win scroll

  uriEntry <- new Entry [_placeholderText := "Type the address to load here",
                         _widthChars := 50]
  on uriEntry Activate $ do
    uri <- uriEntry `get` _text
    webViewLoadUri view uri

  header <- new HeaderBar [_showCloseButton := True,
                           _customTitle := uriEntry,
                           _title := "A simple WebKit browser"]
  windowSetTitlebar win (Just header)

  on view (PropertyNotify _loadStatus) $ \_ -> do
    view `get` _loadStatus >>= print

  on view LoadError $ \_ uri error -> do
    errMsg <- GLib.errorReadMessage error
    putStrLn $ "Error when reading \"" <> uri <> "\": " <> errMsg
    -- Keep processing, so WebKit shows the error page
    return False

  widgetShowAll win

  Gtk.main
