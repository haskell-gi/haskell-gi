{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import           Data.Maybe (fromJust)
import           Data.Text (Text)

import           Data.GI.Base
import qualified GI.GdkPixbuf as GdkPixbuf  
import qualified GI.Gio as Gio 
import qualified GI.Gtk as Gtk

unsafeGetObjectWithCast :: GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
unsafeGetObjectWithCast builder name totype_ = #getObject builder name >>= unsafeCastTo totype_ . fromJust

resourceFileName :: FilePath -- [String] or Text tested
resourceFileName = "C:/Users/zmtemp/Desktop/haskell/gtk/haskell-gi/examples/introductory/src/resource-bundle/resource-bundle.gresource"

openAbout :: Gtk.AboutDialog -> IO ()
openAbout aboutDialog = #run aboutDialog >> return ()

appActivate :: Gtk.Application -> IO ()
appActivate app = do
  -- resource <- #load resourceFileName -- <== doesn't compile as no instance for load could be found
  resource <- Gio.resourceLoad resourceFileName
  Gio.resourcesRegister resource

  -- builder <- #newFromResource "/haskell-gi/examples/resource-bundle/resource-bundle.ui" <== does not compile due to multiple instances of #newFromResource
  builder <- Gtk.builderNewFromResource "/haskell-gi/examples/resource-bundle/resource-bundle.ui"
  appWindow <- unsafeGetObjectWithCast builder "applicationwindow1" Gtk.ApplicationWindow
  #addWindow app appWindow
  aboutDialog <- unsafeGetObjectWithCast builder "aboutDialog" Gtk.AboutDialog
  -- We manualy get the image for AboutDialog logo from the resource because GTK+ doesn't load it as per .gresource even though the logo file path is a resource:// url.
  -- logo <- #newFromResource "/haskell-gi/examples/resource-bundle/Gnome-compressed-240x240.svg.png" <== does not compile due to multiple instances of #newFromResource
  logo <- GdkPixbuf.pixbufNewFromResource "/haskell-gi/examples/resource-bundle/Gnome-compressed-240x240.svg.png"
  set aboutDialog [#logo := logo]
  aboutWidget <- unsafeCastTo Gtk.Widget aboutDialog
  on aboutWidget #deleteEvent $ \_ -> #hideOnDelete aboutWidget
  mitem <- unsafeGetObjectWithCast builder "about" Gtk.MenuItem
  on mitem #activate $ openAbout aboutDialog
  mitem <- unsafeGetObjectWithCast builder "quit" Gtk.MenuItem
  on mitem #activate $ #destroy appWindow

  #showAll appWindow
  return ()

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.resource-bundle"
                             , #flags := [Gio.ApplicationFlagsFlagsNone]
                             ]
  on app #activate $ appActivate app

  #run app Nothing
  return ()
