{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import           Data.Maybe (fromJust)
import           Data.Text (Text)
-- External resource bundles Step 2: Reactivate next 2 lines
-- import qualified GI.Gio as Gio
-- import           System.Environment 
-- import           System.FilePath 

import           Data.GI.Base
import qualified GI.Gtk as Gtk

unsafeGetObjectWithCast :: GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
unsafeGetObjectWithCast builder name totype_ = #getObject builder name >>= unsafeCastTo totype_ . fromJust

-- External resource bundles Step 2: Reactivate next 4 lines
-- resourceFileName :: IO FilePath
-- resourceFileName = getExecutablePath 
--                  >>= return . dropFileName 
--                  >>= return . (flip combine) "resource-bundle.gresource"

openAbout :: Gtk.AboutDialog -> IO ()
openAbout aboutDialog = #run aboutDialog >> return ()

appActivate :: Gtk.Application -> IO ()
appActivate app = do
  -- External resource bundles Step 2: Reactivate next 2 lines
  -- resource <- Gio.resourceLoad =<< resourceFileName
  -- Gio.resourcesRegister resource

  builder <- Gtk.builderNewFromResource "/haskell-gi/examples/resource-bundle/resource-bundle.ui"
  appWindow <- unsafeGetObjectWithCast builder "applicationwindow1" Gtk.ApplicationWindow
  #addWindow app appWindow
  aboutDialog <- unsafeGetObjectWithCast builder "aboutDialog" Gtk.AboutDialog
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
  app <- new Gtk.Application [#applicationId := "haskell-gi.examples.resource-bundle"]
  on app #activate $ appActivate app

  #run app Nothing
  return ()
