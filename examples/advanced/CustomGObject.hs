{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

import Data.GI.Base

import Data.GI.Base.GObject (DerivedGObject(..), registerGType)

import qualified GI.Gtk as Gtk

newtype CustomLabel = CustomLabel (ManagedPtr CustomLabel)

instance Gtk.IsWidget CustomLabel
instance Gtk.IsLabel CustomLabel

instance GObject CustomLabel where
  gobjectType = registerGType CustomLabel

instance DerivedGObject CustomLabel where
  type GObjectParentType  CustomLabel = Gtk.Label
  type GObjectPrivateData CustomLabel = Int

  objectTypeName = "CustomLabel"
  objectClassInit = \_klass -> return ()
  objectInstanceInit = \_klass -> return 42

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Custom GObject example"]

  _ <- on win #destroy Gtk.mainQuit

  custom <- new CustomLabel []
  Gtk.labelSetText custom "Hi"
  #add win custom

  #showAll win

  Gtk.main
