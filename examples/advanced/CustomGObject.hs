{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.GI.Base.GObject (DerivedGObject(..), registerGType,
                             gobjectGetPrivateData, gobjectSetPrivateData)
import Data.GI.Base.Overloading (HasAttributeList, AttributeList,
                                 SignalList, UnknownAncestorError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Atk as Atk
import qualified GI.GObject as GObject

-- The basic type definition. This should always have the form
--
-- > newtype X = X (ManagedPtr X)
--
-- for any type that you wish to define.
newtype CustomButton = CustomButton (ManagedPtr CustomButton)

-- Declare that the new type is a GObject, with a type to be
-- registered at runtime. The information on the type will be declared
-- in the 'DerivedGObject' instance below.
instance GObject CustomButton where
  gobjectType = registerGType CustomButton

-- Information for the type system. This will be picked up by
-- 'registerGType' above.
instance DerivedGObject CustomButton where
  -- The parent type.
  type GObjectParentType  CustomButton = Gtk.Button
  -- Every custom type has associated private data, which can be of
  -- any type.
  type GObjectPrivateData CustomButton = Int

  -- Name of the type we are about to register. Make sure that it does
  -- not clash with an existing type name. See
  -- https://developer.gnome.org/gobject/stable/gtype-conventions.html
  -- for the naming conventions.
  objectTypeName = "HaskellGI-Example-CustomButton"

  -- This is run when the class is instantiated for the first time,
  -- typically when the first object of this type is created.
  --
  -- This is the place to register custom properties and signals for
  -- the object.
  objectClassInit = \_klass -> return ()

  -- This is run for each instance of the type (each new object of
  -- this type being created). The main goal of this function is to
  -- prepare the private data for each object.
  objectInstanceInit = \_klass -> return 42

-- In general you might eventually want to allow to subclass this
-- object. This is a typeclass whose instances are objects that
-- descend from this one.
class GObject o => IsCustomButton o where
instance IsCustomButton CustomButton

-- This default instance leads to better error messages.
instance {-# OVERLAPPABLE #-} (GObject a,
                               UnknownAncestorError CustomButton a) =>
    IsCustomButton a

-- Our type descends from parent types, and implements various
-- interfaces (by virtue of descending from its parent type), make
-- sure that the type system knows about that.
instance Gtk.IsButton CustomButton
instance Gtk.IsBin CustomButton
instance Gtk.IsContainer CustomButton
instance Gtk.IsWidget CustomButton
instance GObject.IsObject CustomButton
instance Atk.IsImplementorIface CustomButton
instance Gtk.IsActionable CustomButton
instance Gtk.IsActivatable CustomButton
instance Gtk.IsBuildable CustomButton

-- If we have one descendant type, it can be safely cast to our type.
toCustomButton :: (MonadIO m, IsCustomButton o) => o -> m CustomButton
toCustomButton = liftIO . unsafeCastTo CustomButton

-- Allow the overloaded attribute syntax to work. In this case inherit
-- all attributes of our parent type.
instance HasAttributeList CustomButton
type instance AttributeList CustomButton = AttributeList Gtk.Button

-- Support overloaded signals. The following says that we support all
-- signals of our parent type.
type instance SignalList CustomButton = SignalList Gtk.Button

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Custom GObject example"]

  _ <- on win #destroy Gtk.mainQuit

  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add win vbox

  button <- new CustomButton [#label := "Check private value"]
  _ <- on button #clicked $ do
    priv <- gobjectGetPrivateData button
    print priv
    gobjectSetPrivateData button (priv+1)

  #add vbox button

  #showAll win

  Gtk.main
