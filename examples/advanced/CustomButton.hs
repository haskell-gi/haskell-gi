-- Example of creating a new type.

{-# LANGUAGE OverloadedStrings #-}

-- Haskell-gi embeds much of the subclassing information on the type
-- level. The following extensions allow us to do the needful.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

module CustomButton
  ( CustomButton(..)
  , IsCustomButton
  , toCustomButton
  ) where

import Data.GI.Base (GObject(..), ManagedPtr(..), unsafeCastTo)

import Data.GI.Base.GObject (DerivedGObject(..), registerGType)
import qualified Data.GI.Base.Overloading as O
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified GI.Gtk as Gtk

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

-- Our type descends from a parent type, and implements various
-- interfaces (by virtue of descending from its parent type), make
-- sure that the type system knows about that. First we make our type
-- an instance of O.HasParentTypes (having this class leads to better
-- error messages).
instance O.HasParentTypes CustomButton

-- We declare what our parent types are here. This is a type-level
-- list of every type that we can be safely cast to. This is our
-- direct parent and its ancestors, and includes implemented
-- interfaces too.
type instance O.ParentTypes CustomButton = Gtk.Button ': O.ParentTypes Gtk.Button

-- The following is a typeclass that encodes whether a given type
-- descends from us.
class (GObject o, O.IsDescendantOf CustomButton o) => IsCustomButton o
instance (GObject o, O.IsDescendantOf CustomButton o) => IsCustomButton o

-- If we have a type that descends from us, it can be safely cast to
-- our type.
toCustomButton :: (MonadIO m, IsCustomButton o) => o -> m CustomButton
toCustomButton = liftIO . unsafeCastTo CustomButton

-- Allow the overloaded attribute syntax to work. In this case inherit
-- all attributes of our parent type.
instance O.HasAttributeList CustomButton
type instance O.AttributeList CustomButton = O.AttributeList Gtk.Button

-- Support overloaded signals. The following says that we support all
-- signals of our parent type.
type instance O.SignalList CustomButton = O.SignalList Gtk.Button
