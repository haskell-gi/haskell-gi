-- | A version of @Gtk.Box@ which adds a (construct-only) ‘children’
-- property to set the list of children on creation.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Declarative.Gtk.Box
  ( DeclarativeBox(..)
  , IsDeclarativeBox
  , toDeclarativeBox
  ) where

import Data.GI.Base (GObject, TypedObject(glibType), ManagedPtr(..),
                     unsafeCastTo, withManagedPtr)
import Data.GI.Base.Attributes (AttrInfo(..), AttrOpTag(..))
import Data.GI.Base.GValue (GValueConstruct(..), newGValue, take_stablePtr)
import Data.GI.Base.GObject (DerivedGObject(..), GObjectClass(..),
                             registerGType, gobjectInstallProperty)
import Data.GI.Base.GParamSpec (PropertyInfo(..),
                                GParamFlag(..))
import qualified Data.GI.Base.Overloading as O
import Data.GI.Base.GType (gtypeStablePtr)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.StablePtr (newStablePtr)

import GHC.OverloadedLabels as OL

#if MIN_VERSION_base(4,13,0)
import qualified GHC.Records as R
#endif

import qualified GI.Gtk as Gtk

newtype DeclarativeBox = DeclarativeBox (ManagedPtr DeclarativeBox)

instance TypedObject DeclarativeBox where
  glibType = registerGType DeclarativeBox

instance GObject DeclarativeBox

-- Information for the type system. This will be picked up by
-- 'registerGType' above.
instance DerivedGObject DeclarativeBox where
  -- The parent type.
  type GObjectParentType DeclarativeBox = Gtk.Box
  -- Every custom type has associated private data, which can be of
  -- any type.
  type GObjectPrivateData DeclarativeBox = ()

  objectTypeName = "HaskellGI-DeclarativeBox"

  -- This is run when the class is instantiated for the first time,
  -- typically when the first object of this type is created.
  --
  -- This is the place to register custom properties and signals for
  -- the object.
  objectClassInit = declarativeBoxClassInit

  -- There's no instance setup needed.
  objectInstanceInit _ _ = pure ()

-- Our type descends from a parent type, and implements various
-- interfaces (by virtue of descending from its parent type), make
-- sure that the type system knows about that. First we make our type
-- an instance of O.HasParentTypes (having this class leads to better
-- error messages).
instance O.HasParentTypes DeclarativeBox

-- We declare what our parent types are here. This is a type-level
-- list of every type that we can be safely cast to. This is our
-- direct parent and its ancestors, and includes implemented
-- interfaces too.
type instance O.ParentTypes DeclarativeBox = Gtk.Box ': O.ParentTypes Gtk.Box

-- The following is a typeclass that encodes whether a given type
-- descends from us.
class (GObject o, O.IsDescendantOf DeclarativeBox o) => IsDeclarativeBox o
instance (GObject o, O.IsDescendantOf DeclarativeBox o) => IsDeclarativeBox o

-- If we have a type that descends from us, it can be safely cast to
-- our type.
toDeclarativeBox :: (MonadIO m, IsDeclarativeBox o) => o -> m DeclarativeBox
toDeclarativeBox = liftIO . unsafeCastTo DeclarativeBox

-- The list of children to append to the box upon construction, given
-- as a list of actions that construct the widgets.
childrenProperty :: PropertyInfo DeclarativeBox [IO Gtk.Widget]
childrenProperty =
  PropertyInfo { name   = "children"
               , nick   = "Children"
               , blurb  = "Children to append to the box upon construction"
               , setter = setChildren
               , getter = error "‘children’ is not a readable property"
               , flags  = Just [GParamWritable, GParamConstructOnly]
               }

-- Set the children of the box
setChildren :: DeclarativeBox -> [IO Gtk.Widget] -> IO ()
setChildren _ [] = pure ()
setChildren box (firstAction : rest) =
  firstAction >>= Gtk.boxAppend box >> setChildren box rest

-- Tell the type system about the “children” property, so we can use
-- the overloading syntax.
data ChildrenAttrInfo

-- This instance encodes the information for the property at the type
-- level.
instance AttrInfo ChildrenAttrInfo where
  -- This attribute can only be constructed.
  type AttrAllowedOps ChildrenAttrInfo = '[ 'AttrConstruct ]

  -- For which types can we get/set the attribute. Anything deriving
  -- from DeclarativeBox will do.
  type AttrBaseTypeConstraint ChildrenAttrInfo = IsDeclarativeBox

  -- Which type does 'get' on the property return. By default this is
  -- also the type that 'set' and 'new' accept.
  type AttrGetType ChildrenAttrInfo = [IO Gtk.Widget]

  -- Text description for the attribute, for use in error messages.
  type AttrLabel ChildrenAttrInfo = "children"

  -- Type defining the attribute, for use in error messages.
  type AttrOrigin ChildrenAttrInfo = DeclarativeBox

  -- Construct a 'GValue' containing the argument, tagged by the
  -- associated property name.
  attrConstruct children = do
    gv <- newGValue gtypeStablePtr
    withManagedPtr gv $ \gvPtr ->
      newStablePtr children >>= take_stablePtr gvPtr
    return $ GValueConstruct "children" gv

-- Allow the overloaded attribute syntax to work. In this case we
-- inherit all attributes of our parent type, and add the children
-- property.
instance O.HasAttributeList DeclarativeBox
type instance O.AttributeList DeclarativeBox =
  '("children", ChildrenAttrInfo) ': O.AttributeList Gtk.Box

-- Support overloaded signals. The following says that we support all
-- signals of our parent type.
type instance O.SignalList DeclarativeBox = O.SignalList Gtk.Box

-- Support for overloaded methods. We support the same as our parent
-- type.
type family ResolveDeclarativeBoxMethod t o where
  ResolveDeclarativeBoxMethod t o = Gtk.ResolveBoxMethod t o

#if MIN_VERSION_base(4,13,0)
{- The circular instance trick is to avoid the liberal coverage
condition. We should be using DYSFUNCTIONAL pragmas instead, once
those are implemented:
https://github.com/ghc-proposals/ghc-proposals/pull/374
-}
instance (info ~ ResolveDeclarativeBoxMethod method DeclarativeBox,
          O.OverloadedMethod info DeclarativeBox p,
          R.HasField method DeclarativeBox p)
    => R.HasField method DeclarativeBox p where
  getField = O.overloadedMethod @info
#endif

-- Make overloaded labels applied to DeclarativeBoxs resolve to
-- methods.
instance (info ~ ResolveDeclarativeBoxMethod t DeclarativeBox,
          O.OverloadedMethod info DeclarativeBox p)
         => OL.IsLabel t (DeclarativeBox -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

-- This is useful for debugging
instance (info ~ ResolveDeclarativeBoxMethod t DeclarativeBox,
          O.OverloadedMethodInfo info DeclarativeBox)
         => OL.IsLabel t (O.MethodProxy info DeclarativeBox) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

-- This method is run at class creation time.
declarativeBoxClassInit :: GObjectClass -> IO ()
declarativeBoxClassInit klass =
  -- Install the “children” property.
  gobjectInstallProperty klass childrenProperty
