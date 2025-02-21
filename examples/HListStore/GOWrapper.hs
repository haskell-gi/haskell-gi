-- | Wrap arbitrary Haskell values into GObjects.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE CPP #-}

-- Haskell-gi embeds much of the subclassing information on the type
-- level. The following extensions allow us to do the needful.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

module GOWrapper
  ( GOWrapper(..)
  , goWrapHValue
  , goUnwrapHValue
  ) where

import qualified GI.GObject as GO

import Data.GI.Base (GObject, TypedObject(glibType), ManagedPtr(..))
import Data.GI.Base.CallStack (HasCallStack)
import Data.GI.Base.Constructible (Constructible)
import Data.GI.Base.GObject (DerivedGObject(..), registerGType,
                             constructGObject,
                             gobjectModifyPrivateData, gobjectGetPrivateData )
import qualified Data.GI.Base.Overloading as O

import GHC.TypeLits (TypeError, ErrorMessage(Text))

-- | The basic type, parameterised by the Haskell type being wrapped.
newtype GOWrapper a = GOWrapper (ManagedPtr (GOWrapper a))

-- Declare that the new type is a GObject, with a type to be
-- registered at runtime. The information on the type will be declared
-- in the 'DerivedGObject' instance below.
instance TypedObject (GOWrapper a) where
  glibType = registerGType @(GOWrapper a) GOWrapper

instance GObject (GOWrapper a)

-- We keep the given Haskell value here
data GOWrapperPrivate a =
  GOWrapperPrivate { goPrivateHValue :: Maybe a
                     -- ^ The value being wrapped.
                   }

-- Information for the type system. This will be picked up by
-- 'registerGType' above.
instance DerivedGObject (GOWrapper a) where
  -- The parent type.
  type GObjectParentType (GOWrapper a) = GO.Object

  -- Every custom type has associated private data, which can be of
  -- any type.
  type GObjectPrivateData (GOWrapper a) = GOWrapperPrivate a

  -- Name of the type we are about to register. We register a single
  -- type, that the wrapped value is of the right type is ensured on
  -- the Haskell side by the type argument ‘a’, and on the Haskell <->
  -- C boundary by using Dynamic values (see goGetHValue below).
  objectTypeName = "HaskellGI-GOWrapper"

  -- This is run when the class is instantiated for the first time,
  -- typically when the first object of this type is created.
  --
  -- This is the place to register custom properties and signals for
  -- the object.
  objectClassInit = \_ -> return ()

  -- This is run for each instance of the type (each new object of
  -- this type being created). The main goal of this function is to
  -- prepare the private data for each object.
  objectInstanceInit = \_ _ ->
       return $ (GOWrapperPrivate {goPrivateHValue = Nothing} :: GOWrapperPrivate a)

  -- List of interfaces we implement. There are tuples of the form
  -- (gtype, initilizer, maybeFinalizer). Both initializer and
  -- finalizer have signature Ptr () -> IO (), where the passed Ptr ()
  -- is to the interface struct.
  objectInterfaces = []

-- Our type descends from a parent type, and implements various
-- interfaces (by virtue of descending from its parent type), make
-- sure that the type system knows about that. First we make our type
-- an instance of O.HasParentTypes (having this class leads to better
-- error messages).
instance O.HasParentTypes (GOWrapper a)

-- We declare what our parent types are here. This is a type-level
-- list of every type that we can be safely cast to. This is our
-- direct parent and its ancestors, and includes implemented
-- interfaces too.
type instance O.ParentTypes (GOWrapper a) = '[GO.Object]

-- | Set the HValue.
goSetHValue :: GOWrapper a -> Maybe a -> IO ()
goSetHValue wrapper hvalue = gobjectModifyPrivateData wrapper
                             (\priv -> priv {goPrivateHValue = hvalue})

-- | Get the HValue, if it has been set before.
goGetHValue :: GOWrapper a -> IO (Maybe a)
goGetHValue wrapper = goPrivateHValue <$> gobjectGetPrivateData wrapper

-- | Wrap the given HValue into a freshly built GObject wrapper.
goWrapHValue :: a -> IO (GOWrapper a)
goWrapHValue hvalue = do
  wrapper <- constructGObject GOWrapper []
  goSetHValue wrapper (Just hvalue)
  return wrapper

-- | Unwrap the hvalue contained in the given GObject wrapper. It's
-- safe to call this multiple times.
goUnwrapHValue :: HasCallStack => GOWrapper a -> IO a
goUnwrapHValue wrapper = do
  maybeHValue <- goGetHValue wrapper
  case maybeHValue of
    Just hvalue -> return hvalue
    Nothing -> error $ "Empty wrapper, make sure you always construct GObject wrappers by using ‘goWrapHValue’."

-- | Make sure the wrapper is not constructed with `new', which does
-- not ensure that the private data contains an actual Haskell value.
instance {-# OVERLAPPING #-}
  TypeError ('Text "Use ‘goWrapHValue’ to construct GObject wrappers, instead of ‘new’.")
  => Constructible (GOWrapper a) tag where
  new = undefined
