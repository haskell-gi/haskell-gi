{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
  DeriveDataTypeable, TypeFamilies, ScopedTypeVariables #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances #-}
#endif
-- | Basic types used in the bindings.
module Data.GI.Base.BasicTypes
    (
      -- * GType related
      module Data.GI.Base.GType         -- reexported for convenience

     -- * Memory management

    , ForeignPtrNewtype
    , BoxedObject(..)
    , BoxedEnum(..)
    , BoxedFlags(..)
    , GObject(..)
    , WrappedPtr(..)
    , UnexpectedNullPointerReturn(..)
    , NullToNothing(..)

    -- * Basic GLib \/ GObject types
    , GVariant(..)
    , GParamSpec(..)

    , GArray(..)
    , GPtrArray(..)
    , GByteArray(..)
    , GHashTable(..)
    , GList(..)
    , g_list_free
    , GSList(..)
    , g_slist_free

    , IsGFlag

    , PtrWrapped(..)
    , GDestroyNotify
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (Exception, catch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (Coercible)
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr)

#if MIN_VERSION_base(4,9,0)
import GHC.TypeLits
#endif

import Data.GI.Base.GType

-- | A constraint ensuring that the given type is coercible to a
-- ForeignPtr. It will hold for newtypes of the form
--
-- > newtype Foo = Foo (ForeignPtr Foo)
--
-- which is the typical shape of wrapped 'GObject's.
type ForeignPtrNewtype a = Coercible a (ForeignPtr ())
-- Notice that the Coercible here is to ForeignPtr (), instead of
-- "ForeignPtr a", which would be the most natural thing. Both are
-- representationally equivalent, so this is not a big deal. This is
-- to work around a problem in ghc 7.10:
-- https://ghc.haskell.org/trac/ghc/ticket/10715

-- | Wrapped boxed structures, identified by their `GType`.
class ForeignPtrNewtype a => BoxedObject a where
    boxedType :: a -> IO GType -- This should not use the value of its
                               -- argument.

-- | Enums with an associated `GType`.
class BoxedEnum a where
    boxedEnumType :: a -> IO GType

-- | Flags with an associated `GType`.
class BoxedFlags a where
    boxedFlagsType :: Proxy a -> IO GType

-- | Pointers to structs/unions without an associated `GType`.
class ForeignPtrNewtype a => WrappedPtr a where
    -- | Allocate a zero-initialized block of memory for the given type.
    wrappedPtrCalloc :: IO (Ptr a)
    -- | Make a copy of the given pointer.
    wrappedPtrCopy   :: Ptr a -> IO (Ptr a)
    -- | A pointer to a function for freeing the given pointer, or
    -- `Nothing` is the memory associated to the pointer does not need
    -- to be freed.
    wrappedPtrFree   :: Maybe (FunPtr (Ptr a -> IO ()))

-- | A wrapped `GObject`.
class ForeignPtrNewtype a => GObject a where
    -- | Whether the `GObject` is a descendent of <https://developer.gnome.org/gobject/stable/gobject-The-Base-Object-Type.html#GInitiallyUnowned GInitiallyUnowned>.
    gobjectIsInitiallyUnowned :: a -> Bool
    -- | The `GType` for this object.
    gobjectType :: a -> IO GType

-- Raise a more understandable type error whenever the `GObject a`
-- constraint is imposed on a type which has no such instance. This
-- helps in the common case where one passes a wrong type (such as
-- `Maybe Widget`) into a function with a `IsWidget a`
-- constraint. Without this type error, the resulting type error is
-- much less understandable, since GHC complains (at length) about a
-- missing type family instance for `ParentTypes`.
#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPABLE #-}
    (TypeError ('Text "Type ‘" ':<>: 'ShowType a ':<>:
                'Text "’ does not descend from GObject."), ForeignPtrNewtype a)
    => GObject a where
    gobjectIsInitiallyUnowned = undefined
    gobjectType = undefined
#endif

-- | A common omission in the introspection data is missing (nullable)
-- annotations for return types, when they clearly are nullable. (A
-- common idiom is "Returns: valid value, or %NULL if something went
-- wrong.")
--
-- Haskell wrappers will raise this exception if the return value is
-- an unexpected `Foreign.Ptr.nullPtr`.
data UnexpectedNullPointerReturn =
    UnexpectedNullPointerReturn { nullPtrErrorMsg :: T.Text }
                                deriving (Show, Typeable)

instance Exception UnexpectedNullPointerReturn

type family UnMaybe a :: * where
    UnMaybe (Maybe a) = a
    UnMaybe a         = a

class NullToNothing a where
    -- | Some functions are not marked as having a nullable return type
    -- in the introspection data.  The result is that they currently do
    -- not return a Maybe type.  This functions lets you work around this
    -- in a way that will not break when the introspection data is fixed.
    --
    -- When you want to call a `someHaskellGIFunction` that may return null
    -- wrap the call like this.
    --
    -- > nullToNothing (someHaskellGIFunction x y)
    --
    -- The result will be a Maybe type even if the introspection data has
    -- not been fixed for `someHaskellGIFunction` yet.
    nullToNothing :: MonadIO m => IO a -> m (Maybe (UnMaybe a))

instance
#if MIN_VERSION_base(4,8,0)
    {-# OVERLAPPABLE #-}
#endif
    a ~ UnMaybe a => NullToNothing a where
        nullToNothing f = liftIO $
            (Just <$> f) `catch` (\(_::UnexpectedNullPointerReturn) -> return Nothing)

instance NullToNothing (Maybe a) where
    nullToNothing = liftIO

-- | A <https://developer.gnome.org/glib/stable/glib-GVariant.html GVariant>. See "Data.GI.Base.GVariant" for further methods.
newtype GVariant = GVariant (ForeignPtr GVariant)

-- | A <https://developer.gnome.org/gobject/stable/gobject-GParamSpec.html GParamSpec>. See "Data.GI.Base.GParamSpec" for further methods.
newtype GParamSpec = GParamSpec (ForeignPtr GParamSpec)

-- | An enum usable as a flag for a function.
class Enum a => IsGFlag a

-- | A <https://developer.gnome.org/glib/stable/glib-Arrays.html GArray>. Marshalling for this type is done in "Data.GI.Base.BasicConversions", it is mapped to a list on the Haskell side.
data GArray a = GArray (Ptr (GArray a))

-- | A <https://developer.gnome.org/glib/stable/glib-Pointer-Arrays.html GPtrArray>. Marshalling for this type is done in "Data.GI.Base.BasicConversions", it is mapped to a list on the Haskell side.
data GPtrArray a = GPtrArray (Ptr (GPtrArray a))

-- | A <https://developer.gnome.org/glib/stable/glib-Byte-Arrays.html GByteArray>. Marshalling for this type is done in "Data.GI.Base.BasicConversions", it is packed to a 'Data.ByteString.ByteString' on the Haskell side.
data GByteArray = GByteArray (Ptr GByteArray)

-- | A <https://developer.gnome.org/glib/stable/glib-Hash-Tables.html GHashTable>. It is mapped to a 'Data.Map.Map' on the Haskell side.
data GHashTable a b = GHashTable (Ptr (GHashTable a b))

-- | A <https://developer.gnome.org/glib/stable/glib-Doubly-Linked-Lists.html GList>, mapped to a list on the Haskell side. Marshalling is done in "Data.GI.Base.BasicConversions".
data GList a = GList (Ptr (GList a))

-- | A <https://developer.gnome.org/glib/stable/glib-Singly-Linked-Lists.html GSList>, mapped to a list on the Haskell side. Marshalling is done in "Data.GI.Base.BasicConversions".
data GSList a = GSList (Ptr (GSList a))

-- | Some APIs, such as `GHashTable`, pass around scalar types
-- wrapped into a pointer. We encode such a type as follows.
newtype PtrWrapped a = PtrWrapped {unwrapPtr :: Ptr a}

-- | Destroy the memory associated with a given pointer.
type GDestroyNotify a = FunPtr (Ptr a -> IO ())

-- | Free the given 'GList'.
foreign import ccall "g_list_free" g_list_free ::
    Ptr (GList a) -> IO ()

-- | Free the given 'GSList'.
foreign import ccall "g_slist_free" g_slist_free ::
    Ptr (GSList a) -> IO ()
