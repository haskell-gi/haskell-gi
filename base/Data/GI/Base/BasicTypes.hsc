{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
  DeriveDataTypeable, TypeFamilies, ScopedTypeVariables,
  MultiParamTypeClasses, DataKinds, TypeOperators, UndecidableInstances,
  AllowAmbiguousTypes #-}

-- | Basic types used in the bindings.
module Data.GI.Base.BasicTypes
    (
     -- * Memory management
      ManagedPtr(..)
    , ManagedPtrNewtype(..)
    , BoxedPtr(..)
    , CallocPtr(..)
    , UnexpectedNullPointerReturn(..)

    -- * Basic GLib \/ GObject types
    , TypedObject(..)
    , GObject
    , GType(..)
    , CGType
    , gtypeName
    , GVariant(..)
    , GBoxed
    , BoxedEnum
    , BoxedFlags
    , GParamSpec(..)
    , noGParamSpec

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

import Control.Exception (Exception)

import Data.Coerce (coerce, Coercible)
import Data.IORef (IORef)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word

import Foreign.C (CString, peekCString)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr)

import Data.GI.Base.CallStack (CallStack)
import {-# SOURCE #-} Data.GI.Base.Overloading (HasParentTypes)

#include <glib-object.h>

-- | Thin wrapper over `ForeignPtr`, supporting the extra notion of
-- `disowning`, that is, not running the finalizers associated with
-- the foreign ptr.
data ManagedPtr a = ManagedPtr {
      managedForeignPtr :: ForeignPtr a
    , managedPtrAllocCallStack :: Maybe CallStack
    -- ^ `CallStack` for the call that created the pointer.
    , managedPtrIsDisowned :: IORef (Maybe CallStack)
    -- ^ When disowned, the `CallStack` for the disowning call.
    }

-- | Two 'ManagedPtr's are equal if they wrap the same underlying
-- C 'Ptr'.
instance Eq (ManagedPtr a) where
  a == b = managedForeignPtr a == managedForeignPtr b

-- | A constraint ensuring that the given type is a newtype over a
-- `ManagedPtr`.
class Coercible a (ManagedPtr ()) => ManagedPtrNewtype a where
  toManagedPtr :: a -> ManagedPtr a

-- | A default instance for `IsManagedPtr` for newtypes over `ManagedPtr`.
instance {-# OVERLAPPABLE #-} Coercible a (ManagedPtr ()) => ManagedPtrNewtype a where
  toManagedPtr = coerce
-- Notice that the Coercible here above to ManagedPtr (), instead of
-- "ManagedPtr a", which would be the most natural thing. Both are
-- representationally equivalent, so this is not a big deal. This is
-- to work around a problem in ghc 7.10:
-- https://ghc.haskell.org/trac/ghc/ticket/10715
--
-- Additionally, a simpler approach would be to simply do
--
-- > type IsManagedPtr a = Coercible a (ManagedPtr ())
--
-- but this requires the constructor of the datatype to be in scope,
-- which is cumbersome (for instance, one often wants to call `castTo`
-- on the results of `Gtk.builderGetObject`, which is a `GObject`,
-- whose constructor is not necessarily in scope when using `GI.Gtk`).
--
-- When we make the bindings we will always add explicit instances,
-- which cannot be hidden, avoiding the issue. We keep the default
-- instance for convenience when writing new object types.

-- | Pointers to chunks of memory which we know how to copy and
-- release.
class ManagedPtrNewtype a => BoxedPtr a where
  -- | Make a copy of the given `BoxedPtr`.
  boxedPtrCopy   :: a -> IO a
  -- | A pointer to a function for freeing the given pointer.
  boxedPtrFree   :: a -> IO ()

-- | A ptr to a memory block which we know how to allocate and fill
-- with zero.
class BoxedPtr a => CallocPtr a where
  -- | Allocate a zero-initialized block of memory for the given type.
  boxedPtrCalloc :: IO (Ptr a)

-- | A wrapped object that has an associated GLib type. This does not
-- necessarily descend from `GObject`, that constraint is implemented
-- by `GObject` below.
class HasParentTypes a => TypedObject a where
  -- | The `GType` for this object.
  glibType :: IO GType

-- | Chunks of memory whose allocation/deallocation info has been
-- registered with the GLib type system.
class (ManagedPtrNewtype a, TypedObject a) => GBoxed a

-- | A wrapped `GObject`, or any other type that descends from it.
class (ManagedPtrNewtype a, TypedObject a) => GObject a

-- | Enums with an associated `GType`.
class TypedObject a => BoxedEnum a

-- | Flags with an associated `GType`.
class TypedObject a => BoxedFlags a

-- | A type identifier in the GLib type system. This is the low-level
-- type associated with the representation in memory, when using this
-- on the Haskell side use `GType` below.
type CGType = #type GType

-- | A newtype for use on the Haskell side.
newtype GType = GType {gtypeToCGType :: CGType}
  deriving (Eq, Show)

foreign import ccall "g_type_name" g_type_name :: GType -> IO CString

-- | Get the name assigned to the given `GType`.
gtypeName :: GType -> IO String
gtypeName gtype = g_type_name gtype >>= peekCString

-- | A common omission in the introspection data is missing (nullable)
-- annotations for return types, when they clearly are nullable. (A
-- common idiom is "Returns: valid value, or %NULL if something went
-- wrong.")
--
-- Haskell wrappers will raise this exception if the return value is
-- an unexpected `Foreign.Ptr.nullPtr`.
data UnexpectedNullPointerReturn =
    UnexpectedNullPointerReturn { nullPtrErrorMsg :: T.Text }
                                deriving (Typeable)

instance Show UnexpectedNullPointerReturn where
  show r = T.unpack (nullPtrErrorMsg r)

instance Exception UnexpectedNullPointerReturn

-- | A <https://developer.gnome.org/glib/stable/glib-GVariant.html GVariant>. See "Data.GI.Base.GVariant" for further methods.
newtype GVariant = GVariant (ManagedPtr GVariant)

-- | A <https://developer.gnome.org/gobject/stable/gobject-GParamSpec.html GParamSpec>. See "Data.GI.Base.GParamSpec" for further methods.
newtype GParamSpec = GParamSpec (ManagedPtr GParamSpec)

-- | A convenient synonym for @Nothing :: Maybe GParamSpec@.
noGParamSpec :: Maybe GParamSpec
noGParamSpec = Nothing

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
