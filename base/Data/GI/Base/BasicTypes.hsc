{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
  DeriveDataTypeable, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Basic types used in the bindings.
module Data.GI.Base.BasicTypes
    (
     -- * Memory management
      ManagedPtr(..)
    , ManagedPtrNewtype
    , BoxedObject(..)
    , BoxedEnum(..)
    , BoxedFlags(..)
    , WrappedPtr(..)
    , UnexpectedNullPointerReturn(..)

    -- * Basic GLib \/ GObject types
    , GObject(..)
    , GType(..)
    , CGType
    , gtypeName
    , GVariant(..)
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

import Data.Coerce (Coercible)
import Data.IORef (IORef)
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word

import Foreign.C (CString, peekCString)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr)

import Data.GI.Base.CallStack (CallStack)
import Data.GI.Base.Overloading (HasParentTypes)

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

-- | A constraint ensuring that the given type is coercible to a
-- ManagedPtr. It will hold for newtypes of the form
--
-- > newtype Foo = Foo (ManagedPtr Foo)
--
-- which is the typical shape of wrapped 'GObject's.
type ManagedPtrNewtype a = Coercible a (ManagedPtr ())
-- Notice that the Coercible here is to ManagedPtr (), instead of
-- "ManagedPtr a", which would be the most natural thing. Both are
-- representationally equivalent, so this is not a big deal. This is
-- to work around a problem in ghc 7.10:
-- https://ghc.haskell.org/trac/ghc/ticket/10715

-- | Wrapped boxed structures, identified by their `GType`.
class ManagedPtrNewtype a => BoxedObject a where
    boxedType :: a -> IO GType -- This should not use the value of its
                               -- argument.

-- | Enums with an associated `GType`.
class BoxedEnum a where
    boxedEnumType :: a -> IO GType

-- | Flags with an associated `GType`.
class BoxedFlags a where
    boxedFlagsType :: Proxy a -> IO GType

-- | Pointers to structs/unions without an associated `GType`.
class ManagedPtrNewtype a => WrappedPtr a where
    -- | Allocate a zero-initialized block of memory for the given type.
    wrappedPtrCalloc :: IO (Ptr a)
    -- | Make a copy of the given `WrappedPtr`.
    wrappedPtrCopy   :: a -> IO a
    -- | A pointer to a function for freeing the given pointer, or
    -- `Nothing` is the memory associated to the pointer does not need
    -- to be freed.
    wrappedPtrFree   :: Maybe (GDestroyNotify a)

-- | A wrapped `GObject`.
class (ManagedPtrNewtype a, HasParentTypes a) => GObject a where
    -- | The `GType` for this object.
    gobjectType :: IO GType

-- | A type identifier in the GLib type system. This is the low-level
-- type associated with the representation in memory, when using this
-- on the Haskell side use `GType` below.
type CGType = #type GType

-- | A newtype for use on the haskell side.
newtype GType = GType {gtypeToCGType :: CGType}

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
