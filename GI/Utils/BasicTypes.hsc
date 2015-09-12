{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances #-}
-- | Basic types used in the bindings.
module GI.Utils.BasicTypes
    (
     -- * GType related
     GType(..)
    , CGType

    , gtypeName

    , gtypeString
    , gtypePointer
    , gtypeInt32
    , gtypeUInt32
    , gtypeInt64
    , gtypeUInt64
    , gtypeFloat
    , gtypeDouble
    , gtypeBoolean
    , gtypeGType
    , gtypeStrv
    , gtypeBoxed
    , gtypeObject

     -- * Memory management

    , ForeignPtrNewtype
    , BoxedObject(..)
    , BoxedEnum(..)
    , GObject(..)

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

import Data.Coerce (Coercible)
import Data.Word
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.C.String (CString, peekCString)

#include <glib-object.h>

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

-- | `GType` of strings.
gtypeString :: GType
gtypeString = GType #const G_TYPE_STRING

-- | `GType` of pointers.
gtypePointer :: GType
gtypePointer = GType #const G_TYPE_POINTER

-- | `GType` for signed integers.
gtypeInt32 :: GType
gtypeInt32 = GType #const G_TYPE_INT

-- | `GType` for unsigned integers.
gtypeUInt32 :: GType
gtypeUInt32 = GType #const G_TYPE_UINT

-- | `GType` for signed 64 bit integers.
gtypeInt64 :: GType
gtypeInt64 = GType #const G_TYPE_INT64

-- | `GType` for unsigned 64 bit integers.
gtypeUInt64 :: GType
gtypeUInt64 = GType #const G_TYPE_UINT64

-- | `GType` for floating point values.
gtypeFloat :: GType
gtypeFloat = GType #const G_TYPE_FLOAT

-- | `GType` for gdouble.
gtypeDouble :: GType
gtypeDouble = GType #const G_TYPE_DOUBLE

-- | `GType` corresponding to gboolean.
gtypeBoolean :: GType
gtypeBoolean = GType #const G_TYPE_BOOLEAN

-- | `GType` corresponding to a `GType` itself.
gtypeGType :: GType
gtypeGType = GType #const G_TYPE_GTYPE

-- | `GType` for a NULL terminated array of strings.
gtypeStrv :: GType
gtypeStrv = GType #const G_TYPE_STRV

-- | `GType` corresponding to a `BoxedObject`.
gtypeBoxed :: GType
gtypeBoxed = GType #const G_TYPE_BOXED

-- | `GType` corresponding to a `GObject`.
gtypeObject :: GType
gtypeObject = GType #const G_TYPE_OBJECT

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

-- | A wrapped `GObject`.
class ForeignPtrNewtype a => GObject a where
    -- | Whether the `GObject` is a descendent of <https://developer.gnome.org/gobject/stable/gobject-The-Base-Object-Type.html#GInitiallyUnowned GInitiallyUnowned>.
    gobjectIsInitiallyUnowned :: a -> Bool
    -- | The `GType` for this object.
    gobjectType :: a -> IO GType

-- | A <https://developer.gnome.org/glib/stable/glib-GVariant.html GVariant>. See "GI.Utils.GVariant" for further methods.
newtype GVariant = GVariant (ForeignPtr GVariant)

-- | A <https://developer.gnome.org/gobject/stable/gobject-GParamSpec.html GParamSpec>. See "GI.Utils.GParamSpec" for further methods.
newtype GParamSpec = GParamSpec (ForeignPtr GParamSpec)

-- | An enum usable as a flag for a function.
class Enum a => IsGFlag a

-- | A <https://developer.gnome.org/glib/stable/glib-Arrays.html GArray>. Marshalling for this type is done in "GI.Utils.BasicConversions", it is mapped to a list on the Haskell side.
data GArray a = GArray (Ptr (GArray a))

-- | A <https://developer.gnome.org/glib/stable/glib-Pointer-Arrays.html GPtrArray>. Marshalling for this type is done in "GI.Utils.BasicConversions", it is mapped to a list on the Haskell side.
data GPtrArray a = GPtrArray (Ptr (GPtrArray a))

-- | A <https://developer.gnome.org/glib/stable/glib-Byte-Arrays.html GByteArray>. Marshalling for this type is done in "GI.Utils.BasicConversions", it is packed to a 'Data.ByteString.ByteString' on the Haskell side.
data GByteArray = GByteArray (Ptr GByteArray)

-- | A <https://developer.gnome.org/glib/stable/glib-Hash-Tables.html GHashTable>. It is mapped to a 'Data.Map.Map' on the Haskell side.
data GHashTable a b = GHashTable (Ptr (GHashTable a b))

-- | A <https://developer.gnome.org/glib/stable/glib-Doubly-Linked-Lists.html GList>, mapped to a list on the Haskell side. Marshalling is done in "GI.Utils.BasicConversions".
data GList a = GList (Ptr (GList a))

-- | A <https://developer.gnome.org/glib/stable/glib-Singly-Linked-Lists.html GSList>, mapped to a list on the Haskell side. Marshalling is done in "GI.Utils.BasicConversions".
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
