{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module GI.Utils.BasicTypes
    ( GType(..)
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

    , FunPtrNewtype
    , BoxedObject(..)
    , BoxedEnum(..)
    , GObject(..)
    , GVariant(..)
    , GParamSpec(..)

    , IsGFlag

    , GArray(..)
    , GPtrArray(..)
    , GByteArray(..)
    , GHashTable(..)
    , GList(..)
    , g_list_free
    , GSList(..)
    , g_slist_free

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
type FunPtrNewtype a = Coercible a (ForeignPtr a)

-- | Wrapped boxed structures, identified by their `GType`.
class FunPtrNewtype a => BoxedObject a where
    boxedType :: a -> IO GType -- This should not use the value of its
                               -- argument.

-- | Enums with an associated `GType`.
class BoxedEnum a where
    boxedEnumType :: a -> IO GType

-- | A wrapped `GObject`.
class FunPtrNewtype a => GObject a where
    gobjectIsInitiallyUnowned :: a -> Bool
    gobjectType :: a -> IO GType

newtype GVariant = GVariant (ForeignPtr GVariant)

newtype GParamSpec = GParamSpec (ForeignPtr GParamSpec)

class Enum a => IsGFlag a

data GArray a = GArray (Ptr (GArray a))
data GPtrArray a = GPtrArray (Ptr (GPtrArray a))
data GByteArray = GByteArray (Ptr GByteArray)
data GHashTable a b = GHashTable (Ptr (GHashTable a b))
data GList a = GList (Ptr (GList a))
data GSList a = GSList (Ptr (GSList a))

-- | Some APIs, such as `GHashTable`, pass around scalar types
-- wrapped into a pointer. We encode such a type as follows.
newtype PtrWrapped a = PtrWrapped {unwrapPtr :: Ptr a}

-- | Destroy the memory associated with a given pointer.
type GDestroyNotify a = FunPtr (Ptr a -> IO ())

foreign import ccall "g_list_free" g_list_free ::
    Ptr (GList a) -> IO ()

foreign import ccall "g_slist_free" g_slist_free ::
    Ptr (GSList a) -> IO ()
