{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
  DeriveDataTypeable #-}
-- | Basic types used in the bindings.
module Data.GI.Base.BasicTypes
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
    , gtypeVariant
    , gtypeByteArray
    , gtypeInvalid

     -- * Memory management

    , ForeignPtrNewtype
    , BoxedObject(..)
    , BoxedEnum(..)
    , BoxedFlags(..)
    , GObject(..)
    , UnexpectedNullPointerReturn(..)

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

import Control.Exception (Exception)
import Data.Coerce (Coercible)
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Typeable (Typeable)
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

{-| [Note: compile-time vs run-time GTypes]

Notice that there are two types of GType's: the fundamental ones,
which are created with G_TYPE_MAKE_FUNDAMENTAL(n) and always have the
same runtime representation, and the ones that are registered in the
GObject type system at runtime, and whose `CGType` may change for each
program run (and generally does).

For the first type it is safe to use hsc to read the numerical values
of the CGType at compile type, but for the second type it is essential
to call the corresponding _get_type() function at runtime, and not use
the value of the corresponding "constant" at compile time via hsc.
-}

{- Fundamental types -}

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

-- | `GType` corresponding to a `BoxedObject`.
gtypeBoxed :: GType
gtypeBoxed = GType #const G_TYPE_BOXED

-- | `GType` corresponding to a `GObject`.
gtypeObject :: GType
gtypeObject = GType #const G_TYPE_OBJECT

-- | An invalid `GType` used as error return value in some functions
-- which return a `GType`.
gtypeInvalid :: GType
gtypeInvalid = GType #const G_TYPE_INVALID

-- | The `GType` corresponding to a `GVariant`.
gtypeVariant :: GType
gtypeVariant = GType #const G_TYPE_VARIANT

{- Run-time types -}

foreign import ccall "g_gtype_get_type" g_gtype_get_type :: CGType

-- | `GType` corresponding to a `GType` itself.
gtypeGType :: GType
gtypeGType = GType g_gtype_get_type

foreign import ccall "g_strv_get_type" g_strv_get_type :: CGType

-- | `GType` for a NULL terminated array of strings.
gtypeStrv :: GType
gtypeStrv = GType g_strv_get_type

foreign import ccall "g_byte_array_get_type" g_byte_array_get_type :: CGType

-- | `GType` for a boxed type holding a `GByteArray`.
gtypeByteArray :: GType
gtypeByteArray = GType g_byte_array_get_type

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

-- | A wrapped `GObject`.
class ForeignPtrNewtype a => GObject a where
    -- | Whether the `GObject` is a descendent of <https://developer.gnome.org/gobject/stable/gobject-The-Base-Object-Type.html#GInitiallyUnowned GInitiallyUnowned>.
    gobjectIsInitiallyUnowned :: a -> Bool
    -- | The `GType` for this object.
    gobjectType :: a -> IO GType

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
