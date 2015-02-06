module GI.Utils.BasicTypes
    ( GType
    , gtypeString
    , gtypePointer
    , gtypeInt32
    , gtypeUInt32
    , gtypeInt64
    , gtypeUInt64
    , gtypeFloat
    , gtypeDouble
    , gtypeBoolean
    , gtypeStrv
    , gtypeBoxed
    , gtypeObject

    , ManagedPtr(..)
    , BoxedObject(..)
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
    ) where

import Data.Word
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr.Safe (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

#include <glib-object.h>

type GType = #type GType

gtypeString :: GType
gtypeString = #const G_TYPE_STRING

gtypePointer :: GType
gtypePointer = #const G_TYPE_POINTER

gtypeInt32 :: GType
gtypeInt32 = #const G_TYPE_INT

gtypeUInt32 :: GType
gtypeUInt32 = #const G_TYPE_UINT

gtypeInt64 :: GType
gtypeInt64 = #const G_TYPE_INT64

gtypeUInt64 :: GType
gtypeUInt64 = #const G_TYPE_UINT64

gtypeFloat :: GType
gtypeFloat = #const G_TYPE_FLOAT

gtypeDouble :: GType
gtypeDouble = #const G_TYPE_DOUBLE

gtypeBoolean :: GType
gtypeBoolean = #const G_TYPE_BOOLEAN

gtypeStrv :: GType
gtypeStrv = #const G_TYPE_STRV

gtypeBoxed :: GType
gtypeBoxed = #const G_TYPE_BOXED

gtypeObject :: GType
gtypeObject = #const G_TYPE_OBJECT

class ManagedPtr a where
    unsafeManagedPtrGetPtr :: a -> Ptr b
    touchManagedPtr        :: a -> IO ()

class BoxedObject a where
    boxedType :: a -> IO GType -- This should not use the value of its
                               -- argument.

-- These class methods should not use the value of their argument.
class GObject a where
    gobjectIsInitiallyUnowned :: a -> Bool
    gobjectType :: a -> IO GType

data GVariant = GVariant (ForeignPtr GVariant)

instance ManagedPtr GVariant where
    unsafeManagedPtrGetPtr = (\(GVariant x) -> castPtr $ unsafeForeignPtrToPtr x)
    touchManagedPtr        = (\(GVariant x) -> touchForeignPtr x)

data GParamSpec = GParamSpec (ForeignPtr GParamSpec)

instance ManagedPtr GParamSpec where
    unsafeManagedPtrGetPtr = (\(GParamSpec x) -> castPtr $ unsafeForeignPtrToPtr x)
    touchManagedPtr        = (\(GParamSpec x) -> touchForeignPtr x)

class Enum a => IsGFlag a

data GArray a = GArray (Ptr (GArray a))
data GPtrArray a = GPtrArray (Ptr (GPtrArray a))
data GByteArray = GByteArray (Ptr GByteArray)
data GHashTable a b = GHashTable (Ptr (GHashTable a b))
data GList a = GList (Ptr (GList a))
data GSList a = GSList (Ptr (GSList a))

foreign import ccall "g_list_free" g_list_free ::
    Ptr (GList a) -> IO ()

foreign import ccall "g_slist_free" g_slist_free ::
    Ptr (GSList a) -> IO ()
