{- | Machinery for some basic support of `GHashTable`.

The GLib `GHashTable` implementation requires two things: we need to
"pack" a datatype into a pointer (for datatypes that are represented
by pointers this is the trivial operation, for integers it is not, and
GLib has some helper macros).

We also need to be able to hash and check for equality different
datatypes.
-}
module GI.Utils.GHashTable
    ( GHashFunc
    , GEqualFunc

    , gDirectHash
    , gDirectEqual
    , ptrPackPtr
    , ptrUnpackPtr

    , gStrHash
    , gStrEqual
    , cstringPackPtr
    , cstringUnpackPtr
    ) where

import Data.Int
import Data.Word

import Foreign.C
import Foreign.Ptr (Ptr, castPtr, FunPtr)

import GI.Utils.BasicTypes (PtrWrapped(..))

#include <glib-object.h>

-- | A pointer to a hashing function on the C side.
type GHashFunc a = FunPtr (PtrWrapped a -> IO #{type guint})

-- | A pointer to an equality checking function on the C side.
type GEqualFunc a = FunPtr (PtrWrapped a -> PtrWrapped a -> IO #{type gboolean})

-- | Compute the hash for a `Ptr`.
foreign import ccall "&g_direct_hash" gDirectHash :: GHashFunc (Ptr a)

-- | Check whether two pointers are equal.
foreign import ccall "&g_direct_equal" gDirectEqual :: GEqualFunc (Ptr a)

-- | Pack a `Ptr` into a `PtrWrapped` `Ptr`.
ptrPackPtr :: Ptr a -> PtrWrapped (Ptr a)
ptrPackPtr = PtrWrapped . castPtr

-- | Extract a `Ptr` from a `PtrWrapped` `Ptr`.
ptrUnpackPtr :: PtrWrapped (Ptr a) -> Ptr a
ptrUnpackPtr = castPtr . unwrapPtr

-- | Compute the hash for a `CString`.
foreign import ccall "&g_str_hash" gStrHash :: GHashFunc CString

-- | Check whether two `CString`s are equal.
foreign import ccall "&g_str_equal" gStrEqual :: GEqualFunc CString

-- | Pack a `CString` into a `Ptr` than can go into a `GHashTable`.
cstringPackPtr :: CString -> PtrWrapped CString
cstringPackPtr = ptrPackPtr

-- | Extract a `CString` wrapped into a `Ptr` coming from a `GHashTable`.
cstringUnpackPtr :: PtrWrapped CString -> CString
cstringUnpackPtr = ptrUnpackPtr
