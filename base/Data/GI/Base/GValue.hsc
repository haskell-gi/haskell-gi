{-# LANGUAGE FlexibleInstances #-}
module Data.GI.Base.GValue
    (
    -- * Constructing GValues
      GValue(..)
    , IsGValue(..)
    , GValueConstruct(..)

    , newGValue
    , buildGValue
    , noGValue

    -- * Setters and getters
    , set_string
    , get_string
    , set_pointer
    , get_pointer
    , set_int
    , get_int
    , set_uint
    , get_uint
    , set_long
    , get_long
    , set_ulong
    , get_ulong
    , set_int32
    , get_int32
    , set_uint32
    , get_uint32
    , set_int64
    , get_int64
    , set_uint64
    , get_uint64
    , set_float
    , get_float
    , set_double
    , get_double
    , set_boolean
    , get_boolean
    , set_gtype
    , get_gtype
    , set_object
    , get_object
    , set_boxed
    , get_boxed
    , set_variant
    , get_variant
    , set_enum
    , get_enum
    , set_flags
    , get_flags
    ) where

#include <glib-object.h>

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Coerce (coerce)
import Data.Word
import Data.Int
import Data.Text (Text, pack, unpack)

import Foreign.C.Types (CInt(..), CUInt(..), CFloat(..), CDouble(..),
                        CLong(..), CULong(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)

import Data.GI.Base.BasicTypes
import Data.GI.Base.BasicConversions (cstringToText, textToCString)

import Data.GI.Base.ManagedPtr
import Data.GI.Base.Utils (callocBytes, freeMem)

-- | Haskell-side representation of a @GValue@.
newtype GValue = GValue (ManagedPtr GValue)

-- | A convenience alias for @`Nothing` :: `Maybe` `GValue`@.
noGValue :: Maybe GValue
noGValue = Nothing

foreign import ccall unsafe "g_value_get_type" c_g_value_get_type ::
    IO CGType

instance BoxedObject GValue where
    boxedType _ = GType <$> c_g_value_get_type

foreign import ccall "g_value_init" g_value_init ::
    Ptr GValue -> CGType -> IO (Ptr GValue)

-- | A type holding a `GValue` with an associated label. It is
-- parameterized by a phantom type encoding the target type for the
-- `GValue` (useful when constructing properties).
data GValueConstruct o = GValueConstruct String GValue

-- | Build a new, empty, `GValue` of the given type.
newGValue :: GType -> IO GValue
newGValue (GType gtype) = do
  gvptr <- callocBytes (#size GValue)
  _ <- g_value_init gvptr gtype
  gv <- wrapBoxed GValue gvptr
  return $! gv

-- | A convenience function for building a new GValue and setting the
-- initial value.
buildGValue :: GType -> (GValue -> a -> IO ()) -> a -> IO GValue
buildGValue gtype setter val = do
  gv <- newGValue gtype
  setter gv val
  return gv

-- | A convenience class for marshaling back and forth between Haskell
-- values and `GValue`s.
class IsGValue a where
    toGValue :: a -> IO GValue
    fromGValue :: GValue -> IO a

instance IsGValue (Maybe String) where
    toGValue = buildGValue gtypeString set_string . fmap pack
    fromGValue v = (fmap unpack) <$> get_string v

instance IsGValue (Maybe Text) where
    toGValue = buildGValue gtypeString set_string
    fromGValue = get_string

instance IsGValue (Ptr a) where
    toGValue = buildGValue gtypePointer set_pointer
    fromGValue = get_pointer

instance IsGValue Int32 where
    toGValue = buildGValue gtypeInt set_int32
    fromGValue = get_int32

instance IsGValue Word32 where
    toGValue = buildGValue gtypeUInt set_uint32
    fromGValue = get_uint32

instance IsGValue CInt where
    toGValue = buildGValue gtypeInt set_int
    fromGValue = get_int

instance IsGValue CUInt where
    toGValue = buildGValue gtypeUInt set_uint
    fromGValue = get_uint

instance IsGValue CLong where
    toGValue = buildGValue gtypeLong set_long
    fromGValue = get_long

instance IsGValue CULong where
    toGValue = buildGValue gtypeULong set_ulong
    fromGValue = get_ulong

instance IsGValue Int64 where
    toGValue = buildGValue gtypeInt64 set_int64
    fromGValue = get_int64

instance IsGValue Word64 where
    toGValue = buildGValue gtypeUInt64 set_uint64
    fromGValue = get_uint64

instance IsGValue Float where
    toGValue = buildGValue gtypeFloat set_float
    fromGValue = get_float

instance IsGValue Double where
    toGValue = buildGValue gtypeDouble set_double
    fromGValue = get_double

instance IsGValue Bool where
    toGValue = buildGValue gtypeBoolean set_boolean
    fromGValue = get_boolean

instance IsGValue GType where
    toGValue = buildGValue gtypeGType set_gtype
    fromGValue = get_gtype

foreign import ccall "g_value_set_string" _set_string ::
    Ptr GValue -> CString -> IO ()
foreign import ccall "g_value_get_string" _get_string ::
    Ptr GValue -> IO CString

set_string :: GValue -> Maybe Text -> IO ()
set_string gv maybeStr =
    withManagedPtr gv $ \ptr -> do
      cstr <- case maybeStr of
                Just str -> textToCString str
                Nothing -> return nullPtr
      _set_string ptr cstr
      freeMem cstr

get_string :: GValue -> IO (Maybe Text)
get_string gv = withManagedPtr gv $ \gvptr -> do
                  cstr <- _get_string gvptr
                  if cstr /= nullPtr
                  then Just <$> cstringToText cstr
                  else return Nothing

foreign import ccall unsafe "g_value_set_pointer" _set_pointer ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall unsafe "g_value_get_pointer" _get_pointer ::
    Ptr GValue -> IO (Ptr b)

set_pointer :: GValue -> Ptr a -> IO ()
set_pointer gv ptr = withManagedPtr gv $ flip _set_pointer ptr

get_pointer :: GValue -> IO (Ptr b)
get_pointer gv = withManagedPtr gv _get_pointer

foreign import ccall unsafe "g_value_set_int" _set_int ::
    Ptr GValue -> CInt -> IO ()
foreign import ccall unsafe "g_value_get_int" _get_int ::
    Ptr GValue -> IO CInt

set_int32 :: GValue -> Int32 -> IO ()
set_int32 gv n = withManagedPtr gv $ flip _set_int (coerce n)

get_int32 :: GValue -> IO Int32
get_int32 gv = coerce <$> withManagedPtr gv _get_int

set_int :: GValue -> CInt -> IO ()
set_int gv n = withManagedPtr gv $ flip _set_int n

get_int :: GValue -> IO CInt
get_int gv = withManagedPtr gv _get_int

foreign import ccall unsafe "g_value_set_uint" _set_uint ::
    Ptr GValue -> CUInt -> IO ()
foreign import ccall unsafe "g_value_get_uint" _get_uint ::
    Ptr GValue -> IO CUInt

set_uint32 :: GValue -> Word32 -> IO ()
set_uint32 gv n = withManagedPtr gv $ flip _set_uint (coerce n)

get_uint32 :: GValue -> IO Word32
get_uint32 gv = coerce <$> withManagedPtr gv _get_uint

set_uint :: GValue -> CUInt -> IO ()
set_uint gv n = withManagedPtr gv $ flip _set_uint n

get_uint :: GValue -> IO CUInt
get_uint gv = withManagedPtr gv _get_uint

foreign import ccall unsafe "g_value_set_long" _set_long ::
    Ptr GValue -> CLong -> IO ()
foreign import ccall unsafe "g_value_get_long" _get_long ::
    Ptr GValue -> IO CLong

set_long :: GValue -> CLong -> IO ()
set_long gv n = withManagedPtr gv $ flip _set_long n

get_long :: GValue -> IO CLong
get_long gv = withManagedPtr gv _get_long

foreign import ccall unsafe "g_value_set_ulong" _set_ulong ::
    Ptr GValue -> CULong -> IO ()
foreign import ccall unsafe "g_value_get_ulong" _get_ulong ::
    Ptr GValue -> IO CULong

set_ulong :: GValue -> CULong -> IO ()
set_ulong gv n = withManagedPtr gv $ flip _set_ulong n

get_ulong :: GValue -> IO CULong
get_ulong gv = withManagedPtr gv _get_ulong

foreign import ccall unsafe "g_value_set_int64" _set_int64 ::
    Ptr GValue -> Int64 -> IO ()
foreign import ccall unsafe "g_value_get_int64" _get_int64 ::
    Ptr GValue -> IO Int64

set_int64 :: GValue -> Int64 -> IO ()
set_int64 gv n = withManagedPtr gv $ flip _set_int64 n

get_int64 :: GValue -> IO Int64
get_int64 gv = withManagedPtr gv _get_int64

foreign import ccall unsafe "g_value_set_uint64" _set_uint64 ::
    Ptr GValue -> Word64 -> IO ()
foreign import ccall unsafe "g_value_get_uint64" _get_uint64 ::
    Ptr GValue -> IO Word64

set_uint64 :: GValue -> Word64 -> IO ()
set_uint64 gv n = withManagedPtr gv $ flip _set_uint64 n

get_uint64 :: GValue -> IO Word64
get_uint64 gv = withManagedPtr gv _get_uint64

foreign import ccall unsafe "g_value_set_float" _set_float ::
    Ptr GValue -> CFloat -> IO ()
foreign import ccall unsafe "g_value_get_float" _get_float ::
    Ptr GValue -> IO CFloat

set_float :: GValue -> Float -> IO ()
set_float gv f = withManagedPtr gv $ flip _set_float (realToFrac f)

get_float :: GValue -> IO Float
get_float gv = realToFrac <$> withManagedPtr gv _get_float

foreign import ccall unsafe "g_value_set_double" _set_double ::
    Ptr GValue -> CDouble -> IO ()
foreign import ccall unsafe "g_value_get_double" _get_double ::
    Ptr GValue -> IO CDouble

set_double :: GValue -> Double -> IO ()
set_double gv d = withManagedPtr gv $ flip _set_double (realToFrac d)

get_double :: GValue -> IO Double
get_double gv = realToFrac <$> withManagedPtr gv _get_double

foreign import ccall unsafe "g_value_set_boolean" _set_boolean ::
    Ptr GValue -> CInt -> IO ()
foreign import ccall unsafe "g_value_get_boolean" _get_boolean ::
    Ptr GValue -> IO CInt

set_boolean :: GValue -> Bool -> IO ()
set_boolean gv b = withManagedPtr gv $ \ptr ->
                   _set_boolean ptr (fromIntegral $ fromEnum b)

get_boolean :: GValue -> IO Bool
get_boolean gv = withManagedPtr gv $ \ptr -> (/= 0) <$> _get_boolean ptr

foreign import ccall unsafe "g_value_set_gtype" _set_gtype ::
    Ptr GValue -> CGType -> IO ()
foreign import ccall unsafe "g_value_get_gtype" _get_gtype ::
    Ptr GValue -> IO CGType

set_gtype :: GValue -> GType -> IO ()
set_gtype gv (GType g) = withManagedPtr gv $ \ptr -> _set_gtype ptr g

get_gtype :: GValue -> IO GType
get_gtype gv = GType <$> withManagedPtr gv _get_gtype

foreign import ccall "g_value_set_object" _set_object ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall "g_value_get_object" _get_object ::
    Ptr GValue -> IO (Ptr a)

set_object :: GObject a => GValue -> Ptr a -> IO ()
set_object gv o = withManagedPtr gv $ flip _set_object o

get_object :: GObject b => GValue -> IO (Ptr b)
get_object gv = withManagedPtr gv _get_object

foreign import ccall "g_value_set_boxed" _set_boxed ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall "g_value_get_boxed" _get_boxed ::
    Ptr GValue -> IO (Ptr b)

set_boxed :: GValue -> Ptr a -> IO ()
set_boxed gv b = withManagedPtr gv $ flip _set_boxed b

get_boxed :: GValue -> IO (Ptr b)
get_boxed gv = withManagedPtr gv _get_boxed

foreign import ccall "g_value_set_variant" _set_variant ::
    Ptr GValue -> Ptr GVariant -> IO ()
foreign import ccall "g_value_get_variant" _get_variant ::
    Ptr GValue -> IO (Ptr GVariant)

set_variant :: GValue -> Ptr GVariant -> IO ()
set_variant gv v = withManagedPtr gv $ flip _set_variant v

get_variant :: GValue -> IO (Ptr GVariant)
get_variant gv = withManagedPtr gv _get_variant

foreign import ccall unsafe "g_value_set_enum" _set_enum ::
    Ptr GValue -> CUInt -> IO ()
foreign import ccall unsafe "g_value_get_enum" _get_enum ::
    Ptr GValue -> IO CUInt

set_enum :: GValue -> CUInt -> IO ()
set_enum gv e = withManagedPtr gv $ flip _set_enum e

get_enum :: GValue -> IO CUInt
get_enum gv = withManagedPtr gv _get_enum

foreign import ccall unsafe "g_value_set_flags" _set_flags ::
    Ptr GValue -> CUInt -> IO ()
foreign import ccall unsafe "g_value_get_flags" _get_flags ::
    Ptr GValue -> IO CUInt

set_flags :: GValue -> CUInt -> IO ()
set_flags gv f = withManagedPtr gv $ flip _set_flags f

get_flags :: GValue -> IO CUInt
get_flags gv = withManagedPtr gv _get_flags
