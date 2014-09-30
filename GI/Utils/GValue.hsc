{-# LANGUAGE FlexibleInstances #-}
module GI.Utils.GValue
    ( GValue(..)

    , IsGValue(..)

    , newGValue         -- Build a new, empty, GValue of the given type
    , buildGValue       -- Build a new GValue and initialize to the given value

    , set_string
    , get_string
    , set_pointer
    , get_pointer
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
    , set_object
    , get_object
    , set_boxed
    , get_boxed
    , set_enum
    , get_enum
    , set_flags
    , get_flags
    ) where

#include <glib-object.h>

#if !defined(GLIB_VERSION_2_36)
#literal g_type_init();
#endif

import Control.Applicative ((<$>))

import Foreign.C
import Foreign.Safe
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified Data.Text as T

import GI.Utils.BasicTypes
import GI.Utils.ManagedPtr
import GI.Utils.Utils (callocBytes)

data GValue = GValue (ForeignPtr GValue)

instance ManagedPtr GValue where
    unsafeManagedPtrGetPtr = (\(GValue x) -> castPtr $ unsafeForeignPtrToPtr x)
    touchManagedPtr        = (\(GValue x) -> touchForeignPtr x)

foreign import ccall unsafe "g_value_get_type" c_g_value_get_type ::
    IO GType

instance BoxedObject GValue where
    boxedType _ = c_g_value_get_type

foreign import ccall "g_value_init" g_value_init ::
    Ptr GValue -> GType -> IO (Ptr GValue)
foreign import ccall "g_value_unset" g_value_unset ::
    Ptr GValue -> IO ()

newGValue :: GType -> IO GValue
newGValue gtype = do
  gvptr <- callocBytes #size GValue
  _ <- g_value_init gvptr gtype
  gv <- wrapBoxed GValue gvptr
  return $! gv

-- Build a new GValue and set the initial value, just for convenience
buildGValue :: GType -> (GValue -> a -> IO ()) -> a -> IO GValue
buildGValue gtype setter val = do
  gv <- newGValue gtype
  setter gv val
  return gv

class IsGValue a where
    toGValue :: a -> IO GValue

instance IsGValue String where
    toGValue = buildGValue gtypeString set_string

instance IsGValue T.Text where
    toGValue = (buildGValue gtypeString set_string) . T.unpack

instance IsGValue (Ptr a) where
    toGValue = buildGValue gtypePointer set_pointer

instance IsGValue Int32 where
    toGValue = buildGValue gtypeInt32 set_int32

instance IsGValue Word32 where
    toGValue = buildGValue gtypeUInt32 set_uint32

instance IsGValue Int64 where
    toGValue = buildGValue gtypeInt64 set_int64

instance IsGValue Word64 where
    toGValue = buildGValue gtypeUInt64 set_uint64

instance IsGValue Float where
    toGValue = buildGValue gtypeFloat set_float

instance IsGValue Double where
    toGValue = buildGValue gtypeDouble set_double

instance IsGValue Bool where
    toGValue = buildGValue gtypeBoolean set_boolean

foreign import ccall "g_value_set_string" _set_string ::
    Ptr GValue -> CString -> IO ()
foreign import ccall unsafe "g_value_get_string" _get_string ::
    Ptr GValue -> IO CString

set_string :: GValue -> String -> IO ()
set_string gv str = withManagedPtr gv $ \ptr ->
                    withCString str $ \cstr -> _set_string ptr cstr

get_string :: GValue -> IO String
get_string gv = withManagedPtr gv $ \ptr ->
                _get_string ptr >>= peekCString

foreign import ccall unsafe "g_value_set_pointer" _set_pointer ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall unsafe "g_value_get_pointer" _get_pointer ::
    Ptr GValue -> IO (Ptr b)

set_pointer :: GValue -> Ptr a -> IO ()
set_pointer gv ptr = withManagedPtr gv $ flip _set_pointer ptr

get_pointer :: GValue -> IO (Ptr b)
get_pointer gv = withManagedPtr gv _get_pointer

foreign import ccall unsafe "g_value_set_int" _set_int32 ::
    Ptr GValue -> Int32 -> IO ()
foreign import ccall unsafe "g_value_get_int" _get_int32 ::
    Ptr GValue -> IO Int32

set_int32 :: GValue -> Int32 -> IO ()
set_int32 gv n = withManagedPtr gv $ flip _set_int32 n

get_int32 :: GValue -> IO Int32
get_int32 gv = withManagedPtr gv _get_int32

foreign import ccall unsafe "g_value_set_uint" _set_uint32 ::
    Ptr GValue -> Word32 -> IO ()
foreign import ccall unsafe "g_value_get_uint" _get_uint32 ::
    Ptr GValue -> IO Word32

set_uint32 :: GValue -> Word32 -> IO ()
set_uint32 gv n = withManagedPtr gv $ flip _set_uint32 n

get_uint32 :: GValue -> IO Word32
get_uint32 gv = withManagedPtr gv _get_uint32

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
