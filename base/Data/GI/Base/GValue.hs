{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.GI.Base.GValue
    (
    -- * Constructing GValues
      GValue(..)
    , IsGValue(..)
    , toGValue
    , fromGValue
    , GValueConstruct(..)

    , newGValue
    , buildGValue
    , disownGValue
    , noGValue
    , newGValueFromPtr
    , wrapGValuePtr
    , unsetGValue
    , gvalueType

    -- * Packing GValues into arrays
    , packGValueArray
    , unpackGValueArrayWithLength
    , mapGValueArrayWithLength

    -- * Setters and getters
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
    , set_stablePtr
    , get_stablePtr
    , take_stablePtr

    ) where

import Data.Coerce (coerce)
import Data.Word
import Data.Int
import Data.Text (Text, pack, unpack)

import Foreign.C.Types (CInt(..), CUInt(..), CFloat(..), CDouble(..),
                        CLong(..), CULong(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, castPtrToStablePtr)

import Data.GI.Base.BasicTypes
import Data.GI.Base.BasicConversions (cstringToText, textToCString)
import Data.GI.Base.GType
import Data.GI.Base.ManagedPtr
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Base.Utils (callocBytes, freeMem)
import Data.GI.Base.Internal.CTypes (cgvalueSize)

-- | Haskell-side representation of a @GValue@.
newtype GValue = GValue (ManagedPtr GValue)

-- | A convenience alias for @`Nothing` :: `Maybe` `GValue`@.
noGValue :: Maybe GValue
noGValue = Nothing

foreign import ccall unsafe "g_value_get_type" c_g_value_get_type ::
    IO GType

-- | There are no types in the bindings that a `GValue` can be safely
-- cast to.
type instance ParentTypes GValue = '[]
instance HasParentTypes GValue

-- | Find the associated `GType` for `GValue`.
instance TypedObject GValue where
  glibType = c_g_value_get_type

-- | `GValue`s are registered as boxed in the GLib type system.
instance GBoxed GValue

foreign import ccall "g_value_init" g_value_init ::
    Ptr GValue -> CGType -> IO (Ptr GValue)

-- | A type holding a `GValue` with an associated label. It is
-- parameterized by a phantom type encoding the target type for the
-- `GValue` (useful when constructing properties).
data GValueConstruct o = GValueConstruct String GValue

-- | Build a new, empty, `GValue` of the given type.
newGValue :: GType -> IO GValue
newGValue (GType gtype) = do
  gvptr <- callocBytes cgvalueSize
  _ <- g_value_init gvptr gtype
  gv <- wrapBoxed GValue gvptr
  return $! gv

-- | Take ownership of a passed in 'Ptr'.
wrapGValuePtr :: Ptr GValue -> IO GValue
wrapGValuePtr ptr = wrapBoxed GValue ptr

-- | Construct a Haskell wrapper for the given 'GValue', making a
-- copy.
newGValueFromPtr :: Ptr GValue -> IO GValue
newGValueFromPtr ptr = newBoxed GValue ptr

-- | A convenience function for building a new GValue and setting the
-- initial value.
buildGValue :: GType -> (Ptr GValue -> a -> IO ()) -> a -> IO GValue
buildGValue gtype setter val = do
  gv <- newGValue gtype
  withManagedPtr gv $ \gvPtr -> setter gvPtr val
  return gv

-- | Disown a `GValue`, i.e. do not unref the underlying object when
-- the Haskell object is garbage collected.
disownGValue :: GValue -> IO (Ptr GValue)
disownGValue = disownManagedPtr

foreign import ccall "_haskell_gi_g_value_get_type" g_value_get_type :: Ptr GValue -> IO CGType

-- | Return the `GType` contained by a `GValue`.
gvalueType :: GValue -> IO GType
gvalueType gv = withManagedPtr gv $ \gvptr -> do
  cgtype <- g_value_get_type gvptr
  return (GType cgtype)

foreign import ccall "g_value_unset" g_value_unset :: Ptr GValue -> IO ()

-- | Unset the `GValue`, freeing all resources associated to it.
unsetGValue :: Ptr GValue -> IO ()
unsetGValue = g_value_unset

-- | Class for types that can be marshaled back and forth between
-- Haskell values and `GValue`s. These are low-level methods, you
-- might want to use `toGValue` and `fromGValue` instead for a higher
-- level interface.
class IsGValue a where
  gvalueGType_ :: IO GType     -- ^ `GType` for the `GValue`
                               -- containing values of this type.
  gvalueSet_   :: Ptr GValue -> a -> IO ()  -- ^ Set the `GValue` to
                                            -- the given Haskell
                                            -- value.
  gvalueGet_   :: Ptr GValue -> IO a -- ^ Get the Haskel value inside
                                     -- the `GValue`.

-- | Create a `GValue` from the given Haskell value.
toGValue :: forall a. IsGValue a => a -> IO GValue
toGValue val = do
  gvptr <- callocBytes cgvalueSize
  GType gtype <- gvalueGType_ @a
  _ <- g_value_init gvptr gtype
  gvalueSet_ gvptr val
  gv <- wrapBoxed GValue gvptr
  return $! gv

-- | Create a Haskell object out of the given `GValue`.
fromGValue :: IsGValue a => GValue -> IO a
fromGValue gv = withManagedPtr gv gvalueGet_

instance IsGValue (Maybe String) where
  gvalueGType_ = return gtypeString
  gvalueSet_ gvPtr mstr = set_string gvPtr (pack <$> mstr)
  gvalueGet_ v = (fmap unpack) <$> get_string v

instance IsGValue (Maybe Text) where
  gvalueGType_ = return gtypeString
  gvalueSet_ = set_string
  gvalueGet_ = get_string

instance IsGValue (Ptr a) where
  gvalueGType_ = return gtypePointer
  gvalueSet_ = set_pointer
  gvalueGet_ = get_pointer

instance IsGValue Int32 where
  gvalueGType_ = return gtypeInt
  gvalueSet_ = set_int32
  gvalueGet_ = get_int32

instance IsGValue Word32 where
  gvalueGType_ = return gtypeUInt
  gvalueSet_ = set_uint32
  gvalueGet_ = get_uint32

instance IsGValue CInt where
  gvalueGType_ = return gtypeInt
  gvalueSet_ = set_int
  gvalueGet_ = get_int

instance IsGValue CUInt where
  gvalueGType_ = return gtypeUInt
  gvalueSet_ = set_uint
  gvalueGet_ = get_uint

instance IsGValue CLong where
  gvalueGType_ = return gtypeLong
  gvalueSet_ = set_long
  gvalueGet_ = get_long

instance IsGValue CULong where
  gvalueGType_ = return gtypeULong
  gvalueSet_ = set_ulong
  gvalueGet_ = get_ulong

instance IsGValue Int64 where
  gvalueGType_ = return gtypeInt64
  gvalueSet_ = set_int64
  gvalueGet_ = get_int64

instance IsGValue Word64 where
  gvalueGType_ = return gtypeUInt64
  gvalueSet_ = set_uint64
  gvalueGet_ = get_uint64

instance IsGValue Float where
  gvalueGType_ = return gtypeFloat
  gvalueSet_ = set_float
  gvalueGet_ = get_float

instance IsGValue Double where
  gvalueGType_ = return gtypeDouble
  gvalueSet_ = set_double
  gvalueGet_ = get_double

instance IsGValue Bool where
  gvalueGType_ = return gtypeBoolean
  gvalueSet_ = set_boolean
  gvalueGet_ = get_boolean

instance IsGValue GType where
  gvalueGType_ = return gtypeGType
  gvalueSet_ = set_gtype
  gvalueGet_ = get_gtype

instance IsGValue (StablePtr a) where
  gvalueGType_ = return gtypeStablePtr
  gvalueSet_ = set_stablePtr
  gvalueGet_ = get_stablePtr

foreign import ccall "g_value_set_string" _set_string ::
    Ptr GValue -> CString -> IO ()
foreign import ccall "g_value_get_string" _get_string ::
    Ptr GValue -> IO CString

set_string :: Ptr GValue -> Maybe Text -> IO ()
set_string ptr maybeStr = do
  cstr <- case maybeStr of
            Just str -> textToCString str
            Nothing -> return nullPtr
  _set_string ptr cstr
  freeMem cstr

get_string :: Ptr GValue -> IO (Maybe Text)
get_string gvptr = do
  cstr <- _get_string gvptr
  if cstr /= nullPtr
    then Just <$> cstringToText cstr
    else return Nothing

foreign import ccall unsafe "g_value_set_pointer" set_pointer ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall unsafe "g_value_get_pointer" get_pointer ::
    Ptr GValue -> IO (Ptr b)

foreign import ccall unsafe "g_value_set_int" set_int ::
    Ptr GValue -> CInt -> IO ()
foreign import ccall unsafe "g_value_get_int" get_int ::
    Ptr GValue -> IO CInt

set_int32 :: Ptr GValue -> Int32 -> IO ()
set_int32 gv n = set_int gv (coerce n)

get_int32 :: Ptr GValue -> IO Int32
get_int32 gv = coerce <$> get_int gv

foreign import ccall unsafe "g_value_set_uint" set_uint ::
    Ptr GValue -> CUInt -> IO ()
foreign import ccall unsafe "g_value_get_uint" get_uint ::
    Ptr GValue -> IO CUInt

set_uint32 :: Ptr GValue -> Word32 -> IO ()
set_uint32 gv n = set_uint gv (coerce n)

get_uint32 :: Ptr GValue -> IO Word32
get_uint32 gv = coerce <$> get_uint gv

foreign import ccall unsafe "g_value_set_long" set_long ::
    Ptr GValue -> CLong -> IO ()
foreign import ccall unsafe "g_value_get_long" get_long ::
    Ptr GValue -> IO CLong

foreign import ccall unsafe "g_value_set_ulong" set_ulong ::
    Ptr GValue -> CULong -> IO ()
foreign import ccall unsafe "g_value_get_ulong" get_ulong ::
    Ptr GValue -> IO CULong

foreign import ccall unsafe "g_value_set_int64" set_int64 ::
    Ptr GValue -> Int64 -> IO ()
foreign import ccall unsafe "g_value_get_int64" get_int64 ::
    Ptr GValue -> IO Int64

foreign import ccall unsafe "g_value_set_uint64" set_uint64 ::
    Ptr GValue -> Word64 -> IO ()
foreign import ccall unsafe "g_value_get_uint64" get_uint64 ::
    Ptr GValue -> IO Word64

foreign import ccall unsafe "g_value_set_float" _set_float ::
    Ptr GValue -> CFloat -> IO ()
foreign import ccall unsafe "g_value_get_float" _get_float ::
    Ptr GValue -> IO CFloat

set_float :: Ptr GValue -> Float -> IO ()
set_float gv f = _set_float gv (realToFrac f)

get_float :: Ptr GValue -> IO Float
get_float gv = realToFrac <$> _get_float gv

foreign import ccall unsafe "g_value_set_double" _set_double ::
    Ptr GValue -> CDouble -> IO ()
foreign import ccall unsafe "g_value_get_double" _get_double ::
    Ptr GValue -> IO CDouble

set_double :: Ptr GValue -> Double -> IO ()
set_double gv d = _set_double gv (realToFrac d)

get_double :: Ptr GValue -> IO Double
get_double gv = realToFrac <$> _get_double gv

foreign import ccall unsafe "g_value_set_boolean" _set_boolean ::
    Ptr GValue -> CInt -> IO ()
foreign import ccall unsafe "g_value_get_boolean" _get_boolean ::
    Ptr GValue -> IO CInt

set_boolean :: Ptr GValue -> Bool -> IO ()
set_boolean gv b = _set_boolean gv (fromIntegral $ fromEnum b)

get_boolean :: Ptr GValue -> IO Bool
get_boolean gv = (/= 0) <$> _get_boolean gv

foreign import ccall unsafe "g_value_set_gtype" _set_gtype ::
    Ptr GValue -> CGType -> IO ()
foreign import ccall unsafe "g_value_get_gtype" _get_gtype ::
    Ptr GValue -> IO CGType

set_gtype :: Ptr GValue -> GType -> IO ()
set_gtype gv (GType g) = _set_gtype gv g

get_gtype :: Ptr GValue -> IO GType
get_gtype gv = GType <$> _get_gtype gv

foreign import ccall "g_value_set_object" _set_object ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall "g_value_get_object" _get_object ::
    Ptr GValue -> IO (Ptr a)

set_object :: GObject a => Ptr GValue -> Ptr a -> IO ()
set_object = _set_object

get_object :: GObject a => Ptr GValue -> IO (Ptr a)
get_object = _get_object

foreign import ccall "g_value_set_boxed" set_boxed ::
    Ptr GValue -> Ptr a -> IO ()
foreign import ccall "g_value_get_boxed" get_boxed ::
    Ptr GValue -> IO (Ptr b)

foreign import ccall "g_value_set_variant" set_variant ::
    Ptr GValue -> Ptr GVariant -> IO ()
foreign import ccall "g_value_get_variant" get_variant ::
    Ptr GValue -> IO (Ptr GVariant)

foreign import ccall unsafe "g_value_set_enum" set_enum ::
    Ptr GValue -> CUInt -> IO ()
foreign import ccall unsafe "g_value_get_enum" get_enum ::
    Ptr GValue -> IO CUInt

foreign import ccall unsafe "g_value_set_flags" set_flags ::
    Ptr GValue -> CUInt -> IO ()
foreign import ccall unsafe "g_value_get_flags" get_flags ::
    Ptr GValue -> IO CUInt

-- | Set the value of `GValue` containing a `StablePtr`
set_stablePtr :: Ptr GValue -> StablePtr a -> IO ()
set_stablePtr gv ptr = set_boxed gv (castStablePtrToPtr ptr)

foreign import ccall g_value_take_boxed :: Ptr GValue -> StablePtr a -> IO ()

-- | Like `set_stablePtr`, but the `GValue` takes ownership of the `StablePtr`
take_stablePtr :: Ptr GValue -> StablePtr a -> IO ()
take_stablePtr = g_value_take_boxed

-- | Get the value of a `GValue` containing a `StablePtr`
get_stablePtr :: Ptr GValue -> IO (StablePtr a)
get_stablePtr gv = castPtrToStablePtr <$> get_boxed gv

foreign import ccall g_value_copy :: Ptr GValue -> Ptr GValue -> IO ()

-- | Pack the given list of GValues contiguously into a C array
packGValueArray :: [GValue] -> IO (Ptr GValue)
packGValueArray gvalues = withManagedPtrList gvalues $ \ptrs -> do
  let nitems = length ptrs
  mem <- callocBytes $ cgvalueSize * nitems
  fill mem ptrs
  return mem
  where fill :: Ptr GValue -> [Ptr GValue] -> IO ()
        fill _ [] = return ()
        fill ptr (x:xs) = do
          gtype <- g_value_get_type x
          _ <- g_value_init ptr gtype
          g_value_copy x ptr
          fill (ptr `plusPtr` cgvalueSize) xs

-- | Unpack an array of contiguous GValues into a list of GValues.
unpackGValueArrayWithLength :: Integral a =>
                               a -> Ptr GValue -> IO [GValue]
unpackGValueArrayWithLength nitems gvalues = go (fromIntegral nitems) gvalues
  where go :: Int -> Ptr GValue -> IO [GValue]
        go 0 _ = return []
        go n ptr = do
          gv <- callocBytes cgvalueSize
          gtype <- g_value_get_type ptr
          _ <- g_value_init gv gtype
          g_value_copy ptr gv
          wrapped <- wrapGValuePtr gv
          (wrapped :) <$> go (n-1) (ptr `plusPtr` cgvalueSize)

-- | Map over the `GValue`s inside a C array.
mapGValueArrayWithLength :: Integral a =>
                            a -> (Ptr GValue -> IO c) -> Ptr GValue -> IO ()
mapGValueArrayWithLength nvalues f arrayPtr
  | (arrayPtr == nullPtr) = return ()
  | (nvalues <= 0) = return ()
  | otherwise = go (fromIntegral nvalues) arrayPtr
  where go :: Int -> Ptr GValue -> IO ()
        go 0 _ = return ()
        go n ptr = do
          _ <- f ptr
          go (n-1) (ptr `plusPtr` cgvalueSize)
