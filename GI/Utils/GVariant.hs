{-# LANGUAGE FlexibleInstances #-}
module GI.Utils.GVariant
    ( IsGVariant(toGVariant, fromGVariant)

    , noGVariant

    , wrapGVariantPtr
    , newGVariantFromPtr
    , refGVariant
    , unrefGVariant

    , gvariant_to_string
    , gvariant_from_string

    , gvariant_to_integral
    , gvariant_from_integral
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad (when, void)

import GI.Utils.BasicTypes (GVariant(..))

noGVariant :: Maybe GVariant
noGVariant = Nothing

class IsGVariant a where
    toGVariant   :: a -> IO GVariant
    fromGVariant :: GVariant -> IO a

foreign import ccall "g_variant_is_floating" g_variant_is_floating ::
    Ptr GVariant -> IO CInt
foreign import ccall "g_variant_ref_sink" g_variant_ref_sink ::
    Ptr GVariant -> IO (Ptr GVariant)
foreign import ccall "g_variant_ref" g_variant_ref ::
    Ptr GVariant -> IO (Ptr GVariant)
foreign import ccall "g_variant_unref" g_variant_unref ::
    Ptr GVariant -> IO ()
foreign import ccall "&g_variant_unref" ptr_to_g_variant_unref ::
    FunPtr (Ptr GVariant -> IO ())

-- Take ownership of a passed in Ptr (typically created just for us,
-- so often it is floating).
wrapGVariantPtr :: Ptr GVariant -> IO GVariant
wrapGVariantPtr ptr = do
  floating <- g_variant_is_floating ptr
  when (floating /= 0) $ void $ g_variant_ref_sink ptr
  fPtr <- newForeignPtr ptr_to_g_variant_unref ptr
  return $! GVariant fPtr

-- Construct a Haskell wrapper for the given GGVariant, without
-- assuming ownership of the original.
newGVariantFromPtr :: Ptr GVariant -> IO GVariant
newGVariantFromPtr ptr = do
  fPtr <- g_variant_ref ptr >>= newForeignPtr ptr_to_g_variant_unref
  return $! GVariant fPtr

-- Ref and unref the given variant
refGVariant :: GVariant -> IO (Ptr GVariant)
refGVariant (GVariant fptr) = withForeignPtr fptr g_variant_ref

unrefGVariant :: GVariant -> IO ()
unrefGVariant (GVariant fptr) = withForeignPtr fptr g_variant_unref

instance IsGVariant String where
    toGVariant = gvariant_from_string
    fromGVariant = gvariant_to_string

gvariant_to_string :: GVariant -> IO String
gvariant_to_string _ = undefined

gvariant_from_string :: String -> IO GVariant
gvariant_from_string = undefined

instance IsGVariant Integer where
    toGVariant = gvariant_from_integral
    fromGVariant = gvariant_to_integral

instance IsGVariant Int where
    toGVariant = gvariant_from_integral
    fromGVariant = gvariant_to_integral

gvariant_to_integral :: Integral a => GVariant -> IO a
gvariant_to_integral = undefined

gvariant_from_integral :: Integral a => a -> IO GVariant
gvariant_from_integral = undefined

