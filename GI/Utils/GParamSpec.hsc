module GI.Utils.GParamSpec
    ( noGParamSpec

    , wrapGParamSpecPtr
    , newGParamSpecFromPtr
    , refGParamSpec
    , unrefGParamSpec
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad (void)

import GI.Utils.BasicTypes (GParamSpec(..))

#include <glib-object.h>

noGParamSpec :: Maybe GParamSpec
noGParamSpec = Nothing

foreign import ccall "g_param_spec_ref_sink" g_param_spec_ref_sink ::
    Ptr GParamSpec -> IO (Ptr GParamSpec)
foreign import ccall "g_param_spec_ref" g_param_spec_ref ::
    Ptr GParamSpec -> IO (Ptr GParamSpec)
foreign import ccall "g_param_spec_unref" g_param_spec_unref ::
    Ptr GParamSpec -> IO ()
foreign import ccall "&g_param_spec_unref" ptr_to_g_param_spec_unref ::
    FunPtr (Ptr GParamSpec -> IO ())

-- | Take ownership of a ParamSpec passed in 'Ptr'.
wrapGParamSpecPtr :: Ptr GParamSpec -> IO GParamSpec
wrapGParamSpecPtr ptr = do
  void $ g_param_spec_ref_sink ptr
  fPtr <- newForeignPtr ptr_to_g_param_spec_unref ptr
  return $! GParamSpec fPtr

-- | Construct a Haskell wrapper for the given 'GParamSpec', without
-- assuming ownership.
newGParamSpecFromPtr :: Ptr GParamSpec -> IO GParamSpec
newGParamSpecFromPtr ptr = do
  fPtr <- g_param_spec_ref ptr >>= newForeignPtr ptr_to_g_param_spec_unref
  return $! GParamSpec fPtr

-- | Add a reference to the given 'GParamSpec'.
refGParamSpec :: GParamSpec -> IO (Ptr GParamSpec)
refGParamSpec (GParamSpec fptr) = withForeignPtr fptr g_param_spec_ref

-- | Remove a reference to the given 'GParamSpec'.
unrefGParamSpec :: GParamSpec -> IO ()
unrefGParamSpec (GParamSpec fptr) = withForeignPtr fptr g_param_spec_unref
