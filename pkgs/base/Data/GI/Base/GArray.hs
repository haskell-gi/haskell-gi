-- | Utilities for dealing with `GArray` types.
module Data.GI.Base.GArray
  ( allocGArray
  ) where

import Foreign.C (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)

import Data.GI.Base.BasicTypes (GArray(..))

-- | Args are zero_terminated, clear_, element_size
foreign import ccall g_array_new :: CInt -> CInt -> CUInt -> IO (Ptr (GArray a))

-- | Allocate a `GArray` with elements of the given size.
allocGArray :: CUInt -> IO (Ptr (GArray a))
allocGArray size = g_array_new 0 1 size
