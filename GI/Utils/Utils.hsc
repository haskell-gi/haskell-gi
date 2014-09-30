{-# LANGUAGE ScopedTypeVariables #-}
module GI.Utils.Utils
    ( whenJust
    , maybeFromPtr
    , callocBytes
    , callocBoxedBytes
    , allocBytes
    , allocMem
    , freeMem
    , memcpy
    , safeFreeFunPtr
    , safeFreeFunPtrPtr
    , maybeReleaseFunPtr
    ) where

#include <glib-object.h>

import Foreign (peek)
import Foreign.Storable (Storable(..))
import Data.Word
import Foreign.Ptr
import Foreign.C.Types (CSize(..))
import Control.Monad (void)

import GI.Utils.BasicTypes (GType, BoxedObject(..))

-- When the given value is of "Just a" form, execute the given action,
-- otherwise do nothing.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = return ()

maybeFromPtr :: Ptr a -> Maybe (Ptr a)
maybeFromPtr ptr = if ptr == nullPtr
                   then Nothing
                   else Just ptr

foreign import ccall "g_malloc0" g_malloc0 ::
    #{type gsize} -> IO (Ptr a)

{-# INLINE callocBytes #-}
callocBytes :: Int -> IO (Ptr a)
callocBytes n =  g_malloc0 (fromIntegral n)

foreign import ccall "g_boxed_copy" g_boxed_copy ::
    GType -> Ptr a -> IO (Ptr a)

-- Make a zero filled allocation of n bytes for a boxed object. The
-- difference with a normal callocBytes is that the returned memory is
-- allocated using whatever memory allocator g_boxed_copy uses, which
-- in particular may well be different from a plain g_malloc. In
-- particular g_slice_alloc is often used for allocating boxed
-- objects, which are then freed using g_slice_free.
callocBoxedBytes :: forall a. BoxedObject a => Int -> IO (Ptr a)
callocBoxedBytes n = do
  ptr <- callocBytes n
  gtype <- boxedType (undefined :: a)
  result <- g_boxed_copy gtype ptr
  freeMem ptr
  return result

foreign import ccall "g_malloc" g_malloc ::
    #{type gsize} -> IO (Ptr a)

{-# INLINE allocBytes #-}
allocBytes :: Integral a => a -> IO (Ptr b)
allocBytes n = g_malloc (fromIntegral n)

-- A version of malloc that uses the GLib allocator.
{-# INLINE allocMem #-}
allocMem :: forall a. Storable a => IO (Ptr a)
allocMem = g_malloc $ (fromIntegral . sizeOf) (undefined :: a)

foreign import ccall "g_free" freeMem :: Ptr a -> IO ()

foreign import ccall unsafe "string.h memcpy" _memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr ())

{-# INLINE memcpy #-}
memcpy :: Ptr a -> Ptr b -> Int -> IO ()
memcpy dest src n = void $ _memcpy dest src (fromIntegral n)

-- Same as freeHaskellFunPtr, but it does nothing when given a
-- nullPtr.
foreign import ccall "safeFreeFunPtr" safeFreeFunPtr ::
    Ptr a -> IO ()

foreign import ccall "& safeFreeFunPtr" safeFreeFunPtrPtr ::
    FunPtr (Ptr a -> IO ())

maybeReleaseFunPtr :: Maybe (Ptr (FunPtr a)) -> IO ()
maybeReleaseFunPtr Nothing = return ()
maybeReleaseFunPtr (Just f) = do
  peek f >>= freeHaskellFunPtr
  freeMem f
