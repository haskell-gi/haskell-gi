module GI.Utils.Utils
    ( whenJust
    , callocBytes
    , memcpy
    ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.C.Types (CSize(..))
import Control.Monad (when, void)

-- When the given value is of "Just a" form, execute the given action,
-- otherwise do nothing.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = return ()

foreign import ccall unsafe "stdlib.h calloc" _calloc :: CSize -> CSize ->
                                                         IO (Ptr a)

{-# INLINE callocBytes #-}
callocBytes :: Int -> IO (Ptr a)
callocBytes n = do
  ptr <- _calloc 1 (fromIntegral n)
  when (ptr == nullPtr) $
       hPutStrLn stderr "GI.Utils.calloc failed" >> exitFailure
  return ptr

foreign import ccall unsafe "string.h memcpy" _memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr ())

{-# INLINE memcpy #-}
memcpy :: Ptr a -> Ptr b -> Int -> IO ()
memcpy dest src n = void $ _memcpy dest src (fromIntegral n)
