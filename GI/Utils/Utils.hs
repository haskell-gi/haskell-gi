module GI.Utils.Utils
    ( whenJust
    , maybeFromPtr
    , callocBytes
    , memcpy
    , safeFreeFunPtr
    , safeFreeFunPtrPtr
    , maybeReleaseFunPtr
    ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Foreign (peek, free)
import Foreign.Ptr
import Foreign.C.Types (CSize(..))
import Control.Monad (when, void)

-- When the given value is of "Just a" form, execute the given action,
-- otherwise do nothing.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = return ()

maybeFromPtr :: Ptr a -> Maybe (Ptr a)
maybeFromPtr ptr = if ptr == nullPtr
                   then Nothing
                   else Just ptr

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

-- Same as freeHaskellFunPtr, but it does nothing when given a
-- nullPtr.
foreign import ccall unsafe "safeFreeFunPtr" safeFreeFunPtr ::
    Ptr a -> IO ()

foreign import ccall unsafe "& safeFreeFunPtr" safeFreeFunPtrPtr ::
    FunPtr (Ptr a -> IO ())

-- We are not supposed to call freeHaskellFunPtr from the Haskell code
-- references by the FunPtr itself, but for callbacks of
-- ScopeTypeTypeAsync we only know that we can release the FunPtr from
-- _inside_ the FunPtr (the semantics is that the callback will be
-- called exactly once, and there's no associated GDestroyNotify to
-- separately free resources).

-- The following is a bit of hack, we create an idle function that
-- frees the resources when invoked.
foreign import ccall unsafe "releaseFunPtr" releaseFunPtr ::
    FunPtr a -> IO ()

maybeReleaseFunPtr :: Maybe (Ptr (FunPtr a)) -> IO ()
maybeReleaseFunPtr Nothing = return ()
maybeReleaseFunPtr (Just f) = do
  peek f >>= releaseFunPtr
  free f
