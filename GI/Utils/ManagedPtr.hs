module GI.Utils.ManagedPtr
    ( ManagedPtr(..)
    , makeNewObject
    ) where

import Foreign.Safe

class ManagedPtr a where
    unsafeManagedPtrGetPtr :: a -> Ptr a
    touchManagedPtr        :: a -> IO ()

-- Reference counting for constructors
foreign import ccall unsafe "&g_object_unref"
    ptr_to_g_object_unref :: FunPtr (Ptr a -> IO ())

foreign import ccall "g_object_ref_sink" g_object_ref_sink ::
    Ptr () -> IO (Ptr ())

makeNewObject :: (ForeignPtr a -> a) -> Ptr b -> IO a
makeNewObject constructor ptr = do
    _ <- g_object_ref_sink $ castPtr ptr
    fPtr <- newForeignPtr ptr_to_g_object_unref $ castPtr ptr
    return $! constructor fPtr
