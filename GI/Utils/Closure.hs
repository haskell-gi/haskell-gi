-- Some helper functions to create closures.
module GI.Utils.Closure
    ( Closure(..)
    , newCClosure
    , noClosure
    ) where

import Foreign
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import GI.Utils.BasicTypes
import GI.Utils.ManagedPtr (wrapBoxed)
import GI.Utils.Utils (safeFreeFunPtrPtr)

data Closure = Closure (ForeignPtr Closure)

noClosure :: Maybe Closure
noClosure = Nothing

instance ManagedPtr Closure where
    unsafeManagedPtrGetPtr (Closure x) = unsafeForeignPtrToPtr x
    touchManagedPtr        (Closure x) = touchForeignPtr x

foreign import ccall "g_closure_get_type" c_g_closure_get_type ::
    IO GType

instance BoxedObject Closure where
    boxedType _ = c_g_closure_get_type


foreign import ccall "g_cclosure_new" g_cclosure_new
    :: FunPtr a -> Ptr () -> FunPtr c -> IO (Ptr Closure)

foreign import ccall "g_closure_ref" g_closure_ref
    :: Ptr Closure -> IO (Ptr Closure)

foreign import ccall "g_closure_sink" g_closure_sink
    :: Ptr Closure -> IO ()

newCClosure :: FunPtr a -> IO Closure
newCClosure ptr = do
  closure <- g_cclosure_new ptr nullPtr safeFreeFunPtrPtr
  -- The Haskell runtime will manage the memory associated to the
  -- closure, so ref and sink to let GLib know this.
  g_closure_ref closure >>= g_closure_sink
  wrapBoxed Closure closure
