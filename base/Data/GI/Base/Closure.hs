-- Some helper functions to create closures.
module Data.GI.Base.Closure
    ( Closure(..)
    , newCClosure
    , noClosure
    ) where

import Foreign

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.GI.Base.BasicTypes
import Data.GI.Base.ManagedPtr (wrapBoxed)

newtype Closure = Closure (ManagedPtr Closure)

noClosure :: Maybe Closure
noClosure = Nothing

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

-- Releasing the `FunPtr` for the signal handler.
foreign import ccall "& haskell_gi_release_signal_closure"
    ptr_to_release_closure :: FunPtr (Ptr () -> Ptr () -> IO ())

-- | Create a new `Closure` holding the given `FunPtr`. Note that
-- after calling this the `FunPtr` will be freed whenever the
-- `Closure` is garbage collected, so it is generally not safe to
-- refer to the generated `FunPtr` after this function returns.
newCClosure :: MonadIO m => FunPtr a -> m Closure
newCClosure ptr = liftIO $ do
  closure <- g_cclosure_new ptr nullPtr ptr_to_release_closure
  -- The Haskell runtime will manage the memory associated to the
  -- closure, so ref and sink to let GLib know this.
  g_closure_ref closure >>= g_closure_sink
  wrapBoxed Closure closure
