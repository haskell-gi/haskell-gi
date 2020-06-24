{-# LANGUAGE TypeFamilies, DataKinds #-}
-- | Some helper functions for dealing with @GClosure@s.
module Data.GI.Base.GClosure
    ( GClosure(..)
    , newGClosure
    , wrapGClosurePtr
    , newGClosureFromPtr
    , noGClosure
    , unrefGClosure
    , disownGClosure
    ) where

import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.C (CInt(..))

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.GI.Base.BasicTypes
import Data.GI.Base.CallStack (HasCallStack)
import Data.GI.Base.ManagedPtr (newBoxed, newManagedPtr',
                                disownManagedPtr, withManagedPtr)
import Data.GI.Base.Overloading (ParentTypes, HasParentTypes)

-- | The basic type. This corresponds to a wrapped @GClosure@ on the C
-- side, which is a boxed object.
newtype GClosure a = GClosure (ManagedPtr (GClosure a))

-- | A convenience alias for @Nothing :: Maybe (GClosure a)@.
noGClosure :: Maybe (GClosure a)
noGClosure = Nothing

foreign import ccall "g_closure_get_type" c_g_closure_get_type ::
    IO GType

-- | There are no types in the bindings that a closure can be safely
-- cast to.
type instance ParentTypes (GClosure a) = '[]
instance HasParentTypes (GClosure a)

-- | Find the associated `GType` for the given closure.
instance TypedObject (GClosure a) where
  glibType = c_g_closure_get_type

-- | `GClosure`s are registered as boxed in the GLib type system.
instance GBoxed (GClosure a)

foreign import ccall "g_cclosure_new" g_cclosure_new
    :: FunPtr a -> Ptr () -> FunPtr c -> IO (Ptr (GClosure a))

-- Releasing the `FunPtr` for the signal handler.
foreign import ccall "& haskell_gi_release_signal_closure"
    ptr_to_release_closure :: FunPtr (Ptr () -> Ptr () -> IO ())

-- | Create a new `GClosure` holding the given `FunPtr`. Note that
-- after calling this the `FunPtr` will be freed whenever the
-- `GClosure` is garbage collected, so it is generally not safe to
-- refer to the generated `FunPtr` after this function returns.
newGClosure :: MonadIO m => FunPtr a -> m (GClosure a)
newGClosure ptr = liftIO $ do
  closure <- g_cclosure_new ptr nullPtr ptr_to_release_closure
  wrapGClosurePtr closure

foreign import ccall g_closure_ref :: Ptr (GClosure a) -> IO (Ptr (GClosure a))
foreign import ccall g_closure_sink :: Ptr (GClosure a) -> IO ()
foreign import ccall g_closure_unref :: Ptr (GClosure a) -> IO ()
foreign import ccall "&g_closure_unref" ptr_to_g_closure_unref ::
        FunPtr (Ptr (GClosure a) -> IO ())

foreign import ccall "haskell_gi_g_closure_is_floating" g_closure_is_floating ::
        Ptr (GClosure a) -> IO CInt

-- | Take ownership of a passed in 'Ptr' to a 'GClosure'.
wrapGClosurePtr :: Ptr (GClosure a) -> IO (GClosure a)
wrapGClosurePtr closurePtr = do
  floating <- g_closure_is_floating closurePtr
  when (floating /= 0) $ do
    _ <- g_closure_ref closurePtr
    g_closure_sink closurePtr
  fPtr <- newManagedPtr' ptr_to_g_closure_unref closurePtr
  return $! GClosure fPtr

-- | Construct a Haskell wrapper for the 'GClosure', without assuming
-- ownership.
newGClosureFromPtr :: Ptr (GClosure a) -> IO (GClosure a)
newGClosureFromPtr = newBoxed GClosure

-- | Decrease the reference count of the given 'GClosure'. If the
-- reference count reaches 0 the memory will be released.
unrefGClosure :: (HasCallStack, MonadIO m) => GClosure a -> m ()
unrefGClosure closure = liftIO $ withManagedPtr closure g_closure_unref

-- | Disown (that is, remove from te purview of the Haskell Garbage
-- Collector) the given 'GClosure'.
disownGClosure :: GClosure a -> IO (Ptr (GClosure a))
disownGClosure = disownManagedPtr
