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

import Foreign

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.GI.Base.BasicTypes
import Data.GI.Base.CallStack (HasCallStack)
import Data.GI.Base.ManagedPtr (newBoxed, wrapBoxed, freeBoxed,
                                disownManagedPtr)

-- | The basic type. This corresponds to a wrapped @GClosure@ on the C
-- side, which is a boxed object.
newtype GClosure = GClosure (ManagedPtr GClosure)

-- | A convenience alias for @Nothing :: Maybe GClosure@.
noGClosure :: Maybe GClosure
noGClosure = Nothing

foreign import ccall "g_closure_get_type" c_g_closure_get_type ::
    IO GType

instance BoxedObject GClosure where
    boxedType _ = c_g_closure_get_type

foreign import ccall "g_cclosure_new" g_cclosure_new
    :: FunPtr a -> Ptr () -> FunPtr c -> IO (Ptr GClosure)

-- Releasing the `FunPtr` for the signal handler.
foreign import ccall "& haskell_gi_release_signal_closure"
    ptr_to_release_closure :: FunPtr (Ptr () -> Ptr () -> IO ())

-- | Create a new `GClosure` holding the given `FunPtr`. Note that
-- after calling this the `FunPtr` will be freed whenever the
-- `GClosure` is garbage collected, so it is generally not safe to
-- refer to the generated `FunPtr` after this function returns.
newGClosure :: MonadIO m => FunPtr a -> m GClosure
newGClosure ptr = liftIO $ do
  closure <- g_cclosure_new ptr nullPtr ptr_to_release_closure
  wrapGClosurePtr closure

-- | Take ownership of a passed in 'Ptr' to a 'GClosure'.
wrapGClosurePtr :: Ptr GClosure -> IO GClosure
wrapGClosurePtr = wrapBoxed GClosure

-- | Construct a Haskell wrapper for the 'GClosure', without assuming
-- ownership.
newGClosureFromPtr :: Ptr GClosure -> IO GClosure
newGClosureFromPtr = newBoxed GClosure

-- | Decrease the reference count of the given 'GClosure'. If the
-- reference count reaches 0 the memory will be released.
unrefGClosure :: (HasCallStack, MonadIO m) => GClosure -> m ()
unrefGClosure closure = liftIO $ freeBoxed closure

-- | Disown (that is, remove from te purview of the Haskell Garbage
-- Collector) the given 'GClosure'.
disownGClosure :: GClosure -> IO (Ptr GClosure)
disownGClosure = disownManagedPtr
