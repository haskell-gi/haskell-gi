{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, KindSignatures, ConstraintKinds #-}

-- | We wrap most objects in a "managed pointer", which is basically a
-- 'ForeignPtr' of the appropriate type together with a notion of
-- "disowning", which means not running the finalizers passed upon
-- construction of the object upon garbage collection. The routines in
-- this module deal with the memory management of such managed
-- pointers.

module Data.GI.Base.ManagedPtr
    (
    -- * Managed pointers
      newManagedPtr
    , newManagedPtr'
    , withManagedPtr
    , maybeWithManagedPtr
    , withManagedPtrList
    , withTransient
    , unsafeManagedPtrGetPtr
    , unsafeManagedPtrCastPtr
    , touchManagedPtr
    , disownManagedPtr

    -- * Safe casting
    , castTo
    , unsafeCastTo

    -- * Wrappers
    , newObject
    , wrapObject
    , unrefObject
    , disownObject
    , newBoxed
    , wrapBoxed
    , copyBoxed
    , copyBoxedPtr
    , freeBoxed
    , disownBoxed
    , wrapPtr
    , newPtr
    , copyBytes
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when, void)

import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Maybe (isNothing)

import Foreign.C (CInt(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.ForeignPtr (FinalizerPtr, touchForeignPtr, newForeignPtr_)
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Data.GI.Base.BasicTypes
import Data.GI.Base.CallStack (CallStack, HasCallStack,
                               prettyCallStack, callStack)
import Data.GI.Base.Utils

import System.IO (hPutStrLn, stderr)

-- | Thin wrapper over `Foreign.Concurrent.newForeignPtr`.
newManagedPtr :: Ptr a -> IO () -> IO (ManagedPtr a)
newManagedPtr ptr finalizer = do
  let ownedFinalizer :: IORef (Maybe CallStack) -> IO ()
      ownedFinalizer callStackRef = do
        cs <- readIORef callStackRef
        when (isNothing cs) finalizer
  isDisownedRef <- newIORef Nothing
  fPtr <- FC.newForeignPtr ptr (ownedFinalizer isDisownedRef)
  return $ ManagedPtr {
               managedForeignPtr = fPtr
             , managedPtrIsDisowned = isDisownedRef
             }

foreign import ccall "dynamic"
   mkFinalizer :: FinalizerPtr a -> Ptr a -> IO ()

-- | Version of `newManagedPtr` taking a `FinalizerPtr` and a
-- corresponding `Ptr`, as in `Foreign.ForeignPtr.newForeignPtr`.
newManagedPtr' :: FinalizerPtr a -> Ptr a -> IO (ManagedPtr a)
newManagedPtr' finalizer ptr = newManagedPtr ptr (mkFinalizer finalizer ptr)

-- | Thin wrapper over `Foreign.Concurrent.newForeignPtr_`.
newManagedPtr_ :: Ptr a -> IO (ManagedPtr a)
newManagedPtr_ ptr = do
  isDisownedRef <- newIORef Nothing
  fPtr <- newForeignPtr_ ptr
  return $ ManagedPtr {
               managedForeignPtr = fPtr
             , managedPtrIsDisowned = isDisownedRef
             }

-- | Do not run the finalizers upon garbage collection of the
-- `ManagedPtr`, for the given reason. If later code tries to access
-- the underlying pointer the given reason will be printed as part of
-- the error message.
disownManagedPtr :: forall a. (HasCallStack, ManagedPtrNewtype a) => a -> IO (Ptr a)
disownManagedPtr managed = do
  ptr <- unsafeManagedPtrGetPtr managed
  writeIORef (managedPtrIsDisowned c) (Just callStack)
  return ptr
    where c = coerce managed :: ManagedPtr ()

-- | Perform an IO action on the 'Ptr' inside a managed pointer.
withManagedPtr :: (HasCallStack, ManagedPtrNewtype a) => a -> (Ptr a -> IO c) -> IO c
withManagedPtr managed action = do
  ptr <- unsafeManagedPtrGetPtr managed
  result <- action ptr
  touchManagedPtr managed
  return result

-- | Like `withManagedPtr`, but accepts a `Maybe` type. If the passed
-- value is `Nothing` the inner action will be executed with a
-- `nullPtr` argument.
maybeWithManagedPtr :: (HasCallStack, ManagedPtrNewtype a) => Maybe a -> (Ptr a -> IO c) -> IO c
maybeWithManagedPtr Nothing action = action nullPtr
maybeWithManagedPtr (Just managed) action = withManagedPtr managed action

-- | Perform an IO action taking a list of 'Ptr' on a list of managed
-- pointers.
withManagedPtrList :: (HasCallStack, ManagedPtrNewtype a) => [a] -> ([Ptr a] -> IO c) -> IO c
withManagedPtrList managedList action = do
  ptrs <- mapM unsafeManagedPtrGetPtr managedList
  result <- action ptrs
  mapM_ touchManagedPtr managedList
  return result

-- | Perform the IO action with a transient managed pointer. The
-- managed pointer will be valid while calling the action, but will be
-- disowned as soon as the action finished.
withTransient :: (HasCallStack, ManagedPtrNewtype a)
              => (ManagedPtr a -> a) -> Ptr a -> (a -> IO b) -> IO b
withTransient constructor ptr action = do
  managed <- constructor <$> newManagedPtr_ ptr
  r <- action managed
  _ <- disownManagedPtr managed
  return r

-- | Return the 'Ptr' in a given managed pointer. As the name says,
-- this is potentially unsafe: the given 'Ptr' may only be used
-- /before/ a call to 'touchManagedPtr'. This function is of most
-- interest to the autogenerated bindings, for hand-written code
-- 'withManagedPtr' is almost always a better choice.
unsafeManagedPtrGetPtr :: (HasCallStack, ManagedPtrNewtype a) => a -> IO (Ptr a)
unsafeManagedPtrGetPtr = unsafeManagedPtrCastPtr

-- | Same as 'unsafeManagedPtrGetPtr', but is polymorphic on the
-- return type.
unsafeManagedPtrCastPtr :: forall a b. (HasCallStack, ManagedPtrNewtype a) =>
                           a -> IO (Ptr b)
unsafeManagedPtrCastPtr m = do
    let c = coerce m :: ManagedPtr ()
        ptr = (castPtr . unsafeForeignPtrToPtr . managedForeignPtr) c
    disowned <- readIORef (managedPtrIsDisowned c)
    maybe (return ptr) (notOwnedWarning ptr) disowned

-- | Print a warning when we try to access a disowned foreign ptr.
notOwnedWarning :: HasCallStack => Ptr a -> CallStack -> IO (Ptr a)
notOwnedWarning ptr cs = do
  hPutStrLn stderr ("WARNING: Accessing a disowned pointer <" ++ show ptr
                     ++ ">, this may lead to crashes.\n\n"
                     ++ "• Callstack for the unsafe access to the pointer:\n"
                     ++ prettyCallStack callStack ++ "\n\n"
                     ++ "• The pointer was disowned at:\n"
                     ++ prettyCallStack cs ++ "\n")
  return ptr

-- | Ensure that the 'Ptr' in the given managed pointer is still alive
-- (i.e. it has not been garbage collected by the runtime) at the
-- point that this is called.
touchManagedPtr :: forall a. ManagedPtrNewtype a => a -> IO ()
touchManagedPtr m = let c = coerce m :: ManagedPtr ()
                    in (touchForeignPtr . managedForeignPtr) c

-- Safe casting machinery
foreign import ccall unsafe "check_object_type"
    c_check_object_type :: Ptr o -> CGType -> CInt

-- | Cast to the given type, checking that the cast is valid. If it is
-- not, we return `Nothing`. Usage:
--
-- > maybeWidget <- castTo Widget label
castTo :: forall o o'. (GObject o, GObject o') =>
          (ManagedPtr o' -> o') -> o -> IO (Maybe o')
castTo constructor obj =
    withManagedPtr obj $ \objPtr -> do
      GType t <- gobjectType (undefined :: o')
      if c_check_object_type objPtr t /= 1
        then return Nothing
        else Just <$> newObject constructor objPtr

-- | Cast to the given type, assuming that the cast will succeed. This
-- function will call `error` if the cast is illegal.
unsafeCastTo :: forall o o'. (HasCallStack, GObject o, GObject o') =>
                (ManagedPtr o' -> o') -> o -> IO o'
unsafeCastTo constructor obj =
  withManagedPtr obj $ \objPtr -> do
    GType t <- gobjectType (undefined :: o')
    if c_check_object_type objPtr t /= 1
      then do
      srcType <- gobjectType obj >>= gtypeName
      destType <- gobjectType (undefined :: o') >>= gtypeName
      error $ "unsafeCastTo :: invalid conversion from " ++ srcType ++ " to "
        ++ destType ++ " requested."
      else newObject constructor objPtr

-- Reference counting for constructors
foreign import ccall "&dbg_g_object_unref"
    ptr_to_g_object_unref :: FunPtr (Ptr a -> IO ())

foreign import ccall "g_object_ref_sink" g_object_ref_sink ::
    Ptr a -> IO (Ptr a)

-- | Construct a Haskell wrapper for a 'GObject', increasing its
-- reference count, or taking ownership of the floating reference if
-- there is one.
newObject :: (GObject a, GObject b) => (ManagedPtr a -> a) -> Ptr b -> IO a
newObject constructor ptr = do
  void $ g_object_ref_sink ptr
  fPtr <- newManagedPtr' ptr_to_g_object_unref $ castPtr ptr
  return $! constructor fPtr

-- | Same as 'newObject', but we steal ownership of the object.
wrapObject :: forall a b. (GObject a, GObject b) =>
              (ManagedPtr a -> a) -> Ptr b -> IO a
wrapObject constructor ptr = do
  fPtr <- newManagedPtr' ptr_to_g_object_unref $ castPtr ptr
  return $! constructor fPtr


foreign import ccall "dbg_g_object_unref"
        dbg_g_object_unref :: Ptr a -> IO ()

-- | Decrease the reference count of the given 'GObject'. The memory
-- associated with the object may be released if the reference count
-- reaches 0.
unrefObject :: GObject a => a -> IO ()
unrefObject obj = withManagedPtr obj dbg_g_object_unref

-- | Print some debug info (if the right environment valiable is set)
-- about the object being disowned.
foreign import ccall "dbg_g_object_disown"
        dbg_g_object_disown :: Ptr a -> IO ()

-- | Disown a GObject, that is, do not unref the associated foreign
-- GObject when the Haskell object gets garbage collected. Returns the
-- pointer to the underlying GObject.
disownObject :: GObject a => a -> IO (Ptr b)
disownObject obj = withManagedPtr obj $ \ptr -> do
                     dbg_g_object_disown ptr
                     castPtr <$> disownManagedPtr obj

foreign import ccall "boxed_free_helper" boxed_free_helper ::
    CGType -> Ptr a -> IO ()

foreign import ccall "g_boxed_copy" g_boxed_copy ::
    CGType -> Ptr a -> IO (Ptr a)

-- | Construct a Haskell wrapper for the given boxed object. We make a
-- copy of the object.
newBoxed :: forall a. BoxedObject a => (ManagedPtr a -> a) -> Ptr a -> IO a
newBoxed constructor ptr = do
  GType gtype <- boxedType (undefined :: a)
  ptr' <- g_boxed_copy gtype ptr
  fPtr <- newManagedPtr ptr' (boxed_free_helper gtype ptr')
  return $! constructor fPtr

-- | Like 'newBoxed', but we do not make a copy (we "steal" the passed
-- object, so now it is managed by the Haskell runtime).
wrapBoxed :: forall a. BoxedObject a => (ManagedPtr a -> a) -> Ptr a -> IO a
wrapBoxed constructor ptr = do
  GType gtype <- boxedType (undefined :: a)
  fPtr <- newManagedPtr ptr (boxed_free_helper gtype ptr)
  return $! constructor fPtr

-- | Make a copy of the given boxed object.
copyBoxed :: forall a. BoxedObject a => a -> IO (Ptr a)
copyBoxed b = do
  GType gtype <- boxedType b
  withManagedPtr b (g_boxed_copy gtype)

-- | Like 'copyBoxed', but acting directly on a pointer, instead of a
-- managed pointer.
copyBoxedPtr :: forall a. BoxedObject a => Ptr a -> IO (Ptr a)
copyBoxedPtr ptr = do
  GType gtype <- boxedType (undefined :: a)
  g_boxed_copy gtype ptr

foreign import ccall "g_boxed_free" g_boxed_free ::
    CGType -> Ptr a -> IO ()

-- | Free the memory associated with a boxed object
freeBoxed :: forall a. (HasCallStack, BoxedObject a) => a -> IO ()
freeBoxed boxed = do
  GType gtype <- boxedType (undefined :: a)
  ptr <- unsafeManagedPtrGetPtr boxed
  g_boxed_free gtype ptr
  touchManagedPtr boxed

-- | Disown a boxed object, that is, do not free the associated
-- foreign GBoxed when the Haskell object gets garbage
-- collected. Returns the pointer to the underlying `BoxedObject`.
disownBoxed :: BoxedObject a => a -> IO (Ptr a)
disownBoxed = disownManagedPtr

-- | Wrap a pointer, taking ownership of it.
wrapPtr :: WrappedPtr a => (ManagedPtr a -> a) -> Ptr a -> IO a
wrapPtr constructor ptr = do
  fPtr <- case wrappedPtrFree of
            Nothing -> newManagedPtr_ ptr
            Just finalizer -> newManagedPtr' finalizer ptr
  return $! constructor fPtr

-- | Wrap a pointer, making a copy of the data.
newPtr :: WrappedPtr a => (ManagedPtr a -> a) -> Ptr a -> IO a
newPtr constructor ptr = do
  tmpWrap <- newManagedPtr_ ptr
  ptr' <- wrappedPtrCopy (constructor tmpWrap)
  return $! ptr'

-- | Make a copy of a wrapped pointer using @memcpy@ into a freshly
-- allocated memory region of the given size.
copyBytes :: WrappedPtr a => Int -> Ptr a -> IO (Ptr a)
copyBytes size ptr = do
  ptr' <- wrappedPtrCalloc
  memcpy ptr' ptr size
  return ptr'
