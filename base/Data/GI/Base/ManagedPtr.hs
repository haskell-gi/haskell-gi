{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, KindSignatures, ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

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
    , newManagedPtr_
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
    , checkInstanceType

    -- * Wrappers
    , newObject
    , wrapObject
    , releaseObject
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
import Control.Monad.Fix (mfix)

import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Maybe (isNothing, isJust)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

import Foreign.C (CInt(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.ForeignPtr (FinalizerPtr, touchForeignPtr, newForeignPtr_)
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Data.GI.Base.BasicTypes
import Data.GI.Base.CallStack (CallStack, HasCallStack,
                               prettyCallStack, callStack)
import Data.GI.Base.Utils

import qualified Data.Text as T

import System.IO (hPutStrLn, stderr)
import System.Environment (lookupEnv)

-- | Thin wrapper over `Foreign.Concurrent.newForeignPtr`.
newManagedPtr :: HasCallStack => Ptr a -> IO () -> IO (ManagedPtr a)
newManagedPtr ptr finalizer = do
  isDisownedRef <- newIORef Nothing
  dbgMode <- isJust <$> lookupEnv "HASKELL_GI_DEBUG_MEM"
  let dbgCallStack = if dbgMode
                     then Just callStack
                     else Nothing
  fPtr <- FC.newForeignPtr ptr (ownedFinalizer finalizer ptr dbgCallStack isDisownedRef)
  return $ ManagedPtr {
               managedForeignPtr = fPtr
             , managedPtrAllocCallStack = dbgCallStack
             , managedPtrIsDisowned = isDisownedRef
             }

-- | Run the finalizer for an owned pointer, assuming it has now been
-- disowned.
ownedFinalizer :: IO () -> Ptr a -> Maybe CallStack -> IORef (Maybe CallStack)
               -> IO ()
ownedFinalizer finalizer ptr allocCallStack callStackRef = do
  cs <- readIORef callStackRef
  -- cs will be @Just cs@ whenever the pointer has been disowned.
  when (isNothing cs) $ case allocCallStack of
                          Just acs -> do
                            printAllocDebug ptr acs
                            finalizer
                            dbgLog (T.pack "Released successfully.\n")
                          Nothing -> finalizer

-- | Print some debug diagnostics for an allocation.
printAllocDebug :: Ptr a -> CallStack -> IO ()
printAllocDebug ptr allocCS =
  (dbgLog . T.pack) ("Releasing <" <> show ptr <> ">. "
                     <> "Callstack for allocation was:\n"
                     <> prettyCallStack allocCS <> "\n\n")

foreign import ccall "dynamic"
   mkFinalizer :: FinalizerPtr a -> Ptr a -> IO ()

-- | Version of `newManagedPtr` taking a `FinalizerPtr` and a
-- corresponding `Ptr`, as in `Foreign.ForeignPtr.newForeignPtr`.
newManagedPtr' :: HasCallStack => FinalizerPtr a -> Ptr a -> IO (ManagedPtr a)
newManagedPtr' finalizer ptr = newManagedPtr ptr (mkFinalizer finalizer ptr)

-- | Thin wrapper over `Foreign.Concurrent.newForeignPtr_`.
newManagedPtr_ :: Ptr a -> IO (ManagedPtr a)
newManagedPtr_ ptr = do
  isDisownedRef <- newIORef Nothing
  fPtr <- newForeignPtr_ ptr
  return $ ManagedPtr {
               managedForeignPtr = fPtr
             , managedPtrAllocCallStack = Nothing
             , managedPtrIsDisowned = isDisownedRef
             }

-- | Do not run the finalizers upon garbage collection of the
-- `ManagedPtr`.
disownManagedPtr :: forall a b. (HasCallStack, ManagedPtrNewtype a) => a -> IO (Ptr b)
disownManagedPtr managed = do
  ptr <- unsafeManagedPtrGetPtr managed
  writeIORef (managedPtrIsDisowned c) (Just callStack)
  return (castPtr ptr)
    where c = toManagedPtr managed

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
    let c = toManagedPtr m
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
touchManagedPtr m = let c = toManagedPtr m
                    in (touchForeignPtr . managedForeignPtr) c

-- Safe casting machinery
foreign import ccall unsafe "check_object_type"
    c_check_object_type :: Ptr o -> CGType -> IO CInt

-- | Check whether the given object is an instance of the given type.
checkInstanceType :: (ManagedPtrNewtype o, TypedObject o) =>
                     o -> GType -> IO Bool
checkInstanceType obj (GType cgtype) = withManagedPtr obj $ \objPtr -> do
  check <- c_check_object_type objPtr cgtype
  return $ check /= 0

-- | Cast from one object type to another, checking that the cast is
-- valid. If it is not, we return `Nothing`. Usage:
--
-- > maybeWidget <- castTo Widget label
castTo :: forall o o'. (HasCallStack,
                        ManagedPtrNewtype o, TypedObject o,
                        ManagedPtrNewtype o', TypedObject o',
                        GObject o') =>
          (ManagedPtr o' -> o') -> o -> IO (Maybe o')
castTo constructor obj = do
  gtype <- glibType @o'
  isInstance <- checkInstanceType obj gtype
  if isInstance
    then return . Just . constructor . coerce $ toManagedPtr obj
    else return Nothing

-- | Cast a typed object to a new type (without any assumption that
-- both types descend from `GObject`), assuming that the cast will
-- succeed. This function will call `error` if the cast is illegal.
unsafeCastTo :: forall o o'. (HasCallStack,
                              ManagedPtrNewtype o, TypedObject o,
                              ManagedPtrNewtype o', TypedObject o') =>
                (ManagedPtr o' -> o') -> o -> IO o'
unsafeCastTo constructor obj = do
    gtype <- glibType @o'
    isInstance <- checkInstanceType obj gtype
    if not isInstance
      then do
      srcType <- glibType @o >>= gtypeName
      destType <- glibType @o' >>= gtypeName
      error $ "unsafeCastTo :: invalid conversion from " ++ srcType ++ " to "
        ++ destType ++ " requested."
      else return (constructor $ coerce $ toManagedPtr obj)

-- Reference counting for constructors
foreign import ccall "&dbg_g_object_unref"
    ptr_to_g_object_unref :: FunPtr (Ptr a -> IO ())

foreign import ccall "g_object_ref_sink" g_object_ref_sink ::
    Ptr a -> IO (Ptr a)

-- | Print a warning when receiving a null pointer in a function that
-- did not expect one, for easier debugging.
nullPtrWarning :: String -> CallStack -> IO ()
nullPtrWarning fn cs =
  hPutStrLn stderr ("WARNING: Trying to wrap a null pointer in " ++ quotedFn
                     ++ ", this may lead to crashes.\n\n"
                     ++ "• Callstack for the unsafe call to "
                     ++ quotedFn ++ ":\n"
                     ++ prettyCallStack cs ++ "\n\n"
                     ++ "This is probably a bug in the introspection data,\n"
                     ++ "please report it at https://github.com/haskell-gi/haskell-gi/issues")
  where quotedFn = "‘" ++ fn ++ "’"

-- | Construct a Haskell wrapper for a 'GObject', increasing its
-- reference count, or taking ownership of the floating reference if
-- there is one.
newObject :: (HasCallStack, GObject a, GObject b) =>
             (ManagedPtr a -> a) -> Ptr b -> IO a
newObject constructor ptr = do
  when (ptr == nullPtr) (nullPtrWarning "newObject" callStack)
  void $ g_object_ref_sink ptr
  fPtr <- newManagedPtr' ptr_to_g_object_unref $ castPtr ptr
  return $! constructor fPtr

-- | Same as 'newObject', but we steal ownership of the object.
wrapObject :: forall a b. (HasCallStack, GObject a, GObject b) =>
              (ManagedPtr a -> a) -> Ptr b -> IO a
wrapObject constructor ptr = do
  when (ptr == nullPtr) (nullPtrWarning "wrapObject" callStack)
  fPtr <- newManagedPtr' ptr_to_g_object_unref $ castPtr ptr
  return $! constructor fPtr

-- | Unref the given `GObject` and disown it. Use this if you want to
-- manually release the memory associated to a given `GObject`
-- (assuming that no other reference to the underlying C object exists)
-- before the garbage collector does it. It is typically not safe to
-- access the `GObject` after calling this function.
releaseObject :: (HasCallStack, GObject a) => a -> IO ()
releaseObject obj = do
  ptr <- disownObject obj
  dbgDealloc obj
  dbg_g_object_unref ptr

-- It is fine to use unsafe here, since all this does is schedule an
-- idle callback. The scheduling itself will never block for a long
-- time, or call back into Haskell.
foreign import ccall unsafe "dbg_g_object_unref"
        dbg_g_object_unref :: Ptr a -> IO ()

-- | Decrease the reference count of the given 'GObject'. The memory
-- associated with the object may be released if the reference count
-- reaches 0.
unrefObject :: (HasCallStack, GObject a) => a -> IO ()
unrefObject obj = withManagedPtr obj $ \ptr -> do
  dbgDealloc obj
  dbg_g_object_unref ptr

-- | Print some debug info (if the right environment valiable is set)
-- about the object being disowned.
foreign import ccall "dbg_g_object_disown"
        dbg_g_object_disown :: Ptr a -> IO ()

-- | Disown a GObject, that is, do not unref the associated foreign
-- GObject when the Haskell object gets garbage collected. Returns the
-- pointer to the underlying GObject.
disownObject :: (HasCallStack, GObject a) => a -> IO (Ptr b)
disownObject obj = withManagedPtr obj $ \ptr -> do
  dbgDealloc obj
  dbg_g_object_disown ptr
  castPtr <$> disownManagedPtr obj

-- It is fine to use unsafe here, since all this does is schedule an
-- idle callback. The scheduling itself will never block for a long
-- time, or call back into Haskell.
foreign import ccall unsafe "boxed_free_helper" boxed_free_helper ::
    CGType -> Ptr a -> IO ()

foreign import ccall "g_boxed_copy" g_boxed_copy ::
    CGType -> Ptr a -> IO (Ptr a)

-- | Construct a Haskell wrapper for the given boxed object. We make a
-- copy of the object.
newBoxed :: forall a. (HasCallStack, GBoxed a) => (ManagedPtr a -> a) -> Ptr a -> IO a
newBoxed constructor ptr = do
  GType gtype <- glibType @a
  ptr' <- g_boxed_copy gtype ptr
  fPtr <- newManagedPtr ptr' (boxed_free_helper gtype ptr')
  return $! constructor fPtr

-- | Like 'newBoxed', but we do not make a copy (we "steal" the passed
-- object, so now it is managed by the Haskell runtime).
wrapBoxed :: forall a. (HasCallStack, GBoxed a) => (ManagedPtr a -> a) -> Ptr a -> IO a
wrapBoxed constructor ptr = do
  GType gtype <- glibType @a
  fPtr <- newManagedPtr ptr (boxed_free_helper gtype ptr)
  return $! constructor fPtr

-- | Make a copy of the given boxed object.
copyBoxed :: forall a. (HasCallStack, GBoxed a) => a -> IO (Ptr a)
copyBoxed b = do
  GType gtype <- glibType @a
  withManagedPtr b (g_boxed_copy gtype)

-- | Like 'copyBoxed', but acting directly on a pointer, instead of a
-- managed pointer.
copyBoxedPtr :: forall a. GBoxed a => Ptr a -> IO (Ptr a)
copyBoxedPtr ptr = do
  GType gtype <- glibType @a
  g_boxed_copy gtype ptr

foreign import ccall "g_boxed_free" g_boxed_free ::
    CGType -> Ptr a -> IO ()

-- | Free the memory associated with a boxed object. Note that this
-- disowns the associated `ManagedPtr` via `disownManagedPtr`.
freeBoxed :: forall a. (HasCallStack, GBoxed a) => a -> IO ()
freeBoxed boxed = do
  GType gtype <- glibType @a
  ptr <- disownManagedPtr boxed
  dbgDealloc boxed
  g_boxed_free gtype ptr

-- | Disown a boxed object, that is, do not free the associated
-- foreign GBoxed when the Haskell object gets garbage
-- collected. Returns the pointer to the underlying `GBoxed`.
disownBoxed :: (HasCallStack, GBoxed a) => a -> IO (Ptr a)
disownBoxed = disownManagedPtr

-- | Wrap a pointer, taking ownership of it.
wrapPtr :: (HasCallStack, BoxedPtr a) => (ManagedPtr a -> a) -> Ptr a -> IO a
wrapPtr constructor ptr = mfix $ \wrapped -> do
  fPtr <- newManagedPtr ptr (boxedPtrFree wrapped)
  return $! constructor fPtr

-- | Wrap a pointer, making a copy of the data.
newPtr :: (HasCallStack, BoxedPtr a) => (ManagedPtr a -> a) -> Ptr a -> IO a
newPtr constructor ptr = do
  tmpWrap <- newManagedPtr_ ptr
  ptr' <- boxedPtrCopy (constructor tmpWrap)
  return $! ptr'

-- | Make a copy of a wrapped pointer using @memcpy@ into a freshly
-- allocated memory region of the given size.
copyBytes :: (HasCallStack, CallocPtr a) => Int -> Ptr a -> IO (Ptr a)
copyBytes size ptr = do
  ptr' <- boxedPtrCalloc
  memcpy ptr' ptr size
  return ptr'

foreign import ccall unsafe "g_thread_self" g_thread_self :: IO (Ptr ())

-- | Same as `dbgDeallocPtr`, but for `ManagedPtr`s, and no callstack
-- needs to be provided.
dbgDealloc :: (HasCallStack, ManagedPtrNewtype a) => a -> IO ()
dbgDealloc m = do
  env <- lookupEnv "HASKELL_GI_DEBUG_MEM"
  case env of
    Nothing -> return ()
    Just _ -> do
      let mPtr = toManagedPtr m
          ptr = (unsafeForeignPtrToPtr . managedForeignPtr) mPtr
      threadPtr <- g_thread_self
      hPutStrLn stderr ("Releasing <" ++ show ptr ++ "> from thread ["
                         ++ show threadPtr ++ "].\n"
                         ++ (case managedPtrAllocCallStack mPtr of
                               Just allocCS -> "• Callstack for allocation:\n"
                                               ++ prettyCallStack allocCS ++ "\n\n"
                               Nothing -> "")
                         ++ "• CallStack for deallocation:\n"
                         ++ prettyCallStack callStack ++ "\n")
