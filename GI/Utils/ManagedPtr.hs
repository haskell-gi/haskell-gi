{-# LANGUAGE ScopedTypeVariables #-}
module GI.Utils.ManagedPtr
    ( withManagedPtr
    , castTo
    , newObject
    , wrapObject
    , refObject
    , unrefObject
    , newBoxed
    , wrapBoxed
    , copyBoxed
    , copyBoxedPtr
    , freeBoxed
    , wrapPtr
    , newPtr
    ) where

import Foreign.Safe (finalizerFree, poke)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad (when, void)
import Control.Applicative ((<$>))

import GI.Utils.BasicTypes
import GI.Utils.Utils

withManagedPtr :: ManagedPtr a => a -> (Ptr a -> IO c) -> IO c
withManagedPtr managed action = do
  let ptr = unsafeManagedPtrGetPtr managed
  result <- action ptr
  touchManagedPtr managed
  return result

-- Safe casting machinery
foreign import ccall unsafe "check_object_type"
    c_check_object_type :: Ptr o -> GType -> CInt

castTo :: forall o o'. (ManagedPtr o, GObject o, GObject o') =>
          (ForeignPtr o' -> o') -> String -> o -> IO o'
castTo constructor typeName obj =
    withManagedPtr obj $ \objPtr -> do
      t <- gobjectType (undefined :: o')
      when (c_check_object_type objPtr t /= 1) $
         error $ "Cannot cast object to " ++ typeName
      newObject constructor objPtr

-- Reference counting for constructors
foreign import ccall "&g_object_unref"
    ptr_to_g_object_unref :: FunPtr (Ptr a -> IO ())

foreign import ccall "g_object_ref" g_object_ref ::
    Ptr a -> IO (Ptr a)

-- Construct a Haskell wrapper for a GObject, making a copy.
newObject :: (GObject a, GObject b) => (ForeignPtr a -> a) -> Ptr b -> IO a
newObject constructor ptr = do
  void $ g_object_ref ptr
  fPtr <- newForeignPtr ptr_to_g_object_unref $ castPtr ptr
  return $! constructor fPtr

foreign import ccall "g_object_ref_sink" g_object_ref_sink ::
    Ptr a -> IO (Ptr a)

-- Same as newObject, but we take ownership of the object, which is
-- typically floating, so we use g_object_ref_sink. Notice that the
-- semantics here are a little bit funky: some objects (such as
-- GtkWindow, see the code about "user_ref_count" in gtkwindow.c in
-- the gtk+ distribution) are created _without_ the floating flag,
-- since they own a reference to themselves. So, wrapping them is
-- really about adding a ref. If we add the ref, when Haskell drops
-- the last ref to the GObject it will g_object_unref, and the window
-- will g_object_unref itself upon destruction, so by the end we don't
-- leak memory. If we don't add the ref, there will be two
-- g_object_unrefs acting on the object (one from Haskell and one from
-- the GtkWindow destroy) when the object is destroyed and the second
-- one will give a segfault.
--
-- This is the story for GInitiallyUnowned objects (e.g. anything that
-- is a descendant from GtkWidget). For objects that are not initially
-- floating (i.e. not descendents of GInitiallyUnowned) we simply take
-- control of the reference.
wrapObject :: forall a b. (GObject a, GObject b) =>
              (ForeignPtr a -> a) -> Ptr b -> IO a
wrapObject constructor ptr = do
  when (gobjectIsInitiallyUnowned (undefined :: a)) $
       void $ g_object_ref_sink ptr
  fPtr <- newForeignPtr ptr_to_g_object_unref $ castPtr ptr
  return $! constructor fPtr

refObject :: (ManagedPtr a, GObject a, GObject b) => a -> IO (Ptr b)
refObject obj = castPtr <$> withManagedPtr obj g_object_ref

foreign import ccall "g_object_unref" g_object_unref ::
    Ptr a -> IO ()

unrefObject :: (ManagedPtr a, GObject a) => a -> IO ()
unrefObject obj = withManagedPtr obj g_object_unref

foreign import ccall "& boxed_free_helper" boxed_free_helper ::
    FunPtr (Ptr env -> Ptr a -> IO ())

foreign import ccall "g_boxed_copy" g_boxed_copy ::
    GType -> Ptr a -> IO (Ptr a)

-- Construct a Haskell wrapper for the given boxed object. We make a
-- copy of the object.
newBoxed :: forall a. BoxedObject a => (ForeignPtr a -> a) -> Ptr a -> IO a
newBoxed constructor ptr = do
  gtype <- boxedType (undefined :: a)
  env <- allocMem :: IO (Ptr GType)   -- Will be freed by boxed_free_helper
  poke env gtype
  ptr' <- g_boxed_copy gtype ptr
  fPtr <- newForeignPtrEnv boxed_free_helper env ptr'
  return $! constructor fPtr

-- Like newBoxed, but we do not make a copy (we "steal" the passed
-- object, so now it is managed by the Haskell GC).
wrapBoxed :: forall a. BoxedObject a => (ForeignPtr a -> a) -> Ptr a -> IO a
wrapBoxed constructor ptr = do
  gtype <- boxedType (undefined :: a)
  env <- allocMem :: IO (Ptr GType)   -- Will be freed by boxed_free_helper
  poke env gtype
  fPtr <- newForeignPtrEnv boxed_free_helper env ptr
  return $! constructor fPtr

-- Make a copy of the given boxed object.
copyBoxed :: forall a. (BoxedObject a, ManagedPtr a) => a -> IO (Ptr a)
copyBoxed boxed = withManagedPtr boxed copyBoxedPtr

copyBoxedPtr :: forall a. BoxedObject a => Ptr a -> IO (Ptr a)
copyBoxedPtr ptr = do
  gtype <- boxedType (undefined :: a)
  g_boxed_copy gtype ptr

foreign import ccall "g_boxed_free" g_boxed_free ::
    GType -> Ptr a -> IO ()

-- Free the memory associated with a boxed object
freeBoxed :: forall a. (BoxedObject a, ManagedPtr a) => a -> IO ()
freeBoxed boxed = do
  gtype <- boxedType (undefined :: a)
  let ptr = unsafeManagedPtrGetPtr boxed
  g_boxed_free gtype ptr
  touchManagedPtr boxed

-- Wrap a pointer, taking ownership of it.
wrapPtr :: (ForeignPtr a -> a) -> Ptr a -> IO a
wrapPtr constructor ptr = do
  fPtr <- newForeignPtr finalizerFree ptr
  return $! constructor fPtr

-- Wrap a pointer to n bytes, making a copy of the data.
newPtr :: Int -> (ForeignPtr a -> a) -> Ptr a -> IO a
newPtr n constructor ptr = do
  ptr' <- callocBytes n :: IO (Ptr a)
  memcpy ptr' ptr n
  fPtr <- newForeignPtr finalizerFree ptr'
  return $! constructor fPtr
