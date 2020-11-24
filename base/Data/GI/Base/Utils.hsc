{-# LANGUAGE ScopedTypeVariables, TupleSections, OverloadedStrings,
    FlexibleContexts, ConstraintKinds, TypeApplications #-}
{- | Assorted utility functions for bindings. -}
module Data.GI.Base.Utils
    ( whenJust
    , maybeM
    , maybeFromPtr
    , mapFirst
    , mapFirstA
    , mapSecond
    , mapSecondA
    , convertIfNonNull
    , convertFunPtrIfNonNull
    , callocBytes
    , callocBoxedBytes
    , callocMem
    , allocBytes
    , allocMem
    , freeMem
    , ptr_to_g_free
    , memcpy
    , safeFreeFunPtr
    , safeFreeFunPtrPtr
    , safeFreeFunPtrPtr'
    , maybeReleaseFunPtr
    , checkUnexpectedReturnNULL
    , checkUnexpectedNothing
    , dbgLog
    ) where

#include <glib-object.h>

import Control.Exception (throwIO)
import Control.Monad (void)

import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Word

#if !MIN_VERSION_base(4,13,0)
import Foreign (peek)
#endif
import Foreign.C.Types (CSize(..), CChar)
import Foreign.Ptr (Ptr, nullPtr, FunPtr, nullFunPtr, freeHaskellFunPtr)
import Foreign.Storable (Storable(..))

import Data.GI.Base.BasicTypes (GType(..), CGType, GBoxed,
                                TypedObject(glibType),
                                UnexpectedNullPointerReturn(..))
import Data.GI.Base.CallStack (HasCallStack, callStack, prettyCallStack)

-- | When the given value is of "Just a" form, execute the given action,
-- otherwise do nothing.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = return ()

-- | Like `Control.Monad.maybe`, but for actions on a monad, and with
-- slightly different argument order.
maybeM :: Monad m => b -> Maybe a -> (a -> m b) -> m b
maybeM d Nothing _ = return d
maybeM _ (Just v) action = action v

-- | Check if the pointer is `nullPtr`, and wrap it on a `Maybe`
-- accordingly.
maybeFromPtr :: Ptr a -> Maybe (Ptr a)
maybeFromPtr ptr = if ptr == nullPtr
                   then Nothing
                   else Just ptr

-- | Given a function and a list of two-tuples, apply the function to
-- every first element of the tuples.
mapFirst :: (a -> c) -> [(a,b)] -> [(c,b)]
mapFirst _ [] = []
mapFirst f ((x,y) : rest) = (f x, y) : mapFirst f rest

-- | Same for the second element.
mapSecond :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSecond _ [] = []
mapSecond f ((x,y) : rest) = (x, f y) : mapSecond f rest

-- | Applicative version of `mapFirst`.
mapFirstA :: Applicative f => (a -> f c) -> [(a,b)] -> f [(c,b)]
mapFirstA _ [] = pure []
mapFirstA f ((x,y) : rest) = (:) <$> ((,y) <$> f x) <*> mapFirstA f rest

-- | Applicative version of `mapSecond`.
mapSecondA :: Applicative f => (b -> f c) -> [(a,b)] -> f [(a,c)]
mapSecondA _ [] = pure []
mapSecondA f ((x,y) : rest) = (:) <$> ((x,) <$> f y) <*> mapSecondA f rest

-- | Apply the given conversion action to the given pointer if it is
-- non-NULL, otherwise return `Nothing`.
convertIfNonNull :: Ptr a -> (Ptr a -> IO b) -> IO (Maybe b)
convertIfNonNull ptr convert = if ptr == nullPtr
                               then return Nothing
                               else Just <$> convert ptr

-- | Apply the given conversion action to the given function pointer
-- if it is non-NULL, otherwise return `Nothing`.
convertFunPtrIfNonNull :: FunPtr a -> (FunPtr a -> IO b) -> IO (Maybe b)
convertFunPtrIfNonNull ptr convert = if ptr == nullFunPtr
                                     then return Nothing
                                     else Just <$> convert ptr

foreign import ccall "g_malloc0" g_malloc0 ::
    #{type gsize} -> IO (Ptr a)

-- | Make a zero-filled allocation using the GLib allocator.
{-# INLINE callocBytes #-}
callocBytes :: Int -> IO (Ptr a)
callocBytes n =  g_malloc0 (fromIntegral n)

-- | Make a zero-filled allocation of enough size to hold the given
-- `Storable` type, using the GLib allocator.
{-# INLINE callocMem #-}
callocMem :: forall a. Storable a => IO (Ptr a)
callocMem = g_malloc0 $ (fromIntegral . sizeOf) (undefined :: a)

foreign import ccall "g_boxed_copy" g_boxed_copy ::
    CGType -> Ptr a -> IO (Ptr a)

-- | Make a zero filled allocation of n bytes for a boxed object. The
-- difference with a normal callocBytes is that the returned memory is
-- allocated using whatever memory allocator g_boxed_copy uses, which
-- in particular may well be different from a plain g_malloc. In
-- particular g_slice_alloc is often used for allocating boxed
-- objects, which are then freed using g_slice_free.
callocBoxedBytes :: forall a. GBoxed a => Int -> IO (Ptr a)
callocBoxedBytes n = do
  ptr <- callocBytes n
  GType cgtype <- glibType @a
  result <- g_boxed_copy cgtype ptr
  freeMem ptr
  return result

foreign import ccall "g_malloc" g_malloc ::
    #{type gsize} -> IO (Ptr a)

-- | Allocate the given number of bytes using the GLib allocator.
{-# INLINE allocBytes #-}
allocBytes :: Integral a => a -> IO (Ptr b)
allocBytes n = g_malloc (fromIntegral n)

-- | Allocate space for the given `Storable` using the GLib allocator.
{-# INLINE allocMem #-}
allocMem :: forall a. Storable a => IO (Ptr a)
allocMem = g_malloc $ (fromIntegral . sizeOf) (undefined :: a)

-- | A wrapper for `g_free`.
foreign import ccall "g_free" freeMem :: Ptr a -> IO ()

-- | Pointer to `g_free`.
foreign import ccall "&g_free" ptr_to_g_free :: FunPtr (Ptr a -> IO ())

foreign import ccall unsafe "string.h memcpy" _memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr ())

-- | Copy memory into a destination (in the first argument) from a
-- source (in the second argument).
{-# INLINE memcpy #-}
memcpy :: Ptr a -> Ptr b -> Int -> IO ()
memcpy dest src n = void $ _memcpy dest src (fromIntegral n)

-- | Same as freeHaskellFunPtr, but it does nothing when given a
-- nullPtr.
foreign import ccall "safeFreeFunPtr" safeFreeFunPtr ::
    Ptr a -> IO ()

-- | A pointer to `safeFreeFunPtr`.
foreign import ccall "& safeFreeFunPtr" safeFreeFunPtrPtr ::
    FunPtr (Ptr a -> IO ())

-- | Similar to 'safeFreeFunPtrPtr', but accepts an additional
-- (ignored) argument. The first argument is interpreted as a
-- 'FunPtr', and released.
foreign import ccall "& safeFreeFunPtr2" safeFreeFunPtrPtr' ::
    FunPtr (Ptr a -> Ptr b -> IO ())

-- | If given a pointer to the memory location, free the `FunPtr` at
-- that location, and then the pointer itself. Useful for freeing the
-- memory associated to callbacks which are called just once, with no
-- destroy notification.
maybeReleaseFunPtr :: Maybe (Ptr (FunPtr a)) -> IO ()
maybeReleaseFunPtr Nothing = return ()
maybeReleaseFunPtr (Just f) = do
  peek f >>= freeHaskellFunPtr
  freeMem f

-- | Check that the given pointer is not NULL. If it is, raise a
-- `UnexpectedNullPointerReturn` exception.
checkUnexpectedReturnNULL :: HasCallStack => T.Text -> Ptr a -> IO ()
checkUnexpectedReturnNULL fnName ptr
    | ptr == nullPtr =
        throwIO (UnexpectedNullPointerReturn {
                   nullPtrErrorMsg = "Received unexpected nullPtr in \""
                                     <> fnName <> "\".\n" <>
                                     "This might be a bug in the introspection data, or perhaps a use-after-free bug.\n" <>
                                     "If in doubt, please report it at\n\thttps://github.com/haskell-gi/haskell-gi/issues\n" <>
                                     T.pack (prettyCallStack callStack)
                 })
    | otherwise = return ()

-- | An annotated version of `fromJust`, which raises a
-- `UnexpectedNullPointerReturn` in case it encounters a `Nothing`.
checkUnexpectedNothing :: HasCallStack => T.Text -> IO (Maybe a) -> IO a
checkUnexpectedNothing fnName action = do
  result <- action
  case result of
    Just r -> return r
    Nothing -> throwIO (UnexpectedNullPointerReturn {
                 nullPtrErrorMsg = "Received unexpected Nothing in \""
                                     <> fnName <> "\".\n" <>
                                     "This might be a bug in the introspection data, or perhaps a use-after-free bug.\n" <>
                                     "If in doubt, please report it at\n\thttps://github.com/haskell-gi/haskell-gi/issues\n" <>
                                     T.pack (prettyCallStack callStack)
                 })

foreign import ccall unsafe "dbg_log_with_len" dbg_log_with_len ::
        Ptr CChar -> Int -> IO ()

-- | Print a string to the debug log in an atomic way (so the output
-- of different threads does not get intermingled).
dbgLog :: T.Text -> IO ()
dbgLog msg = TF.withCStringLen msg $ \(ptr, len) -> dbg_log_with_len ptr len
