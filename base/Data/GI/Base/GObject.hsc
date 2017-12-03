{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies #-}

module Data.GI.Base.GObject
    ( constructGObject
    , new'
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))

import Foreign.C (CUInt(..), CString, newCString)
import Foreign

import Data.GI.Base.Attributes (AttrOp(..), AttrOpTag(..), AttrLabelProxy,
                                attrConstruct)
import Data.GI.Base.BasicTypes (GType(..), GObject(..), ManagedPtr)
import Data.GI.Base.GValue (GValue(..), GValueConstruct(..))
import Data.GI.Base.ManagedPtr (withManagedPtr, touchManagedPtr, wrapObject)
import Data.GI.Base.Overloading (ResolveAttribute)

#include <glib-object.h>

foreign import ccall "dbg_g_object_new" g_object_new ::
    GType -> CUInt -> Ptr CString -> Ptr a -> IO (Ptr b)

-- | Construct a GObject given the constructor and a list of settable
-- attributes.
constructGObject :: forall o m. (GObject o, MonadIO m)
    => (ManagedPtr o -> o)
    -> [AttrOp o 'AttrConstruct]
    -> m o
constructGObject constructor attrs = liftIO $ do
  props <- mapM construct attrs
  doConstructGObject constructor props
  where
    resolve :: AttrLabelProxy attr -> Proxy (ResolveAttribute attr o)
    resolve _ = Proxy

    construct :: AttrOp o 'AttrConstruct ->
                 IO (GValueConstruct o)
    construct (attr := x) = attrConstruct (resolve attr) x
    construct (attr :=> x) = x >>= attrConstruct (resolve attr)

-- | Construct the `GObject` given the list of `GValueConstruct`s.
doConstructGObject :: forall o m. (GObject o, MonadIO m)
                      => (ManagedPtr o -> o) -> [GValueConstruct o] -> m o
doConstructGObject constructor props = liftIO $ do
  let nprops = length props
  names <- mallocBytes (nprops * sizeOf nullPtr)
  values <- mallocBytes (nprops * gvalueSize)
  fill names values props
  gtype <- gobjectType (undefined :: o)
  result <- g_object_new gtype (fromIntegral nprops) names values
  freeStrings nprops names
  free values
  -- Make sure that the GValues defining the GProperties are still
  -- alive at this point (so, in particular, they are still alive when
  -- g_object_new is called). Without this the GHC garbage collector
  -- may free the GValues before g_object_new is called, which will
  -- unref the referred to objects, which may drop the last reference
  -- to the contained objects. g_object_new then tries to access the
  -- (now invalid) contents of the GValue, and mayhem ensues.
  mapM_ (touchManagedPtr . deconstructGValue) props
  wrapObject constructor (result :: Ptr o)

  where
    deconstructGValue :: GValueConstruct o -> GValue
    deconstructGValue (GValueConstruct _ v) = v

    gvalueSize = #size GValue

    -- Fill in the memory associated with the parameters.
    fill :: Ptr CString -> Ptr GValue -> [GValueConstruct o] -> IO ()
    fill _ _ [] = return ()
    fill namePtr dataPtr ((GValueConstruct str gvalue):xs) =
        do cstr <- newCString str
           poke namePtr cstr
           withManagedPtr gvalue $ \gvalueptr ->
                                     copyBytes dataPtr gvalueptr gvalueSize
           fill (namePtr `plusPtr` sizeOf nullPtr)
                (dataPtr `plusPtr` gvalueSize) xs

    -- Free the strings in the GParameter array (the GValues will be
    -- freed separately).
    freeStrings :: Int -> Ptr CString -> IO ()
    freeStrings 0 _ = return ()
    freeStrings n namePtr =
        do peek namePtr >>= free
           freeStrings (n-1) (namePtr `plusPtr` sizeOf nullPtr)

-- | Construct the given `GObject`, given a set of actions
-- constructing desired `GValue`s to set at construction time.
new' :: (MonadIO m, GObject o) =>
        (ManagedPtr o -> o) -> [IO (GValueConstruct o)] -> m o
new' constructor actions = do
  props <- liftIO $ sequence (actions)
  doConstructGObject constructor props
