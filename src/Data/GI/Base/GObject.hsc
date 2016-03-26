{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

module Data.GI.Base.GObject
    ( constructGObject
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))
import Foreign.C (CUInt(..), CString, newCString)
import Foreign

import Data.GI.Base.Attributes (AttrOp(..), AttrOpTag(..), AttrLabelProxy,
                                attrConstruct)
import Data.GI.Base.BasicTypes (GType(..), GObject(..))
import Data.GI.Base.GValue (GValue(..))
import Data.GI.Base.ManagedPtr (withManagedPtr, touchManagedPtr, wrapObject)
import Data.GI.Base.Overloading (ResolveAttribute)

#include <glib-object.h>

foreign import ccall "dbg_g_object_newv" g_object_newv ::
    GType -> CUInt -> Ptr a -> IO (Ptr b)

-- | Construct a GObject given the constructor and a list of settable
-- attributes.
constructGObject :: forall o m. (GObject o, MonadIO m)
    => (ForeignPtr o -> o)
    -> [AttrOp o 'AttrConstruct]
    -> m o
constructGObject constructor attrs = liftIO $ do
  props <- mapM construct attrs
  let nprops = length props
  params <- mallocBytes (nprops*gparameterSize)
  fill params props
  gtype <- gobjectType (undefined :: o)
  result <- g_object_newv gtype (fromIntegral nprops) params
  freeStrings nprops params
  free params
  -- Make sure that the GValues defining the GProperties are still
  -- alive at this point (so, in particular, they are still alive when
  -- g_object_newv is called). Without this the GHC garbage collector
  -- may free the GValues before g_object_newv us called, which will
  -- unref the referred to objects, which may drop the last reference
  -- to the contained objects. g_object_newv then tries to access the
  -- (now invalid) contents of the GValue, and mayhem ensues.
  mapM_ (touchManagedPtr . snd) props
  wrapObject constructor (result :: Ptr o)
  where
    resolve :: AttrLabelProxy attr -> Proxy (ResolveAttribute attr o)
    resolve _ = Proxy

    construct :: AttrOp o 'AttrConstruct ->
                 IO (String, GValue)
    construct (attr := x) = attrConstruct (resolve attr) x
    construct (attr :=> x) = x >>= attrConstruct (resolve attr)

    gvalueSize = #size GValue
    gparameterSize = #size GParameter

    -- Fill the given memory address with the contents of the array of
    -- GParameters.
    fill :: Ptr () -> [(String, GValue)] -> IO ()
    fill _ [] = return ()
    fill dataPtr ((str, gvalue):xs) =
        do cstr <- newCString str
           poke (castPtr dataPtr) cstr
           withManagedPtr gvalue $ \gvalueptr ->
               copyBytes (dataPtr `plusPtr` sizeOf nullPtr) gvalueptr gvalueSize
           fill (dataPtr `plusPtr` gparameterSize) xs

    -- Free the strings in the GParameter array (the GValues will be
    -- freed separately).
    freeStrings :: Int -> Ptr () -> IO ()
    freeStrings 0 _ = return ()
    freeStrings n dataPtr =
        do cstr <- peek (castPtr dataPtr) :: IO CString
           free cstr
           freeStrings (n-1) (dataPtr `plusPtr` gparameterSize)
