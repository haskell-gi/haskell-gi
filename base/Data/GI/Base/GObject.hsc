{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, TypeApplications #-}

-- | This module constains helpers for dealing with `GObject`-derived
-- types.

module Data.GI.Base.GObject
    ( -- * Constructing new `GObject`s
      constructGObject

    -- * Deriving new object types
    , DerivedGObject(..)
    , registerGType
    , gobjectGetPrivateData
    , gobjectSetPrivateData

    , GObjectClass
    , gtypeFromClass
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))

import Foreign.C (CUInt(..), CString, newCString)
import Foreign.Ptr (FunPtr, castPtr)
import Foreign.StablePtr (newStablePtr, castStablePtrToPtr)
import Foreign

import Data.Text (Text)

import Data.GI.Base.Attributes (AttrOp(..), AttrOpTag(..), AttrLabelProxy,
                                attrConstruct)
import Data.GI.Base.BasicTypes (CGType, GType(..), GObject(..), ManagedPtr)
import Data.GI.Base.BasicConversions (withTextCString)
import Data.GI.Base.CallStack (HasCallStack)
import Data.GI.Base.GValue (GValue(..), GValueConstruct(..))
import Data.GI.Base.ManagedPtr (withManagedPtr, touchManagedPtr, wrapObject,
                                newObject)
import Data.GI.Base.Overloading (ResolveAttribute)

#include <glib-object.h>

foreign import ccall "dbg_g_object_new" g_object_new ::
    GType -> CUInt -> Ptr CString -> Ptr a -> IO (Ptr b)

-- | Construct a GObject given the constructor and a list of settable
-- attributes. See `Data.GI.Base.Constructible.new` for a more general
-- version.
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
  gtype <- gobjectType @o
  result <- g_object_new gtype (fromIntegral nprops) names values
  freeStrings nprops names
  free values
  free names
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

-- | Opaque wrapper around @GObjectClass@ on the C-side.
newtype GObjectClass = GObjectClass (Ptr GObjectClass)

-- | This typeclass contains the data necessary for defining a new
-- `GObject` type.
class GObject a => DerivedGObject a where
  -- | The parent type
  type GObjectParentType a
  -- | Type of the private data for each instance.
  type GObjectPrivateData a

  -- | Name of the type, it should be unique.
  objectTypeName     :: Text
  -- | Code to run when the class is inited. This is a good place to
  -- register signals and properties for the type.
  objectClassInit    :: GObjectClass -> IO ()
  -- | Code to run when each instance of the type is
  -- constructed. Returns the private data to be associated with the
  -- new instance (use `gobjectGetPrivateData` and
  -- `gobjectSetPrivateData` to manipulate this further).
  objectInstanceInit :: GObjectClass -> a -> IO (GObjectPrivateData a)

type CGTypeClassInit = GObjectClass -> IO ()
foreign import ccall "wrapper"
        mkClassInit :: CGTypeClassInit -> IO (FunPtr CGTypeClassInit)

type CGTypeInstanceInit = GObjectClass -> Ptr () -> IO ()
foreign import ccall "wrapper"
        mkInstanceInit :: CGTypeInstanceInit -> IO (FunPtr CGTypeInstanceInit)

foreign import ccall "g_type_from_name" g_type_from_name ::
        CString -> IO CGType

foreign import ccall "haskell_gi_register_gtype" register_gtype ::
        CGType -> CString -> FunPtr CGTypeClassInit ->
        FunPtr CGTypeInstanceInit -> IO CGType

foreign import ccall unsafe "haskell_gi_gtype_from_class" gtype_from_class ::
        GObjectClass -> IO CGType

-- | Find the `GType` associated to a given `GObjectClass`.
gtypeFromClass :: GObjectClass -> IO GType
gtypeFromClass klass = GType <$> gtype_from_class klass

-- | Register the given type into the @GObject@ type system and return
-- the resulting `GType`, if it has not been registered already. If
-- the type has been registered already the existing `GType` will be
-- returned instead.
--
-- Note that for this function to work the type must be an instance of
-- `DerivedGObject`.
registerGType :: forall o. (HasCallStack, DerivedGObject o, GObject (GObjectParentType o)) =>
                 (ManagedPtr o -> o) -> IO GType
registerGType cons = withTextCString (objectTypeName @o) $ \cTypeName -> do
  cgtype <- g_type_from_name cTypeName
  if cgtype /= 0
    then return (GType cgtype)  -- Already registered
    else do
      classInit <- mkClassInit (objectClassInit @o)
      instanceInit <- mkInstanceInit (unwrapInstanceInit $ objectInstanceInit @o)
      (GType parentCGType) <- gobjectType @(GObjectParentType o)
      GType <$> register_gtype parentCGType cTypeName classInit instanceInit

   where
     unwrapInstanceInit :: (GObjectClass -> o -> IO (GObjectPrivateData o)) ->
                           CGTypeInstanceInit
     unwrapInstanceInit instanceInit klass objPtr = do
       obj <- newObject @_ @o cons (castPtr objPtr)
       privateData <- instanceInit klass obj
       gobjectSetPrivateData obj privateData
       return ()

-- | Get the private data associated with the given object.
gobjectGetPrivateData :: DerivedGObject o => o -> IO (GObjectPrivateData o)
gobjectGetPrivateData = undefined

-- | Set the private data associated with the given object.
gobjectSetPrivateData :: DerivedGObject o => o -> GObjectPrivateData o -> IO ()
gobjectSetPrivateData = undefined
