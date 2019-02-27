{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

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

    -- * Installing properties for derived objects
    , gobjectInstallProperty
    , gobjectInstallCIntProperty
    , gobjectInstallCStringProperty
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))

import Foreign.C (CUInt(..), CString, newCString)
import Foreign.Ptr (FunPtr, castPtr)
import Foreign.StablePtr (newStablePtr, deRefStablePtr,
                          castStablePtrToPtr, castPtrToStablePtr)
import Foreign

import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.Base.Attributes (AttrOp(..), AttrOpTag(..), AttrLabelProxy,
                                attrConstruct)
import Data.GI.Base.BasicTypes (CGType, GType(..), GObject(..),
                                GDestroyNotify, ManagedPtr, GParamSpec(..),
                                gtypeName)
import Data.GI.Base.BasicConversions (withTextCString, cstringToText)
import Data.GI.Base.CallStack (HasCallStack)
import Data.GI.Base.GParamSpec (PropertyInfo(..),
                                gParamSpecValue,
                                CIntPropertyInfo(..), CStringPropertyInfo(..),
                                gParamSpecCInt, gParamSpecCString,
                                getGParamSpecGetterSetter,
                                PropGetSetter(..))
import Data.GI.Base.GValue (GValue(..), GValueConstruct(..))
import Data.GI.Base.ManagedPtr (withManagedPtr, touchManagedPtr, wrapObject,
                                newObject)
import Data.GI.Base.Overloading (ResolveAttribute)
import Data.GI.Base.Utils (dbgLog)

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
-- `GObject` type from Haskell.
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

foreign import ccall g_type_from_name :: CString -> IO CGType

foreign import ccall "haskell_gi_register_gtype" register_gtype ::
        CGType -> CString -> FunPtr CGTypeClassInit ->
        FunPtr CGTypeInstanceInit -> IO CGType

foreign import ccall "haskell_gi_gtype_from_class" gtype_from_class ::
        GObjectClass -> IO CGType

-- | Find the `GType` associated to a given `GObjectClass`.
gtypeFromClass :: GObjectClass -> IO GType
gtypeFromClass klass = GType <$> gtype_from_class klass

foreign import ccall g_param_spec_get_name ::
   Ptr GParamSpec -> IO CString

type CPropertyGetter o = Ptr o -> CUInt -> Ptr GValue -> Ptr GParamSpec -> IO ()

foreign import ccall "wrapper"
        mkPropertyGetter :: CPropertyGetter o -> IO (FunPtr (CPropertyGetter o))

type CPropertySetter o = Ptr o -> CUInt -> Ptr GValue -> Ptr GParamSpec -> IO ()

foreign import ccall "wrapper"
        mkPropertySetter :: CPropertySetter o -> IO (FunPtr (CPropertySetter o))

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
      classInit <- mkClassInit (unwrapClassInit $ objectClassInit @o)
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

     unwrapClassInit :: (GObjectClass -> IO ()) -> CGTypeClassInit
     unwrapClassInit classInit klass@(GObjectClass klassPtr) = do
       getFunPtr <- mkPropertyGetter marshallGetter
       (#poke GObjectClass, get_property) klassPtr getFunPtr
       setFunPtr <- mkPropertySetter marshallSetter
       (#poke GObjectClass, set_property) klassPtr setFunPtr
       classInit klass

     marshallSetter :: CPropertySetter o
     marshallSetter objPtr _ gvPtr pspecPtr = do
       maybeGetSet <- getGParamSpecGetterSetter pspecPtr
       case maybeGetSet of
         Nothing -> do
           pspecName <- g_param_spec_get_name pspecPtr >>= cstringToText
           typeName <- gobjectType @o >>= gtypeName
           dbgLog $ "WARNING: Attempting to set unknown property \""
                    <> pspecName <> "\" of type \"" <> T.pack typeName <> "\"."
         Just pgs -> (propSetter pgs) objPtr gvPtr

     marshallGetter :: CPropertyGetter o
     marshallGetter objPtr _ destGValuePtr pspecPtr = do
       maybeGetSet <- getGParamSpecGetterSetter pspecPtr
       case maybeGetSet of
         Nothing -> do
           pspecName <- g_param_spec_get_name pspecPtr >>= cstringToText
           typeName <- gobjectType @o >>= gtypeName
           dbgLog $ "WARNING: Attempting to get unknown property \""
                    <> pspecName <> "\" of type \"" <> T.pack typeName <> "\"."
         Just pgs -> (propGetter pgs) objPtr destGValuePtr

-- | Name of the private key for the given type.
privateKey :: forall o. DerivedGObject o => Text
privateKey = objectTypeName @o <> "::haskell-gi-private-data"

foreign import ccall g_object_get_data ::
   Ptr a -> CString -> IO (Ptr b)

-- | Get the value (which should be a `StablePtr` to a Haskell object
-- of the right type) of a given key for the object.
gobjectGetUserData :: (HasCallStack, GObject o) => o -> Text -> IO (Maybe a)
gobjectGetUserData obj key = do
  dataPtr <- withTextCString key $ \ckey ->
               withManagedPtr obj $ \objPtr ->
                 g_object_get_data objPtr ckey
  if dataPtr /= nullPtr
    then Just <$> deRefStablePtr (castPtrToStablePtr dataPtr)
    else return Nothing

-- | Get the private data associated with the given object.
gobjectGetPrivateData :: forall o. (HasCallStack, DerivedGObject o) =>
                         o -> IO (GObjectPrivateData o)
gobjectGetPrivateData obj = do
  maybePrivate <- gobjectGetUserData obj (privateKey @o)
  case maybePrivate of
    Just private -> return private
    Nothing -> error "Could not find private data for the given GObject!"

foreign import ccall "&hs_free_stable_ptr" ptr_to_hs_free_stable_ptr ::
        FunPtr (GDestroyNotify a)

foreign import ccall g_object_set_data_full ::
        Ptr a -> CString -> Ptr () -> FunPtr (GDestroyNotify ()) -> IO ()

-- | Set the value of the user data for the given `GObject` to a
-- `StablePtr` to the given Haskell object. The `StablePtr` will be
-- freed when the object is destroyed, or the value of the key is
-- replaced.
gobjectSetUserData :: (HasCallStack, GObject o) => o -> Text -> a -> IO ()
gobjectSetUserData obj key value = do
  stablePtr <- newStablePtr value
  withTextCString key $ \ckey ->
    withManagedPtr obj $ \objPtr ->
      g_object_set_data_full objPtr ckey (castStablePtrToPtr stablePtr)
                             ptr_to_hs_free_stable_ptr

-- | Set the private data associated with the given object.
gobjectSetPrivateData :: forall o. DerivedGObject o =>
                         o -> GObjectPrivateData o -> IO ()
gobjectSetPrivateData obj value =
  gobjectSetUserData obj (privateKey @o) value

foreign import ccall g_object_class_install_property ::
   GObjectClass -> CUInt -> Ptr GParamSpec -> IO ()

-- | Add a Haskell object-valued property to the given object class.
gobjectInstallProperty :: DerivedGObject o =>
                            GObjectClass -> PropertyInfo o a -> IO ()
gobjectInstallProperty klass propInfo = do
  pspec <- gParamSpecValue propInfo
  withManagedPtr pspec $ \pspecPtr ->
    g_object_class_install_property klass 1 pspecPtr

-- | Add a `Foreign.C.CInt`-valued property to the given object class.
gobjectInstallCIntProperty :: DerivedGObject o =>
                              GObjectClass -> CIntPropertyInfo o -> IO ()
gobjectInstallCIntProperty klass propInfo = do
  pspec <- gParamSpecCInt propInfo
  withManagedPtr pspec $ \pspecPtr ->
    g_object_class_install_property klass 1 pspecPtr

-- | Add a `CString`-valued property to the given object class.
gobjectInstallCStringProperty :: DerivedGObject o =>
                              GObjectClass -> CStringPropertyInfo o -> IO ()
gobjectInstallCStringProperty klass propInfo = do
  pspec <- gParamSpecCString propInfo
  withManagedPtr pspec $ \pspecPtr ->
    g_object_class_install_property klass 1 pspecPtr
