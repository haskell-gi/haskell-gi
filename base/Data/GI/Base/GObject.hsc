{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module constains helpers for dealing with `GObject`-derived
-- types.

module Data.GI.Base.GObject
    ( -- * Constructing new `GObject`s
      constructGObject
    , new'

    -- * User data
    , gobjectGetUserData
    , gobjectSetUserData
    , gobjectModifyUserData

    -- * Deriving new object types
    , DerivedGObject(..)
    , registerGType
    , gobjectGetPrivateData
    , gobjectSetPrivateData
    , gobjectModifyPrivateData

    , GObjectClass(..)
    , gtypeFromClass
    , gtypeFromInstance

    -- * Installing properties for derived objects
    , gobjectInstallProperty
    , gobjectInstallCIntProperty
    , gobjectInstallCStringProperty
    , gobjectInstallGBooleanProperty
    ) where

import Data.Maybe (catMaybes)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Proxy (Proxy(..))
import Data.Coerce (coerce)

import Foreign.C (CUInt(..), CString, newCString)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr, plusPtr, nullFunPtr)
import Foreign.StablePtr (newStablePtr, deRefStablePtr,
                          castStablePtrToPtr, castPtrToStablePtr)
import Foreign.Storable (Storable(peek, poke, pokeByteOff, sizeOf))
import Foreign (mallocBytes, copyBytes, free)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T

import qualified Optics.Core as O

import Data.GI.Base.Attributes (AttrOp(..), AttrOpTag(..), AttrLabelProxy,
                                attrConstruct, attrTransfer,
                                AttrInfo(..), EqMaybe(..), bindPropToField)
import Data.GI.Base.BasicTypes (CGType, GType(..), GObject, GSList,
                                GDestroyNotify, ManagedPtr(..), GParamSpec(..),
                                TypedObject(glibType),
                                gtypeName, g_slist_free)
import Data.GI.Base.BasicConversions (withTextCString, cstringToText,
                                      packGSList, mapGSList)
import Data.GI.Base.CallStack (HasCallStack, prettyCallStack)
import Data.GI.Base.DynVal (modelProxyCurrentValue, modelProxyRegisterHandler,
                            dvKeys, dvRead)
import Data.GI.Base.GParamSpec (PropertyInfo(..),
                                gParamSpecValue,
                                CIntPropertyInfo(..), CStringPropertyInfo(..),
                                GBooleanPropertyInfo(..),
                                gParamSpecCInt, gParamSpecCString, gParamSpecGBoolean,
                                getGParamSpecGetterSetter,
                                PropGetSetter(..))
import Data.GI.Base.GQuark (GQuark(..), gQuarkFromString)
import Data.GI.Base.GValue (GValue(..), GValueConstruct(..))
import Data.GI.Base.ManagedPtr (withManagedPtr, touchManagedPtr, wrapObject,
                                newObject)
import Data.GI.Base.Overloading (ResolveAttribute)
import Data.GI.Base.Signals (on, after)
import Data.GI.Base.Utils (dbgLog, callocBytes, freeMem)
import Data.GI.Base.Internal.PathFieldAccess (Components, PathFieldAccess(..))

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
  props <- catMaybes <$> mapM construct attrs
  obj <- doConstructGObject constructor props
  mapM_ (setSignal obj) attrs
  mapM_ (registerHandlers obj) attrs
  return obj
  where
    construct :: AttrOp o 'AttrConstruct ->
                 IO (Maybe (GValueConstruct o))
    construct ((_attr :: AttrLabelProxy label) := x) =
      Just <$> attrConstruct @(ResolveAttribute label o) x

    construct ((_attr :: AttrLabelProxy label) :=> x) =
      Just <$> (x >>= attrConstruct @(ResolveAttribute label o))

    construct ((_attr :: AttrLabelProxy label) :&= x) = Just <$>
      (attrTransfer @(ResolveAttribute label o) (Proxy @o) x >>=
       attrConstruct @(ResolveAttribute label o))

    construct ((_attr :: AttrLabelProxy label) :!<~ dv) = do
      model <- modelProxyCurrentValue ?_haskell_gi_modelProxy
      Just <$> attrConstruct @(ResolveAttribute label o) (dvRead dv model)

    construct (Bind (_pattr :: AttrLabelProxy prop)
                    (_fattr :: AttrLabelProxy field)) = do
      model <- modelProxyCurrentValue ?_haskell_gi_modelProxy
      let lens = getLens (Proxy @field) model
          value = O.view lens model
      Just <$> attrConstruct @(ResolveAttribute prop o) value
        where getLens :: forall path components model value.
                         (components ~ Components path,
                          PathFieldAccess components model value) =>
                         Proxy path -> model -> O.Lens' model value
              getLens _ _ = let
                (lens, _) = pathFieldAccess (Proxy @components) (Proxy @model)
                in lens

    -- Since we are constructing the object there's nothing to compare
    -- to, so we just set the value.
    construct ((_attr :: AttrLabelProxy label) :<~ dv) = do
      model <- modelProxyCurrentValue ?_haskell_gi_modelProxy
      Just <$> attrConstruct @(ResolveAttribute label o) (dvRead dv model)

    construct (On _ _) = return Nothing
    construct (After _ _) = return Nothing

    setSignal :: GObject o => o -> AttrOp o 'AttrConstruct -> IO ()
    setSignal obj (On signal callback) = void $ on obj signal callback
    setSignal obj (After signal callback) = void $ after obj signal callback
    setSignal _ _ = return ()

    registerHandlers :: GObject o => o -> AttrOp o 'AttrConstruct -> IO ()
    registerHandlers obj ((_attr :: AttrLabelProxy label) :!<~ dv) =
      modelProxyRegisterHandler ?_haskell_gi_modelProxy (dvKeys dv) $ \modifiedModel ->
        attrSet @(ResolveAttribute label o) obj (dvRead dv modifiedModel)

    registerHandlers obj ((_attr :: AttrLabelProxy label) :<~ dv) =
      modelProxyRegisterHandler ?_haskell_gi_modelProxy (dvKeys dv) $ \modifiedModel -> do
      current <- attrGet @(ResolveAttribute label o) obj
      let modifiedValue = dvRead dv modifiedModel
      when (not $ eqMaybe modifiedValue current) $
         attrSet @(ResolveAttribute label o) obj modifiedValue

    registerHandlers obj (Bind pattr fattr) =
      bindPropToField (Proxy @'AttrConstruct) obj pattr fattr

    registerHandlers _ _ = pure ()

-- | Construct the given `GObject`, given a set of actions
-- constructing desired `GValue`s to set at construction time.
new' :: (HasCallStack, MonadIO m, GObject o) =>
        (ManagedPtr o -> o) -> [m (GValueConstruct o)] -> m o
new' constructor actions = do
  props <- sequence actions
  doConstructGObject constructor props

-- | Construct the `GObject` given the list of `GValueConstruct`s.
doConstructGObject :: forall o m. (HasCallStack, GObject o, MonadIO m)
                      => (ManagedPtr o -> o) -> [GValueConstruct o] -> m o
doConstructGObject constructor props = liftIO $ do
  let nprops = length props
  names <- mallocBytes (nprops * sizeOf nullPtr)
  values <- mallocBytes (nprops * gvalueSize)
  fill names values props
  gtype <- glibType @o
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

-- | Wrapper around @GObjectClass@ on the C-side.
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

  -- | List of interfaces implemented by the type. Each element is a
  -- triplet (@gtype@, @interfaceInit@, @interfaceFinalize@), where
  -- @gtype :: IO GType@ is a constructor for the type of the
  -- interface, @interfaceInit :: Ptr () -> IO ()@ is a function that
  -- registers the callbacks in the interface, and @interfaceFinalize
  -- :: Maybe (Ptr () -> IO ())@ is the (optional) finalizer.
  objectInterfaces :: [(IO GType, Ptr () -> IO (), Maybe (Ptr () -> IO ()))]
  objectInterfaces = []

type CGTypeClassInit = GObjectClass -> IO ()
foreign import ccall "wrapper"
        mkClassInit :: CGTypeClassInit -> IO (FunPtr CGTypeClassInit)

type CGTypeInstanceInit o = Ptr o -> GObjectClass -> IO ()
foreign import ccall "wrapper"
        mkInstanceInit :: CGTypeInstanceInit o -> IO (FunPtr (CGTypeInstanceInit o))

type CGTypeInterfaceInit = Ptr () -> Ptr () -> IO ()
foreign import ccall "wrapper"
        mkInterfaceInit :: CGTypeInterfaceInit -> IO (FunPtr CGTypeInterfaceInit)

type CGTypeInterfaceFinalize = Ptr () -> Ptr () -> IO ()
foreign import ccall "wrapper"
        mkInterfaceFinalize :: CGTypeInterfaceFinalize -> IO (FunPtr CGTypeInterfaceFinalize)

foreign import ccall g_type_from_name :: CString -> IO CGType

foreign import ccall "haskell_gi_register_gtype" register_gtype ::
        CGType -> CString -> FunPtr CGTypeClassInit ->
        FunPtr (CGTypeInstanceInit o) -> Ptr (GSList a) -> IO CGType

foreign import ccall "haskell_gi_gtype_from_class" gtype_from_class ::
        GObjectClass -> IO CGType

-- | Find the `GType` associated to a given `GObjectClass`.
gtypeFromClass :: GObjectClass -> IO GType
gtypeFromClass klass = GType <$> gtype_from_class klass

foreign import ccall "haskell_gi_gtype_from_instance" gtype_from_instance ::
        Ptr o -> IO CGType

-- | Find the `GType` for a given `GObject`.
gtypeFromInstance :: GObject o => o -> IO GType
gtypeFromInstance obj = withManagedPtr obj $ \objPtr ->
                            (GType <$> gtype_from_instance objPtr)

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
registerGType :: forall o. (HasCallStack, DerivedGObject o,
                            GObject (GObjectParentType o),
                            GObject o) =>
                 (ManagedPtr o -> o) -> IO GType
registerGType construct = withTextCString (objectTypeName @o) $ \cTypeName -> do
  cgtype <- g_type_from_name cTypeName
  if cgtype /= 0
    then return (GType cgtype)  -- Already registered
    else do
      classInit <- mkClassInit (unwrapClassInit $ objectClassInit @o)
      instanceInit <- mkInstanceInit (unwrapInstanceInit $ objectInstanceInit @o)
      (GType parentCGType) <- glibType @(GObjectParentType o)
      interfaces <- mapM packInterface (objectInterfaces @o) >>= packGSList
      gtype <- GType <$> register_gtype parentCGType cTypeName classInit instanceInit interfaces
      mapGSList freeInterfaceInfo interfaces
      g_slist_free interfaces
      return gtype

   where
     unwrapInstanceInit :: (GObjectClass -> o -> IO (GObjectPrivateData o)) ->
                           CGTypeInstanceInit o
     unwrapInstanceInit instanceInit objPtr klass = do
       privateData <- do
         obj <- newObject construct (castPtr objPtr :: Ptr o)
         instanceInit klass obj
       instanceSetPrivateData objPtr privateData

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
           typeName <- glibType @o >>= gtypeName
           dbgLog $ "WARNING: Attempting to set unknown property \""
                    <> pspecName <> "\" of type \"" <> T.pack typeName <> "\"."
         Just pgs -> (propSetter pgs) objPtr gvPtr

     marshallGetter :: CPropertyGetter o
     marshallGetter objPtr _ destGValuePtr pspecPtr = do
       maybeGetSet <- getGParamSpecGetterSetter pspecPtr
       case maybeGetSet of
         Nothing -> do
           pspecName <- g_param_spec_get_name pspecPtr >>= cstringToText
           typeName <- glibType @o >>= gtypeName
           dbgLog $ "WARNING: Attempting to get unknown property \""
                    <> pspecName <> "\" of type \"" <> T.pack typeName <> "\"."
         Just pgs -> (propGetter pgs) objPtr destGValuePtr

     packInterface :: (IO GType, Ptr () -> IO (), Maybe (Ptr () -> IO ()))
                   -> IO (Ptr CGType)
     packInterface (ifaceGTypeConstruct, initHs, maybeFinalize) = do
       gtype <- ifaceGTypeConstruct
       info <- callocBytes #{size GInterfaceInfo}
       initFn <- mkInterfaceInit (unwrapInit initHs)
       finalizeFn <- case maybeFinalize of
                     Just finalizeHs -> mkInterfaceFinalize (unwrapFinalize finalizeHs)
                     Nothing -> pure nullFunPtr
       #{poke GInterfaceInfo, interface_init} info initFn
       #{poke GInterfaceInfo, interface_finalize} info finalizeFn

       combined <- callocBytes (#{size GType} + #{size gpointer})
       poke combined (gtypeToCGType gtype)
       poke (combined `plusPtr` #{size GType}) info
       return combined

     unwrapInit :: (Ptr () -> IO ()) -> CGTypeInterfaceInit
     unwrapInit f ptr _data = f ptr

     unwrapFinalize :: (Ptr () -> IO ()) -> CGTypeInterfaceFinalize
     unwrapFinalize = unwrapInit

     freeInterfaceInfo :: Ptr CGType -> IO ()
     freeInterfaceInfo combinedPtr = do
       info <- peek (combinedPtr `plusPtr` #{size GType})
       freeMem info
       freeMem combinedPtr

-- | Quark with the key to the private data for this object type.
privateKey :: forall o. DerivedGObject o => IO (GQuark (GObjectPrivateData o))
privateKey = gQuarkFromString $ objectTypeName @o <> "::haskell-gi-private-data"

-- | Get the private data associated with the given object.
gobjectGetPrivateData :: forall o. (HasCallStack, DerivedGObject o) =>
                            o -> IO (GObjectPrivateData o)
gobjectGetPrivateData obj = do
  key <- privateKey @o
  maybePriv <- gobjectGetUserData obj key
  case maybePriv of
    Just priv -> return priv
    Nothing -> do
      case managedPtrAllocCallStack (coerce obj) of
        Nothing -> error ("Failed to get private data pointer!\n"
                          <> "Set the env var HASKELL_GI_DEBUG_MEM=1 to get more info.")
        Just cs -> withManagedPtr obj $ \objPtr -> do
          let errMsg = "Failed to get private data pointer for" <> show objPtr <> "!\n"
                       <> "Callstack for allocation was:\n"
                       <> prettyCallStack cs <> "\n\n"
          error errMsg

foreign import ccall g_object_get_qdata ::
   Ptr a -> GQuark b -> IO (Ptr c)

-- | Get the value of a given key for the object.
gobjectGetUserData :: (HasCallStack, GObject o) => o -> GQuark a -> IO (Maybe a)
gobjectGetUserData obj key = do
  dataPtr <- withManagedPtr obj $ \objPtr ->
                 g_object_get_qdata objPtr key
  if dataPtr /= nullPtr
    then Just <$> deRefStablePtr (castPtrToStablePtr dataPtr)
    else return Nothing

foreign import ccall "&hs_free_stable_ptr" ptr_to_hs_free_stable_ptr ::
        GDestroyNotify (Ptr ())

foreign import ccall g_object_set_qdata_full ::
        Ptr a -> GQuark b -> Ptr () -> GDestroyNotify (Ptr ()) -> IO ()

-- | Set the value of the user data for the given `GObject` to a
-- `StablePtr` to the given Haskell object. The `StablePtr` will be
-- freed when the object is destroyed, or the value is replaced.
gobjectSetUserData :: (HasCallStack, GObject o) =>
                   o -> GQuark a -> a -> IO ()
gobjectSetUserData obj key value = withManagedPtr obj $ \objPtr ->
  instanceSetUserData objPtr key value

-- | A combination of `gobjectGetUserData` and `gobjectSetUserData`,
-- for convenience.
gobjectModifyUserData :: (HasCallStack, GObject o) =>
                   o -> GQuark a -> (Maybe a -> a) -> IO ()
gobjectModifyUserData obj key transform = do
  userData <- gobjectGetUserData obj key
  gobjectSetUserData obj key (transform userData)

-- | Like `gobjectSetUserData`, but it works on the raw object pointer.
-- Note that this is unsafe, unless used in a context where we are sure that
-- the GC will not release the object while we run.
instanceSetUserData :: (HasCallStack, GObject o) =>
                    Ptr o -> GQuark a -> a -> IO ()
instanceSetUserData objPtr key value = do
  stablePtr <- newStablePtr value
  g_object_set_qdata_full objPtr key (castStablePtrToPtr stablePtr)
                             ptr_to_hs_free_stable_ptr

-- | Set the private data associated with the given object.
gobjectSetPrivateData :: forall o. (HasCallStack, DerivedGObject o) =>
                         o -> GObjectPrivateData o -> IO ()
gobjectSetPrivateData obj value = withManagedPtr obj $ \objPtr ->
  instanceSetPrivateData objPtr value

-- | Set the private data for a given instance.
instanceSetPrivateData :: forall o. (HasCallStack, DerivedGObject o) =>
                          Ptr o -> GObjectPrivateData o -> IO ()
instanceSetPrivateData objPtr priv = do
  key <- privateKey @o
  instanceSetUserData objPtr key priv

foreign import ccall g_object_class_install_property ::
   GObjectClass -> CUInt -> Ptr GParamSpec -> IO ()

-- | Modify the private data for the given object.
gobjectModifyPrivateData :: forall o. (HasCallStack, DerivedGObject o) =>
                         o -> (GObjectPrivateData o -> GObjectPrivateData o)
                         -> IO ()
gobjectModifyPrivateData obj transform = do
  private <- gobjectGetPrivateData obj
  gobjectSetPrivateData obj (transform private)

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

-- | Add a `${type gboolean}`-valued property to the given object class.
gobjectInstallGBooleanProperty :: DerivedGObject o =>
                               GObjectClass -> GBooleanPropertyInfo o -> IO ()
gobjectInstallGBooleanProperty klass propInfo = do
  pspec <- gParamSpecGBoolean propInfo
  withManagedPtr pspec $ \pspecPtr ->
    g_object_class_install_property klass 1 pspecPtr
