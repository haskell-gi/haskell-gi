{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds, KindSignatures,
FlexibleInstances, MultiParamTypeClasses, TypeFamilies, PolyKinds,
EmptyDataDecls, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Data.GI.Base.Properties
    ( new

    , PropertyNotify(..)
    , GObjectNotifySignalInfo

    , setObjectPropertyString
    , setObjectPropertyStringArray
    , setObjectPropertyPtr
    , setObjectPropertyCInt
    , setObjectPropertyCUInt
    , setObjectPropertyInt64
    , setObjectPropertyUInt64
    , setObjectPropertyFloat
    , setObjectPropertyDouble
    , setObjectPropertyBool
    , setObjectPropertyGType
    , setObjectPropertyObject
    , setObjectPropertyBoxed
    , setObjectPropertyEnum
    , setObjectPropertyFlags
    , setObjectPropertyVariant
    , setObjectPropertyByteArray
    , setObjectPropertyPtrGList
    , setObjectPropertyHash

    , getObjectPropertyString
    , getObjectPropertyStringArray
    , getObjectPropertyPtr
    , getObjectPropertyCInt
    , getObjectPropertyCUInt
    , getObjectPropertyInt64
    , getObjectPropertyUInt64
    , getObjectPropertyFloat
    , getObjectPropertyDouble
    , getObjectPropertyBool
    , getObjectPropertyGType
    , getObjectPropertyObject
    , getObjectPropertyBoxed
    , getObjectPropertyEnum
    , getObjectPropertyFlags
    , getObjectPropertyVariant
    , getObjectPropertyByteArray
    , getObjectPropertyPtrGList
    , getObjectPropertyHash

    , constructObjectPropertyString
    , constructObjectPropertyStringArray
    , constructObjectPropertyPtr
    , constructObjectPropertyCInt
    , constructObjectPropertyCUInt
    , constructObjectPropertyInt64
    , constructObjectPropertyUInt64
    , constructObjectPropertyFloat
    , constructObjectPropertyDouble
    , constructObjectPropertyBool
    , constructObjectPropertyGType
    , constructObjectPropertyObject
    , constructObjectPropertyBoxed
    , constructObjectPropertyEnum
    , constructObjectPropertyFlags
    , constructObjectPropertyVariant
    , constructObjectPropertyByteArray
    , constructObjectPropertyPtrGList
    , constructObjectPropertyHash
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Proxy (Proxy(..))

import Data.GI.Base.BasicTypes
import Data.GI.Base.BasicConversions
import Data.GI.Base.ManagedPtr
import Data.GI.Base.Attributes
import Data.GI.Base.GParamSpec (newGParamSpecFromPtr)
import Data.GI.Base.GValue
import Data.GI.Base.GVariant (newGVariantFromPtr)
import Data.GI.Base.Overloading (ResolveAttribute, HasAttr)
import Data.GI.Base.Signals (SignalConnectMode, SignalHandlerId,
                         connectSignalFunPtr,
                         SignalInfo(HaskellCallbackType, connectSignal))

import GHC.Exts (Constraint)
import GHC.TypeLits

import Foreign hiding (new)
import Foreign.C

#include <glib-object.h>

foreign import ccall "dbg_g_object_newv" g_object_newv ::
    GType -> CUInt -> Ptr a -> IO (Ptr b)

-- | Construct a GObject given the constructor and a list of settable
-- attributes. AttrOps are always constructible, so we don't need to
-- enforce constraints here.
new :: forall o m. (GObject o, MonadIO m)
    => (ForeignPtr o -> o)
    -> [AttrOp o 'AttrConstruct]
    -> m o
new constructor attrs = liftIO $ do
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
    resolve :: proxy attr -> Proxy (ResolveAttribute attr o)
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

-- | Proxy for "notify::property-name" signals.
data PropertyNotify (s :: Symbol) (propName :: Symbol) (constraint :: * -> Constraint) where
  PropertyNotify :: KnownSymbol propName => proxy propName ->
                    PropertyNotify "notify::[property]" propName (HasAttr propName)

-- | Connection information for a "notify" signal indicating that a
-- specific property changed (see `PropertyNotify` for the relevant
-- constructor).
data GObjectNotifySignalInfo
instance SignalInfo GObjectNotifySignalInfo where
  type HaskellCallbackType GObjectNotifySignalInfo = GObjectNotifyCallback
  connectSignal = connectGObjectNotify

-- | Type for a `GObject` `notify` callback.
type GObjectNotifyCallback = GParamSpec -> IO ()

gobjectNotifyCallbackWrapper ::
    GObjectNotifyCallback -> Ptr () -> Ptr GParamSpec -> Ptr () -> IO ()
gobjectNotifyCallbackWrapper _cb _ pspec _ = do
    pspec' <- newGParamSpecFromPtr pspec
    _cb  pspec'

type GObjectNotifyCallbackC = Ptr () -> Ptr GParamSpec -> Ptr () -> IO ()

foreign import ccall "wrapper"
    mkGObjectNotifyCallback :: GObjectNotifyCallbackC -> IO (FunPtr GObjectNotifyCallbackC)

-- | Connect the given notify callback for a GObject.
connectGObjectNotify :: forall o i proxy propName constraint.
                        (GObject o, constraint o, KnownSymbol propName) =>
                        proxy (i :: *) (propName :: Symbol) (constraint :: * -> Constraint) ->
                        o -> GObjectNotifyCallback ->
                        SignalConnectMode -> IO SignalHandlerId
connectGObjectNotify _ obj cb after = do
  cb' <- mkGObjectNotifyCallback (gobjectNotifyCallbackWrapper cb)
  let signalName = "notify::" ++ symbolVal (Proxy :: Proxy propName)
  connectSignalFunPtr obj signalName cb' after

foreign import ccall "g_object_set_property" g_object_set_property ::
    Ptr a -> CString -> Ptr GValue -> IO ()

setObjectProperty :: GObject a => a -> String -> b ->
                     (GValue -> b -> IO ()) -> GType -> IO ()
setObjectProperty obj propName propValue setter (GType gtype) = do
  gvalue <- buildGValue (GType gtype) setter propValue
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          withManagedPtr gvalue $ \gvalueptr ->
              g_object_set_property objPtr cPropName gvalueptr

foreign import ccall "g_object_get_property" g_object_get_property ::
    Ptr a -> CString -> Ptr GValue -> IO ()

getObjectProperty :: GObject a => a -> String ->
                     (GValue -> IO b) -> GType -> IO b
getObjectProperty obj propName getter gtype = do
  gvalue <- newGValue gtype
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          withManagedPtr gvalue $ \gvalueptr ->
              g_object_get_property objPtr cPropName gvalueptr
  getter gvalue

constructObjectProperty :: String -> b -> (GValue -> b -> IO ()) ->
                           GType -> IO (String, GValue)
constructObjectProperty propName propValue setter gtype = do
  gvalue <- buildGValue gtype setter propValue
  return (propName, gvalue)

setObjectPropertyString :: GObject a =>
                           a -> String -> Text -> IO ()
setObjectPropertyString obj propName str =
    setObjectProperty obj propName str set_string (GType #const G_TYPE_STRING)

constructObjectPropertyString :: String -> Text ->
                                 IO (String, GValue)
constructObjectPropertyString propName str =
    constructObjectProperty propName str set_string (GType #const G_TYPE_STRING)

getObjectPropertyString :: GObject a =>
                           a -> String -> IO Text
getObjectPropertyString obj propName =
    getObjectProperty obj propName get_string (GType #const G_TYPE_STRING)

setObjectPropertyPtr :: GObject a =>
                        a -> String -> Ptr b -> IO ()
setObjectPropertyPtr obj propName ptr =
    setObjectProperty obj propName ptr set_pointer (GType #const G_TYPE_POINTER)

constructObjectPropertyPtr :: String -> Ptr b ->
                              IO (String, GValue)
constructObjectPropertyPtr propName ptr =
    constructObjectProperty propName ptr set_pointer (GType #const G_TYPE_POINTER)

getObjectPropertyPtr :: GObject a =>
                        a -> String -> IO (Ptr b)
getObjectPropertyPtr obj propName =
    getObjectProperty obj propName get_pointer (GType #const G_TYPE_POINTER)

setObjectPropertyCInt :: GObject a =>
                         a -> String -> Int32 -> IO ()
setObjectPropertyCInt obj propName int =
    setObjectProperty obj propName int set_int32 (GType #const G_TYPE_INT)

constructObjectPropertyCInt :: String -> Int32 ->
                                IO (String, GValue)
constructObjectPropertyCInt propName int =
    constructObjectProperty propName int set_int32 (GType #const G_TYPE_INT)

getObjectPropertyCInt :: GObject a => a -> String -> IO Int32
getObjectPropertyCInt obj propName =
    getObjectProperty obj propName get_int32 (GType #const G_TYPE_INT)

setObjectPropertyCUInt :: GObject a =>
                          a -> String -> Word32 -> IO ()
setObjectPropertyCUInt obj propName uint =
    setObjectProperty obj propName uint set_uint32 (GType #const G_TYPE_UINT)

constructObjectPropertyCUInt :: String -> Word32 ->
                                IO (String, GValue)
constructObjectPropertyCUInt propName uint =
    constructObjectProperty propName uint set_uint32 (GType #const G_TYPE_UINT)

getObjectPropertyCUInt :: GObject a => a -> String -> IO Word32
getObjectPropertyCUInt obj propName =
    getObjectProperty obj propName get_uint32 (GType #const G_TYPE_UINT)

setObjectPropertyInt64 :: GObject a =>
                          a -> String -> Int64 -> IO ()
setObjectPropertyInt64 obj propName int64 =
    setObjectProperty obj propName int64 set_int64 (GType #const G_TYPE_INT64)

constructObjectPropertyInt64 :: String -> Int64 ->
                                IO (String, GValue)
constructObjectPropertyInt64 propName int64 =
    constructObjectProperty propName int64 set_int64 (GType #const G_TYPE_INT64)

getObjectPropertyInt64 :: GObject a => a -> String -> IO Int64
getObjectPropertyInt64 obj propName =
    getObjectProperty obj propName get_int64 (GType #const G_TYPE_INT64)

setObjectPropertyUInt64 :: GObject a =>
                          a -> String -> Word64 -> IO ()
setObjectPropertyUInt64 obj propName uint64 =
    setObjectProperty obj propName uint64 set_uint64 (GType #const G_TYPE_UINT64)

constructObjectPropertyUInt64 :: String -> Word64 ->
                                 IO (String, GValue)
constructObjectPropertyUInt64 propName uint64 =
    constructObjectProperty propName uint64 set_uint64 (GType #const G_TYPE_UINT64)

getObjectPropertyUInt64 :: GObject a => a -> String -> IO Word64
getObjectPropertyUInt64 obj propName =
    getObjectProperty obj propName get_uint64 (GType #const G_TYPE_UINT64)

setObjectPropertyFloat :: GObject a =>
                           a -> String -> Float -> IO ()
setObjectPropertyFloat obj propName float =
    setObjectProperty obj propName float set_float (GType #const G_TYPE_FLOAT)

constructObjectPropertyFloat :: String -> Float ->
                                 IO (String, GValue)
constructObjectPropertyFloat propName float =
    constructObjectProperty propName float set_float (GType #const G_TYPE_FLOAT)

getObjectPropertyFloat :: GObject a =>
                           a -> String -> IO Float
getObjectPropertyFloat obj propName =
    getObjectProperty obj propName get_float (GType #const G_TYPE_FLOAT)

setObjectPropertyDouble :: GObject a =>
                            a -> String -> Double -> IO ()
setObjectPropertyDouble obj propName double =
    setObjectProperty obj propName double set_double (GType #const G_TYPE_DOUBLE)

constructObjectPropertyDouble :: String -> Double ->
                                  IO (String, GValue)
constructObjectPropertyDouble propName double =
    constructObjectProperty propName double set_double (GType #const G_TYPE_DOUBLE)

getObjectPropertyDouble :: GObject a =>
                            a -> String -> IO Double
getObjectPropertyDouble obj propName =
    getObjectProperty obj propName get_double (GType #const G_TYPE_DOUBLE)

setObjectPropertyBool :: GObject a =>
                         a -> String -> Bool -> IO ()
setObjectPropertyBool obj propName bool =
    setObjectProperty obj propName bool set_boolean (GType #const G_TYPE_BOOLEAN)

constructObjectPropertyBool :: String -> Bool -> IO (String, GValue)
constructObjectPropertyBool propName bool =
    constructObjectProperty propName bool set_boolean (GType #const G_TYPE_BOOLEAN)

getObjectPropertyBool :: GObject a => a -> String -> IO Bool
getObjectPropertyBool obj propName =
    getObjectProperty obj propName get_boolean (GType #const G_TYPE_BOOLEAN)

setObjectPropertyGType :: GObject a =>
                         a -> String -> GType -> IO ()
setObjectPropertyGType obj propName gtype =
    setObjectProperty obj propName gtype set_gtype (GType #const G_TYPE_GTYPE)

constructObjectPropertyGType :: String -> GType -> IO (String, GValue)
constructObjectPropertyGType propName bool =
    constructObjectProperty propName bool set_gtype (GType #const G_TYPE_GTYPE)

getObjectPropertyGType :: GObject a => a -> String -> IO GType
getObjectPropertyGType obj propName =
    getObjectProperty obj propName get_gtype (GType #const G_TYPE_GTYPE)

setObjectPropertyObject :: (GObject a, GObject b) =>
                           a -> String -> b -> IO ()
setObjectPropertyObject obj propName object = do
  gtype <- gobjectType object
  withManagedPtr object $ \objectPtr ->
      setObjectProperty obj propName objectPtr set_object gtype

constructObjectPropertyObject :: GObject a =>
                                 String -> a -> IO (String, GValue)
constructObjectPropertyObject propName object = do
  gtype <- gobjectType object
  withManagedPtr object $ \objectPtr ->
      constructObjectProperty propName objectPtr set_object gtype

getObjectPropertyObject :: forall a b. (GObject a, GObject b) =>
                           a -> String -> (ForeignPtr b -> b) -> IO b
getObjectPropertyObject obj propName constructor = do
  gtype <- gobjectType (undefined :: b)
  getObjectProperty obj propName
                        (\val -> (get_object val :: IO (Ptr b))
                                 >>= newObject constructor)
                      gtype

setObjectPropertyBoxed :: (GObject a, BoxedObject b) =>
                          a -> String -> b -> IO ()
setObjectPropertyBoxed obj propName boxed = do
  gtype <- boxedType boxed
  withManagedPtr boxed $ \boxedPtr ->
        setObjectProperty obj propName boxedPtr set_boxed gtype

constructObjectPropertyBoxed :: (BoxedObject a) => String -> a ->
                                IO (String, GValue)
constructObjectPropertyBoxed propName boxed = do
  gtype <- boxedType boxed
  withManagedPtr boxed $ \boxedPtr ->
      constructObjectProperty propName boxedPtr set_boxed gtype

getObjectPropertyBoxed :: forall a b. (GObject a, BoxedObject b) =>
                          a -> String -> (ForeignPtr b -> b) -> IO b
getObjectPropertyBoxed obj propName constructor = do
  gtype <- boxedType (undefined :: b)
  getObjectProperty obj propName (get_boxed >=> newBoxed constructor) gtype

setObjectPropertyStringArray :: GObject a =>
                                a -> String -> [Text] -> IO ()
setObjectPropertyStringArray obj propName strv = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  setObjectProperty obj propName cStrv set_boxed (GType #const G_TYPE_STRV)
  mapZeroTerminatedCArray free cStrv
  free cStrv

constructObjectPropertyStringArray :: String -> [Text] ->
                                      IO (String, GValue)
constructObjectPropertyStringArray propName strv = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  result <- constructObjectProperty propName cStrv set_boxed (GType #const G_TYPE_STRV)
  mapZeroTerminatedCArray free cStrv
  free cStrv
  return result

getObjectPropertyStringArray :: GObject a =>
                                a -> String -> IO [Text]
getObjectPropertyStringArray obj propName =
    getObjectProperty obj propName
                      (get_boxed >=> unpackZeroTerminatedUTF8CArray . castPtr)
                      (GType #const G_TYPE_STRV)

setObjectPropertyEnum :: (GObject a, Enum b, BoxedEnum b) =>
                         a -> String -> b -> IO ()
setObjectPropertyEnum obj propName enum = do
  gtype <- boxedEnumType enum
  let cEnum = (fromIntegral . fromEnum) enum
  setObjectProperty obj propName cEnum set_enum gtype

constructObjectPropertyEnum :: (Enum a, BoxedEnum a) =>
                               String -> a -> IO (String, GValue)
constructObjectPropertyEnum propName enum = do
  gtype <- boxedEnumType enum
  let cEnum = (fromIntegral . fromEnum) enum
  constructObjectProperty propName cEnum set_enum gtype

getObjectPropertyEnum :: forall a b. (GObject a,
                                      Enum b, BoxedEnum b) =>
                         a -> String -> IO b
getObjectPropertyEnum obj propName = do
  gtype <- boxedEnumType (undefined :: b)
  getObjectProperty obj propName
                    (\val -> toEnum . fromIntegral <$> get_enum val)
                    gtype

setObjectPropertyFlags :: (IsGFlag b, GObject a) =>
                          a -> String -> [b] -> IO ()
setObjectPropertyFlags obj propName flags =
    let cFlags = gflagsToWord flags
    in setObjectProperty obj propName cFlags set_flags (GType #const G_TYPE_FLAGS)

constructObjectPropertyFlags :: IsGFlag a => String -> [a] ->
                                IO (String, GValue)
constructObjectPropertyFlags propName flags =
    let cFlags = gflagsToWord flags
    in constructObjectProperty propName cFlags set_flags (GType #const G_TYPE_FLAGS)

getObjectPropertyFlags :: (GObject a, IsGFlag b) =>
                          a -> String -> IO [b]
getObjectPropertyFlags obj propName =
    getObjectProperty obj propName
                          (\val -> wordToGFlags <$> get_flags val)
                          (GType #const G_TYPE_FLAGS)

setObjectPropertyVariant :: GObject a =>
                            a -> String -> GVariant -> IO ()
setObjectPropertyVariant obj propName variant =
    withManagedPtr variant $ \variantPtr ->
        setObjectProperty obj propName variantPtr set_variant
                              (GType #const G_TYPE_VARIANT)

constructObjectPropertyVariant :: String -> GVariant -> IO (String, GValue)
constructObjectPropertyVariant propName obj =
    withManagedPtr obj $ \objPtr ->
        constructObjectProperty propName objPtr set_variant
                                    (GType #const G_TYPE_VARIANT)

getObjectPropertyVariant :: GObject a => a -> String ->
                            IO GVariant
getObjectPropertyVariant obj propName =
    getObjectProperty obj propName (get_variant >=> newGVariantFromPtr)
                      (GType #const G_TYPE_VARIANT)

setObjectPropertyByteArray :: GObject a =>
                              a -> String -> B.ByteString -> IO ()
setObjectPropertyByteArray obj propName bytes = do
  packed <- packGByteArray bytes
  setObjectProperty obj propName packed set_boxed (GType #const G_TYPE_BYTE_ARRAY)
  unrefGByteArray packed

constructObjectPropertyByteArray :: String -> B.ByteString ->
                                    IO (String, GValue)
constructObjectPropertyByteArray propName bytes = do
  packed <- packGByteArray bytes
  result <- constructObjectProperty propName packed
            set_boxed (GType #const G_TYPE_BYTE_ARRAY)
  unrefGByteArray packed
  return result

getObjectPropertyByteArray :: GObject a =>
                              a -> String -> IO B.ByteString
getObjectPropertyByteArray obj propName =
    getObjectProperty obj propName (get_boxed >=> unpackGByteArray)
                      (GType #const G_TYPE_BYTE_ARRAY)

setObjectPropertyPtrGList :: GObject a =>
                              a -> String -> [Ptr b] -> IO ()
setObjectPropertyPtrGList obj propName ptrs = do
  packed <- packGList ptrs
  setObjectProperty obj propName packed set_boxed (GType #const G_TYPE_POINTER)
  g_list_free packed

constructObjectPropertyPtrGList :: String -> [Ptr a] ->
                                    IO (String, GValue)
constructObjectPropertyPtrGList propName ptrs = do
  packed <- packGList ptrs
  result <- constructObjectProperty propName packed
            set_boxed (GType #const G_TYPE_POINTER)
  g_list_free packed
  return result

getObjectPropertyPtrGList :: GObject a =>
                              a -> String -> IO [Ptr b]
getObjectPropertyPtrGList obj propName =
    getObjectProperty obj propName (get_pointer >=> unpackGList)
                      (GType #const G_TYPE_POINTER)

setObjectPropertyHash :: GObject a => a -> String -> b -> IO ()
setObjectPropertyHash =
    error $ "Setting GHashTable properties not supported yet."

constructObjectPropertyHash :: String -> b -> IO (String, GValue)
constructObjectPropertyHash =
    error $ "Constructing GHashTable properties not supported yet."

getObjectPropertyHash :: GObject a => a -> String -> IO b
getObjectPropertyHash =
    error $ "Getting GHashTable properties not supported yet."
