{-# LANGUAGE ScopedTypeVariables #-}

module Data.GI.Base.Properties
    ( setObjectPropertyString
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

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Proxy (Proxy(..))

import Data.GI.Base.BasicTypes
import Data.GI.Base.BasicConversions
import Data.GI.Base.ManagedPtr
import Data.GI.Base.GValue
import Data.GI.Base.GVariant (newGVariantFromPtr)
import Data.GI.Base.Utils (freeMem, convertIfNonNull)

import Foreign (Ptr, ForeignPtr, Int32, Word32, Int64, Word64, nullPtr)
import Foreign.C (CString, withCString)

#include <glib-object.h>

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
                           a -> String -> Maybe Text -> IO ()
setObjectPropertyString obj propName str =
    setObjectProperty obj propName str set_string gtypeString

constructObjectPropertyString :: String -> Maybe Text ->
                                 IO (String, GValue)
constructObjectPropertyString propName str =
    constructObjectProperty propName str set_string gtypeString

getObjectPropertyString :: GObject a =>
                           a -> String -> IO (Maybe Text)
getObjectPropertyString obj propName =
    getObjectProperty obj propName get_string gtypeString

setObjectPropertyPtr :: GObject a =>
                        a -> String -> Ptr b -> IO ()
setObjectPropertyPtr obj propName ptr =
    setObjectProperty obj propName ptr set_pointer gtypePointer

constructObjectPropertyPtr :: String -> Ptr b ->
                              IO (String, GValue)
constructObjectPropertyPtr propName ptr =
    constructObjectProperty propName ptr set_pointer gtypePointer

getObjectPropertyPtr :: GObject a =>
                        a -> String -> IO (Ptr b)
getObjectPropertyPtr obj propName =
    getObjectProperty obj propName get_pointer gtypePointer

setObjectPropertyCInt :: GObject a =>
                         a -> String -> Int32 -> IO ()
setObjectPropertyCInt obj propName int =
    setObjectProperty obj propName int set_int32 gtypeInt32

constructObjectPropertyCInt :: String -> Int32 ->
                                IO (String, GValue)
constructObjectPropertyCInt propName int =
    constructObjectProperty propName int set_int32 gtypeInt32

getObjectPropertyCInt :: GObject a => a -> String -> IO Int32
getObjectPropertyCInt obj propName =
    getObjectProperty obj propName get_int32 gtypeInt32

setObjectPropertyCUInt :: GObject a =>
                          a -> String -> Word32 -> IO ()
setObjectPropertyCUInt obj propName uint =
    setObjectProperty obj propName uint set_uint32 gtypeUInt32

constructObjectPropertyCUInt :: String -> Word32 ->
                                IO (String, GValue)
constructObjectPropertyCUInt propName uint =
    constructObjectProperty propName uint set_uint32 gtypeUInt32

getObjectPropertyCUInt :: GObject a => a -> String -> IO Word32
getObjectPropertyCUInt obj propName =
    getObjectProperty obj propName get_uint32 gtypeUInt32

setObjectPropertyInt64 :: GObject a =>
                          a -> String -> Int64 -> IO ()
setObjectPropertyInt64 obj propName int64 =
    setObjectProperty obj propName int64 set_int64 gtypeInt64

constructObjectPropertyInt64 :: String -> Int64 ->
                                IO (String, GValue)
constructObjectPropertyInt64 propName int64 =
    constructObjectProperty propName int64 set_int64 gtypeInt64

getObjectPropertyInt64 :: GObject a => a -> String -> IO Int64
getObjectPropertyInt64 obj propName =
    getObjectProperty obj propName get_int64 gtypeInt64

setObjectPropertyUInt64 :: GObject a =>
                          a -> String -> Word64 -> IO ()
setObjectPropertyUInt64 obj propName uint64 =
    setObjectProperty obj propName uint64 set_uint64 gtypeUInt64

constructObjectPropertyUInt64 :: String -> Word64 ->
                                 IO (String, GValue)
constructObjectPropertyUInt64 propName uint64 =
    constructObjectProperty propName uint64 set_uint64 gtypeUInt64

getObjectPropertyUInt64 :: GObject a => a -> String -> IO Word64
getObjectPropertyUInt64 obj propName =
    getObjectProperty obj propName get_uint64 gtypeUInt64

setObjectPropertyFloat :: GObject a =>
                           a -> String -> Float -> IO ()
setObjectPropertyFloat obj propName float =
    setObjectProperty obj propName float set_float gtypeFloat

constructObjectPropertyFloat :: String -> Float ->
                                 IO (String, GValue)
constructObjectPropertyFloat propName float =
    constructObjectProperty propName float set_float gtypeFloat

getObjectPropertyFloat :: GObject a =>
                           a -> String -> IO Float
getObjectPropertyFloat obj propName =
    getObjectProperty obj propName get_float gtypeFloat

setObjectPropertyDouble :: GObject a =>
                            a -> String -> Double -> IO ()
setObjectPropertyDouble obj propName double =
    setObjectProperty obj propName double set_double gtypeDouble

constructObjectPropertyDouble :: String -> Double ->
                                  IO (String, GValue)
constructObjectPropertyDouble propName double =
    constructObjectProperty propName double set_double gtypeDouble

getObjectPropertyDouble :: GObject a =>
                            a -> String -> IO Double
getObjectPropertyDouble obj propName =
    getObjectProperty obj propName get_double gtypeDouble

setObjectPropertyBool :: GObject a =>
                         a -> String -> Bool -> IO ()
setObjectPropertyBool obj propName bool =
    setObjectProperty obj propName bool set_boolean gtypeBoolean

constructObjectPropertyBool :: String -> Bool -> IO (String, GValue)
constructObjectPropertyBool propName bool =
    constructObjectProperty propName bool set_boolean gtypeBoolean

getObjectPropertyBool :: GObject a => a -> String -> IO Bool
getObjectPropertyBool obj propName =
    getObjectProperty obj propName get_boolean gtypeBoolean

setObjectPropertyGType :: GObject a =>
                         a -> String -> GType -> IO ()
setObjectPropertyGType obj propName gtype =
    setObjectProperty obj propName gtype set_gtype gtypeGType

constructObjectPropertyGType :: String -> GType -> IO (String, GValue)
constructObjectPropertyGType propName bool =
    constructObjectProperty propName bool set_gtype gtypeGType

getObjectPropertyGType :: GObject a => a -> String -> IO GType
getObjectPropertyGType obj propName =
    getObjectProperty obj propName get_gtype gtypeGType

setObjectPropertyObject :: forall a b. (GObject a, GObject b) =>
                           a -> String -> Maybe b -> IO ()
setObjectPropertyObject obj propName maybeObject = do
  gtype <- gobjectType (undefined :: b)
  maybeWithManagedPtr maybeObject $ \objectPtr ->
      setObjectProperty obj propName objectPtr set_object gtype

constructObjectPropertyObject :: forall a. GObject a =>
                                 String -> Maybe a -> IO (String, GValue)
constructObjectPropertyObject propName maybeObject = do
  gtype <- gobjectType (undefined :: a)
  maybeWithManagedPtr maybeObject $ \objectPtr ->
      constructObjectProperty propName objectPtr set_object gtype

getObjectPropertyObject :: forall a b. (GObject a, GObject b) =>
                           a -> String -> (ForeignPtr b -> b) -> IO (Maybe b)
getObjectPropertyObject obj propName constructor = do
  gtype <- gobjectType (undefined :: b)
  getObjectProperty obj propName
                        (\val -> (get_object val :: IO (Ptr b))
                            >>= flip convertIfNonNull (newObject constructor))
                      gtype

setObjectPropertyBoxed :: forall a b. (GObject a, BoxedObject b) =>
                          a -> String -> Maybe b -> IO ()
setObjectPropertyBoxed obj propName maybeBoxed = do
  gtype <- boxedType (undefined :: b)
  maybeWithManagedPtr maybeBoxed $ \boxedPtr ->
        setObjectProperty obj propName boxedPtr set_boxed gtype

constructObjectPropertyBoxed :: forall a. (BoxedObject a) =>
                                String -> Maybe a -> IO (String, GValue)
constructObjectPropertyBoxed propName maybeBoxed = do
  gtype <- boxedType (undefined :: a)
  maybeWithManagedPtr maybeBoxed $ \boxedPtr ->
      constructObjectProperty propName boxedPtr set_boxed gtype

getObjectPropertyBoxed :: forall a b. (GObject a, BoxedObject b) =>
                          a -> String -> (ForeignPtr b -> b) -> IO (Maybe b)
getObjectPropertyBoxed obj propName constructor = do
  gtype <- boxedType (undefined :: b)
  getObjectProperty obj propName (get_boxed >=>
                                  flip convertIfNonNull (newBoxed constructor))
                    gtype

setObjectPropertyStringArray :: GObject a =>
                                a -> String -> Maybe [Text] -> IO ()
setObjectPropertyStringArray obj propName Nothing =
  setObjectProperty obj propName nullPtr set_boxed gtypeStrv
setObjectPropertyStringArray obj propName (Just strv) = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  setObjectProperty obj propName cStrv set_boxed gtypeStrv
  mapZeroTerminatedCArray freeMem cStrv
  freeMem cStrv

constructObjectPropertyStringArray :: String -> Maybe [Text] ->
                                      IO (String, GValue)
constructObjectPropertyStringArray propName Nothing =
  constructObjectProperty propName nullPtr set_boxed gtypeStrv
constructObjectPropertyStringArray propName (Just strv) = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  result <- constructObjectProperty propName cStrv set_boxed gtypeStrv
  mapZeroTerminatedCArray freeMem cStrv
  freeMem cStrv
  return result

getObjectPropertyStringArray :: GObject a => a -> String -> IO (Maybe [Text])
getObjectPropertyStringArray obj propName =
    getObjectProperty obj propName
                      (get_boxed >=>
                       flip convertIfNonNull unpackZeroTerminatedUTF8CArray)
                      gtypeStrv

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

setObjectPropertyFlags :: forall a b. (IsGFlag b, BoxedFlags b, GObject a) =>
                          a -> String -> [b] -> IO ()
setObjectPropertyFlags obj propName flags = do
  let cFlags = gflagsToWord flags
  gtype <- boxedFlagsType (Proxy :: Proxy b)
  setObjectProperty obj propName cFlags set_flags gtype

constructObjectPropertyFlags :: forall a. (IsGFlag a, BoxedFlags a)
                                => String -> [a] -> IO (String, GValue)
constructObjectPropertyFlags propName flags = do
  let cFlags = gflagsToWord flags
  gtype <- boxedFlagsType (Proxy :: Proxy a)
  constructObjectProperty propName cFlags set_flags gtype

getObjectPropertyFlags :: forall a b. (GObject a, IsGFlag b, BoxedFlags b) =>
                          a -> String -> IO [b]
getObjectPropertyFlags obj propName = do
  gtype <- boxedFlagsType (Proxy :: Proxy b)
  getObjectProperty obj propName
                        (\val -> wordToGFlags <$> get_flags val)
                        gtype

setObjectPropertyVariant :: GObject a =>
                            a -> String -> Maybe GVariant -> IO ()
setObjectPropertyVariant obj propName maybeVariant =
    maybeWithManagedPtr maybeVariant $ \variantPtr ->
        setObjectProperty obj propName variantPtr set_variant gtypeVariant

constructObjectPropertyVariant :: String -> Maybe GVariant
                               -> IO (String, GValue)
constructObjectPropertyVariant propName maybeVariant =
    maybeWithManagedPtr maybeVariant $ \objPtr ->
        constructObjectProperty propName objPtr set_variant gtypeVariant

getObjectPropertyVariant :: GObject a => a -> String ->
                            IO (Maybe GVariant)
getObjectPropertyVariant obj propName =
    getObjectProperty obj propName (get_variant >=>
                                    flip convertIfNonNull newGVariantFromPtr)
                      gtypeVariant

setObjectPropertyByteArray :: GObject a =>
                              a -> String -> Maybe B.ByteString -> IO ()
setObjectPropertyByteArray obj propName Nothing =
    setObjectProperty obj propName nullPtr set_boxed gtypeByteArray
setObjectPropertyByteArray obj propName (Just bytes) = do
  packed <- packGByteArray bytes
  setObjectProperty obj propName packed set_boxed gtypeByteArray
  unrefGByteArray packed

constructObjectPropertyByteArray :: String -> Maybe B.ByteString ->
                                    IO (String, GValue)
constructObjectPropertyByteArray propName Nothing =
    constructObjectProperty propName nullPtr set_boxed gtypeByteArray
constructObjectPropertyByteArray propName (Just bytes) = do
  packed <- packGByteArray bytes
  result <- constructObjectProperty propName packed set_boxed gtypeByteArray
  unrefGByteArray packed
  return result

getObjectPropertyByteArray :: GObject a =>
                              a -> String -> IO (Maybe B.ByteString)
getObjectPropertyByteArray obj propName =
    getObjectProperty obj propName (get_boxed >=>
                                    flip convertIfNonNull unpackGByteArray)
                      gtypeByteArray

setObjectPropertyPtrGList :: GObject a =>
                              a -> String -> [Ptr b] -> IO ()
setObjectPropertyPtrGList obj propName ptrs = do
  packed <- packGList ptrs
  setObjectProperty obj propName packed set_boxed gtypePointer
  g_list_free packed

constructObjectPropertyPtrGList :: String -> [Ptr a] ->
                                    IO (String, GValue)
constructObjectPropertyPtrGList propName ptrs = do
  packed <- packGList ptrs
  result <- constructObjectProperty propName packed set_boxed gtypePointer
  g_list_free packed
  return result

getObjectPropertyPtrGList :: GObject a =>
                              a -> String -> IO [Ptr b]
getObjectPropertyPtrGList obj propName =
    getObjectProperty obj propName (get_pointer >=> unpackGList) gtypePointer

setObjectPropertyHash :: GObject a => a -> String -> b -> IO ()
setObjectPropertyHash =
    error $ "Setting GHashTable properties not supported yet."

constructObjectPropertyHash :: String -> b -> IO (String, GValue)
constructObjectPropertyHash =
    error $ "Constructing GHashTable properties not supported yet."

getObjectPropertyHash :: GObject a => a -> String -> IO b
getObjectPropertyHash =
    error $ "Getting GHashTable properties not supported yet."
