{-# LANGUAGE ScopedTypeVariables #-}

module Data.GI.Base.Properties
    ( setObjectPropertyString
    , setObjectPropertyStringArray
    , setObjectPropertyPtr
    , setObjectPropertyInt
    , setObjectPropertyUInt
    , setObjectPropertyLong
    , setObjectPropertyULong
    , setObjectPropertyInt32
    , setObjectPropertyUInt32
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
    , setObjectPropertyCallback

    , getObjectPropertyString
    , getObjectPropertyStringArray
    , getObjectPropertyPtr
    , getObjectPropertyInt
    , getObjectPropertyUInt
    , getObjectPropertyLong
    , getObjectPropertyULong
    , getObjectPropertyInt32
    , getObjectPropertyUInt32
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
    , getObjectPropertyCallback

    , constructObjectPropertyString
    , constructObjectPropertyStringArray
    , constructObjectPropertyPtr
    , constructObjectPropertyInt
    , constructObjectPropertyUInt
    , constructObjectPropertyLong
    , constructObjectPropertyULong
    , constructObjectPropertyInt32
    , constructObjectPropertyUInt32
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
    , constructObjectPropertyCallback
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

import Foreign (Ptr, FunPtr, Int32, Word32, Int64, Word64, nullPtr,
                castFunPtrToPtr, castPtrToFunPtr)
import Foreign.C (CString, withCString)
import Foreign.C.Types (CInt, CUInt, CLong, CULong)

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
                           GType -> IO (GValueConstruct o)
constructObjectProperty propName propValue setter gtype = do
  gvalue <- buildGValue gtype setter propValue
  return (GValueConstruct propName gvalue)

setObjectPropertyString :: GObject a =>
                           a -> String -> Maybe Text -> IO ()
setObjectPropertyString obj propName str =
    setObjectProperty obj propName str set_string gtypeString

constructObjectPropertyString :: String -> Maybe Text ->
                                 IO (GValueConstruct o)
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
                              IO (GValueConstruct o)
constructObjectPropertyPtr propName ptr =
    constructObjectProperty propName ptr set_pointer gtypePointer

getObjectPropertyPtr :: GObject a =>
                        a -> String -> IO (Ptr b)
getObjectPropertyPtr obj propName =
    getObjectProperty obj propName get_pointer gtypePointer

setObjectPropertyInt :: GObject a =>
                         a -> String -> CInt -> IO ()
setObjectPropertyInt obj propName int =
    setObjectProperty obj propName int set_int gtypeInt

constructObjectPropertyInt :: String -> CInt ->
                              IO (GValueConstruct o)
constructObjectPropertyInt propName int =
    constructObjectProperty propName int set_int gtypeInt

getObjectPropertyInt :: GObject a => a -> String -> IO CInt
getObjectPropertyInt obj propName =
    getObjectProperty obj propName get_int gtypeInt

setObjectPropertyUInt :: GObject a =>
                          a -> String -> CUInt -> IO ()
setObjectPropertyUInt obj propName uint =
    setObjectProperty obj propName uint set_uint gtypeUInt

constructObjectPropertyUInt :: String -> CUInt ->
                                IO (GValueConstruct o)
constructObjectPropertyUInt propName uint =
    constructObjectProperty propName uint set_uint gtypeUInt

getObjectPropertyUInt :: GObject a => a -> String -> IO CUInt
getObjectPropertyUInt obj propName =
    getObjectProperty obj propName get_uint gtypeUInt

setObjectPropertyLong :: GObject a =>
                         a -> String -> CLong -> IO ()
setObjectPropertyLong obj propName int =
    setObjectProperty obj propName int set_long gtypeLong

constructObjectPropertyLong :: String -> CLong ->
                               IO (GValueConstruct o)
constructObjectPropertyLong propName int =
    constructObjectProperty propName int set_long gtypeLong

getObjectPropertyLong :: GObject a => a -> String -> IO CLong
getObjectPropertyLong obj propName =
    getObjectProperty obj propName get_long gtypeLong

setObjectPropertyULong :: GObject a =>
                          a -> String -> CULong -> IO ()
setObjectPropertyULong obj propName uint =
    setObjectProperty obj propName uint set_ulong gtypeULong

constructObjectPropertyULong :: String -> CULong ->
                                IO (GValueConstruct o)
constructObjectPropertyULong propName uint =
    constructObjectProperty propName uint set_ulong gtypeULong

getObjectPropertyULong :: GObject a => a -> String -> IO CULong
getObjectPropertyULong obj propName =
    getObjectProperty obj propName get_ulong gtypeULong

setObjectPropertyInt32 :: GObject a =>
                          a -> String -> Int32 -> IO ()
setObjectPropertyInt32 obj propName int32 =
    setObjectProperty obj propName int32 set_int32 gtypeInt

constructObjectPropertyInt32 :: String -> Int32 ->
                                IO (GValueConstruct o)
constructObjectPropertyInt32 propName int32 =
    constructObjectProperty propName int32 set_int32 gtypeInt

getObjectPropertyInt32 :: GObject a => a -> String -> IO Int32
getObjectPropertyInt32 obj propName =
    getObjectProperty obj propName get_int32 gtypeInt

setObjectPropertyUInt32 :: GObject a =>
                          a -> String -> Word32 -> IO ()
setObjectPropertyUInt32 obj propName uint32 =
    setObjectProperty obj propName uint32 set_uint32 gtypeUInt

constructObjectPropertyUInt32 :: String -> Word32 ->
                                 IO (GValueConstruct o)
constructObjectPropertyUInt32 propName uint32 =
    constructObjectProperty propName uint32 set_uint32 gtypeUInt

getObjectPropertyUInt32 :: GObject a => a -> String -> IO Word32
getObjectPropertyUInt32 obj propName =
    getObjectProperty obj propName get_uint32 gtypeUInt

setObjectPropertyInt64 :: GObject a =>
                          a -> String -> Int64 -> IO ()
setObjectPropertyInt64 obj propName int64 =
    setObjectProperty obj propName int64 set_int64 gtypeInt64

constructObjectPropertyInt64 :: String -> Int64 ->
                                IO (GValueConstruct o)
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
                                 IO (GValueConstruct o)
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
                                 IO (GValueConstruct o)
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
                                  IO (GValueConstruct o)
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

constructObjectPropertyBool :: String -> Bool -> IO (GValueConstruct o)
constructObjectPropertyBool propName bool =
    constructObjectProperty propName bool set_boolean gtypeBoolean

getObjectPropertyBool :: GObject a => a -> String -> IO Bool
getObjectPropertyBool obj propName =
    getObjectProperty obj propName get_boolean gtypeBoolean

setObjectPropertyGType :: GObject a =>
                         a -> String -> GType -> IO ()
setObjectPropertyGType obj propName gtype =
    setObjectProperty obj propName gtype set_gtype gtypeGType

constructObjectPropertyGType :: String -> GType -> IO (GValueConstruct o)
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

constructObjectPropertyObject :: forall a o. GObject a =>
                                 String -> Maybe a -> IO (GValueConstruct o)
constructObjectPropertyObject propName maybeObject = do
  gtype <- gobjectType (undefined :: a)
  maybeWithManagedPtr maybeObject $ \objectPtr ->
      constructObjectProperty propName objectPtr set_object gtype

getObjectPropertyObject :: forall a b. (GObject a, GObject b) =>
                           a -> String -> (ManagedPtr b -> b) -> IO (Maybe b)
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

constructObjectPropertyBoxed :: forall a o. (BoxedObject a) =>
                                String -> Maybe a -> IO (GValueConstruct o)
constructObjectPropertyBoxed propName maybeBoxed = do
  gtype <- boxedType (undefined :: a)
  maybeWithManagedPtr maybeBoxed $ \boxedPtr ->
      constructObjectProperty propName boxedPtr set_boxed gtype

getObjectPropertyBoxed :: forall a b. (GObject a, BoxedObject b) =>
                          a -> String -> (ManagedPtr b -> b) -> IO (Maybe b)
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
                                      IO (GValueConstruct o)
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
                               String -> a -> IO (GValueConstruct o)
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

constructObjectPropertyFlags :: forall a o. (IsGFlag a, BoxedFlags a)
                                => String -> [a] -> IO (GValueConstruct o)
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
                               -> IO (GValueConstruct o)
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
                                    IO (GValueConstruct o)
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
                                    IO (GValueConstruct o)
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

constructObjectPropertyHash :: String -> b -> IO (GValueConstruct o)
constructObjectPropertyHash =
    error $ "Constructing GHashTable properties not supported yet."

getObjectPropertyHash :: GObject a => a -> String -> IO b
getObjectPropertyHash =
    error $ "Getting GHashTable properties not supported yet."

setObjectPropertyCallback :: GObject a => a -> String -> FunPtr b -> IO ()
setObjectPropertyCallback obj propName funPtr =
    setObjectProperty obj propName (castFunPtrToPtr funPtr) set_pointer gtypePointer

constructObjectPropertyCallback :: String -> FunPtr b -> IO (GValueConstruct o)
constructObjectPropertyCallback propName funPtr =
  constructObjectProperty propName (castFunPtrToPtr funPtr) set_pointer gtypePointer

getObjectPropertyCallback :: GObject a => a -> String ->
                             (FunPtr b -> c) -> IO (Maybe c)
getObjectPropertyCallback obj propName wrapper = do
  ptr <- getObjectProperty obj propName get_pointer gtypePointer
  if ptr /= nullPtr
    then return . Just . wrapper $ castPtrToFunPtr ptr
    else return Nothing
