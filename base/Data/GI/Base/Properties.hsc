{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Data.GI.Base.Properties
    ( setObjectPropertyIsGValueInstance
    , setObjectPropertyString
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
    , setObjectPropertyClosure
    , setObjectPropertyVariant
    , setObjectPropertyByteArray
    , setObjectPropertyPtrGList
    , setObjectPropertyHash
    , setObjectPropertyCallback
    , setObjectPropertyGError
    , setObjectPropertyGValue

    , getObjectPropertyIsGValueInstance
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
    , getObjectPropertyClosure
    , getObjectPropertyVariant
    , getObjectPropertyByteArray
    , getObjectPropertyPtrGList
    , getObjectPropertyHash
    , getObjectPropertyCallback
    , getObjectPropertyGError
    , getObjectPropertyGValue

    , constructObjectPropertyIsGValueInstance
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
    , constructObjectPropertyClosure
    , constructObjectPropertyVariant
    , constructObjectPropertyByteArray
    , constructObjectPropertyPtrGList
    , constructObjectPropertyHash
    , constructObjectPropertyCallback
    , constructObjectPropertyGError
    , constructObjectPropertyGValue
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad ((>=>))

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)

import Data.GI.Base.BasicTypes
import Data.GI.Base.BasicConversions
import Data.GI.Base.ManagedPtr
import Data.GI.Base.GError (GError(..))
import Data.GI.Base.GValue
import Data.GI.Base.GType
import Data.GI.Base.GClosure (GClosure(..))
import Data.GI.Base.GVariant (newGVariantFromPtr)
import Data.GI.Base.Utils (freeMem, convertIfNonNull)

import Foreign (Ptr, FunPtr, Int32, Word32, Int64, Word64, nullPtr,
                castFunPtrToPtr, castPtrToFunPtr)
import Foreign.C (CString, withCString)
import Foreign.C.Types (CInt, CUInt, CLong, CULong)

#include <glib-object.h>

foreign import ccall "g_object_set_property" g_object_set_property ::
    Ptr a -> CString -> Ptr GValue -> IO ()

-- | Set a property on an object to the given `GValue`.
gobjectSetProperty :: GObject a => a -> String -> GValue -> IO ()
gobjectSetProperty obj propName gvalue =
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          withManagedPtr gvalue $ \gvalueptr ->
              g_object_set_property objPtr cPropName gvalueptr

-- | A convenience wrapper over `gobjectSetProperty` that does the
-- wrapping of a value into a `GValue`.
setObjectProperty :: GObject a => a -> String -> b ->
                     (Ptr GValue -> b -> IO ()) -> GType -> IO ()
setObjectProperty obj propName propValue setter (GType gtype) = do
  gvalue <- buildGValue (GType gtype) setter propValue
  gobjectSetProperty obj propName gvalue

foreign import ccall "g_object_get_property" g_object_get_property ::
    Ptr a -> CString -> Ptr GValue -> IO ()

-- | Get the `GValue` for the given property.
gobjectGetProperty :: GObject a => a -> String -> GType -> IO GValue
gobjectGetProperty obj propName gtype = do
  gvalue <- newGValue gtype
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          withManagedPtr gvalue $ \gvalueptr ->
              g_object_get_property objPtr cPropName gvalueptr
  return gvalue

-- | A convenience wrapper over `gobjectGetProperty` that unwraps the
-- `GValue` into a Haskell value.
getObjectProperty :: GObject a => a -> String ->
                     (Ptr GValue -> IO b) -> GType -> IO b
getObjectProperty obj propName getter gtype = do
  gv <- gobjectGetProperty obj propName gtype
  withManagedPtr gv getter

constructObjectProperty :: String -> b -> (Ptr GValue -> b -> IO ()) ->
                           GType -> IO (GValueConstruct o)
constructObjectProperty propName propValue setter gtype = do
  gvalue <- buildGValue gtype setter propValue
  return (GValueConstruct propName gvalue)

-- | Set a property for a type with a `IsGValue` instance.
setObjectPropertyIsGValueInstance :: (GObject a, IsGValue b) =>
                                     a -> String -> b -> IO ()
setObjectPropertyIsGValueInstance obj propName maybeVal = do
  gvalue <- toGValue maybeVal
  gobjectSetProperty obj propName gvalue

-- | Construct a property for a type with a `IsGValue` instance.
constructObjectPropertyIsGValueInstance :: IsGValue b => String -> b -> IO (GValueConstruct o)
constructObjectPropertyIsGValueInstance propName maybeVal = do
  gvalue <- toGValue maybeVal
  return (GValueConstruct propName gvalue)

-- | Get a nullable property for a type with a `IsGValue` instance.
getObjectPropertyIsGValueInstance :: forall a b. (GObject a, IsGValue b) =>
                       a -> String -> IO b
getObjectPropertyIsGValueInstance obj propName = do
  gtype <- gvalueGType_ @b
  gv <- gobjectGetProperty obj propName gtype
  fromGValue gv

setObjectPropertyString :: GObject a =>
                           a -> String -> Maybe Text -> IO ()
setObjectPropertyString = setObjectPropertyIsGValueInstance

constructObjectPropertyString :: String -> Maybe Text ->
                                 IO (GValueConstruct o)
constructObjectPropertyString = constructObjectPropertyIsGValueInstance

getObjectPropertyString :: GObject a =>
                           a -> String -> IO (Maybe Text)
getObjectPropertyString = getObjectPropertyIsGValueInstance

setObjectPropertyPtr :: GObject a =>
                        a -> String -> Ptr b -> IO ()
setObjectPropertyPtr = setObjectPropertyIsGValueInstance

constructObjectPropertyPtr :: String -> Ptr b ->
                              IO (GValueConstruct o)
constructObjectPropertyPtr = constructObjectPropertyIsGValueInstance

getObjectPropertyPtr :: GObject a =>
                        a -> String -> IO (Ptr b)
getObjectPropertyPtr = getObjectPropertyIsGValueInstance

setObjectPropertyInt :: GObject a =>
                         a -> String -> CInt -> IO ()
setObjectPropertyInt = setObjectPropertyIsGValueInstance

constructObjectPropertyInt :: String -> CInt ->
                              IO (GValueConstruct o)
constructObjectPropertyInt = constructObjectPropertyIsGValueInstance

getObjectPropertyInt :: GObject a => a -> String -> IO CInt
getObjectPropertyInt = getObjectPropertyIsGValueInstance

setObjectPropertyUInt :: GObject a =>
                          a -> String -> CUInt -> IO ()
setObjectPropertyUInt = setObjectPropertyIsGValueInstance

constructObjectPropertyUInt :: String -> CUInt ->
                                IO (GValueConstruct o)
constructObjectPropertyUInt = constructObjectPropertyIsGValueInstance

getObjectPropertyUInt :: GObject a => a -> String -> IO CUInt
getObjectPropertyUInt = getObjectPropertyIsGValueInstance

setObjectPropertyLong :: GObject a =>
                         a -> String -> CLong -> IO ()
setObjectPropertyLong = setObjectPropertyIsGValueInstance

constructObjectPropertyLong :: String -> CLong ->
                               IO (GValueConstruct o)
constructObjectPropertyLong = constructObjectPropertyIsGValueInstance

getObjectPropertyLong :: GObject a => a -> String -> IO CLong
getObjectPropertyLong = getObjectPropertyIsGValueInstance

setObjectPropertyULong :: GObject a =>
                          a -> String -> CULong -> IO ()
setObjectPropertyULong = setObjectPropertyIsGValueInstance

constructObjectPropertyULong :: String -> CULong ->
                                IO (GValueConstruct o)
constructObjectPropertyULong = constructObjectPropertyIsGValueInstance

getObjectPropertyULong :: GObject a => a -> String -> IO CULong
getObjectPropertyULong = getObjectPropertyIsGValueInstance

setObjectPropertyInt32 :: GObject a =>
                          a -> String -> Int32 -> IO ()
setObjectPropertyInt32 = setObjectPropertyIsGValueInstance

constructObjectPropertyInt32 :: String -> Int32 ->
                                IO (GValueConstruct o)
constructObjectPropertyInt32 = constructObjectPropertyIsGValueInstance

getObjectPropertyInt32 :: GObject a => a -> String -> IO Int32
getObjectPropertyInt32 = getObjectPropertyIsGValueInstance

setObjectPropertyUInt32 :: GObject a =>
                          a -> String -> Word32 -> IO ()
setObjectPropertyUInt32 = setObjectPropertyIsGValueInstance

constructObjectPropertyUInt32 :: String -> Word32 ->
                                 IO (GValueConstruct o)
constructObjectPropertyUInt32 = constructObjectPropertyIsGValueInstance

getObjectPropertyUInt32 :: GObject a => a -> String -> IO Word32
getObjectPropertyUInt32 = getObjectPropertyIsGValueInstance

setObjectPropertyInt64 :: GObject a =>
                          a -> String -> Int64 -> IO ()
setObjectPropertyInt64 = setObjectPropertyIsGValueInstance

constructObjectPropertyInt64 :: String -> Int64 ->
                                IO (GValueConstruct o)
constructObjectPropertyInt64 = constructObjectPropertyIsGValueInstance

getObjectPropertyInt64 :: GObject a => a -> String -> IO Int64
getObjectPropertyInt64 = getObjectPropertyIsGValueInstance

setObjectPropertyUInt64 :: GObject a =>
                          a -> String -> Word64 -> IO ()
setObjectPropertyUInt64 = setObjectPropertyIsGValueInstance

constructObjectPropertyUInt64 :: String -> Word64 ->
                                 IO (GValueConstruct o)
constructObjectPropertyUInt64 = constructObjectPropertyIsGValueInstance

getObjectPropertyUInt64 :: GObject a => a -> String -> IO Word64
getObjectPropertyUInt64 = getObjectPropertyIsGValueInstance

setObjectPropertyFloat :: GObject a =>
                           a -> String -> Float -> IO ()
setObjectPropertyFloat = setObjectPropertyIsGValueInstance

constructObjectPropertyFloat :: String -> Float ->
                                 IO (GValueConstruct o)
constructObjectPropertyFloat = constructObjectPropertyIsGValueInstance

getObjectPropertyFloat :: GObject a =>
                           a -> String -> IO Float
getObjectPropertyFloat = getObjectPropertyIsGValueInstance

setObjectPropertyDouble :: GObject a =>
                            a -> String -> Double -> IO ()
setObjectPropertyDouble = setObjectPropertyIsGValueInstance

constructObjectPropertyDouble :: String -> Double ->
                                  IO (GValueConstruct o)
constructObjectPropertyDouble = constructObjectPropertyIsGValueInstance

getObjectPropertyDouble :: GObject a =>
                            a -> String -> IO Double
getObjectPropertyDouble = getObjectPropertyIsGValueInstance

setObjectPropertyBool :: GObject a =>
                         a -> String -> Bool -> IO ()
setObjectPropertyBool = setObjectPropertyIsGValueInstance

constructObjectPropertyBool :: String -> Bool -> IO (GValueConstruct o)
constructObjectPropertyBool = constructObjectPropertyIsGValueInstance

getObjectPropertyBool :: GObject a => a -> String -> IO Bool
getObjectPropertyBool = getObjectPropertyIsGValueInstance

setObjectPropertyGType :: GObject a =>
                         a -> String -> GType -> IO ()
setObjectPropertyGType = setObjectPropertyIsGValueInstance

constructObjectPropertyGType :: String -> GType -> IO (GValueConstruct o)
constructObjectPropertyGType = constructObjectPropertyIsGValueInstance

getObjectPropertyGType :: GObject a => a -> String -> IO GType
getObjectPropertyGType = getObjectPropertyIsGValueInstance

setObjectPropertyObject :: forall a b. (GObject a, GObject b) =>
                           a -> String -> Maybe b -> IO ()
setObjectPropertyObject obj propName maybeObject = do
  gtype <- glibType @b
  maybeWithManagedPtr maybeObject $ \objectPtr ->
      setObjectProperty obj propName objectPtr set_object gtype

constructObjectPropertyObject :: forall a o. GObject a =>
                                 String -> Maybe a -> IO (GValueConstruct o)
constructObjectPropertyObject propName maybeObject = do
  gtype <- glibType @a
  maybeWithManagedPtr maybeObject $ \objectPtr ->
      constructObjectProperty propName objectPtr set_object gtype

getObjectPropertyObject :: forall a b. (GObject a, GObject b) =>
                           a -> String -> (ManagedPtr b -> b) -> IO (Maybe b)
getObjectPropertyObject obj propName constructor = do
  gtype <- glibType @b
  getObjectProperty obj propName
                        (\val -> (get_object val :: IO (Ptr b))
                            >>= flip convertIfNonNull (newObject constructor))
                      gtype

setObjectPropertyBoxed :: forall a b. (GObject a, GBoxed b) =>
                          a -> String -> Maybe b -> IO ()
setObjectPropertyBoxed obj propName maybeBoxed = do
  gtype <- glibType @b
  maybeWithManagedPtr maybeBoxed $ \boxedPtr ->
        setObjectProperty obj propName boxedPtr set_boxed gtype

constructObjectPropertyBoxed :: forall a o. (GBoxed a) =>
                                String -> Maybe a -> IO (GValueConstruct o)
constructObjectPropertyBoxed propName maybeBoxed = do
  gtype <- glibType @a
  maybeWithManagedPtr maybeBoxed $ \boxedPtr ->
      constructObjectProperty propName boxedPtr set_boxed gtype

getObjectPropertyBoxed :: forall a b. (GObject a, GBoxed b) =>
                          a -> String -> (ManagedPtr b -> b) -> IO (Maybe b)
getObjectPropertyBoxed obj propName constructor = do
  gtype <- glibType @b
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

setObjectPropertyEnum :: forall a b. (GObject a, Enum b, BoxedEnum b) =>
                         a -> String -> b -> IO ()
setObjectPropertyEnum obj propName enum = do
  gtype <- glibType @b
  let cEnum = (fromIntegral . fromEnum) enum
  setObjectProperty obj propName cEnum set_enum gtype

constructObjectPropertyEnum :: forall a o. (Enum a, BoxedEnum a) =>
                               String -> a -> IO (GValueConstruct o)
constructObjectPropertyEnum propName enum = do
  gtype <- glibType @a
  let cEnum = (fromIntegral . fromEnum) enum
  constructObjectProperty propName cEnum set_enum gtype

getObjectPropertyEnum :: forall a b. (GObject a,
                                      Enum b, BoxedEnum b) =>
                         a -> String -> IO b
getObjectPropertyEnum obj propName = do
  gtype <- glibType @b
  getObjectProperty obj propName
                    (\val -> toEnum . fromIntegral <$> get_enum val)
                    gtype

setObjectPropertyFlags :: forall a b. (IsGFlag b, BoxedFlags b, GObject a) =>
                          a -> String -> [b] -> IO ()
setObjectPropertyFlags obj propName flags = do
  let cFlags = gflagsToWord flags
  gtype <- glibType @b
  setObjectProperty obj propName cFlags set_flags gtype

constructObjectPropertyFlags :: forall a o. (IsGFlag a, BoxedFlags a)
                                => String -> [a] -> IO (GValueConstruct o)
constructObjectPropertyFlags propName flags = do
  let cFlags = gflagsToWord flags
  gtype <- glibType @a
  constructObjectProperty propName cFlags set_flags gtype

getObjectPropertyFlags :: forall a b. (GObject a, IsGFlag b, BoxedFlags b) =>
                          a -> String -> IO [b]
getObjectPropertyFlags obj propName = do
  gtype <- glibType @b
  getObjectProperty obj propName
                        (\val -> wordToGFlags <$> get_flags val)
                        gtype

setObjectPropertyClosure :: forall a b. GObject a =>
                          a -> String -> Maybe (GClosure b) -> IO ()
setObjectPropertyClosure = setObjectPropertyBoxed

constructObjectPropertyClosure :: String -> Maybe (GClosure a) -> IO (GValueConstruct o)
constructObjectPropertyClosure = constructObjectPropertyBoxed

getObjectPropertyClosure :: forall a b. GObject a =>
                            a -> String -> IO (Maybe (GClosure b))
getObjectPropertyClosure obj propName =
  getObjectPropertyBoxed obj propName GClosure

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
  getObjectProperty obj propName (gvalueGet_ >=> unpackGList) gtypePointer

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
    setObjectProperty obj propName (castFunPtrToPtr funPtr) gvalueSet_ gtypePointer

constructObjectPropertyCallback :: String -> FunPtr b -> IO (GValueConstruct o)
constructObjectPropertyCallback propName funPtr =
  constructObjectProperty propName (castFunPtrToPtr funPtr) gvalueSet_ gtypePointer

getObjectPropertyCallback :: GObject a => a -> String ->
                             (FunPtr b -> c) -> IO (Maybe c)
getObjectPropertyCallback obj propName wrapper = do
  ptr <- getObjectProperty obj propName gvalueGet_ gtypePointer
  if ptr /= nullPtr
    then return . Just . wrapper $ castPtrToFunPtr ptr
    else return Nothing

-- | Set a property of type `GError`.
setObjectPropertyGError :: forall a. GObject a =>
                          a -> String -> Maybe GError -> IO ()
setObjectPropertyGError = setObjectPropertyBoxed

-- | Construct a property of type `GError`.
constructObjectPropertyGError :: String -> Maybe GError -> IO (GValueConstruct o)
constructObjectPropertyGError = constructObjectPropertyBoxed

-- | Get the value of a property of type `GError`.
getObjectPropertyGError :: forall a. GObject a =>
                            a -> String -> IO (Maybe GError)
getObjectPropertyGError obj propName =
  getObjectPropertyBoxed obj propName GError

-- | Set a property of type `GValue`.
setObjectPropertyGValue :: forall a. GObject a =>
                           a -> String -> Maybe GValue -> IO ()
setObjectPropertyGValue = setObjectPropertyBoxed

-- | Construct a property of type `GValue`.
constructObjectPropertyGValue :: String -> Maybe GValue -> IO (GValueConstruct o)
constructObjectPropertyGValue = constructObjectPropertyBoxed

-- | Get the value of a property of type `GValue`.
getObjectPropertyGValue :: forall a. GObject a =>
                           a -> String -> IO (Maybe GValue)
getObjectPropertyGValue obj propName =
  getObjectPropertyBoxed obj propName GValue
