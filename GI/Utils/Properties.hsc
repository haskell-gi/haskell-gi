{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module GI.Utils.Properties
    ( new

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
    , setObjectPropertyObject
    , setObjectPropertyBoxed
    , setObjectPropertyEnum
    , setObjectPropertyFlags
    , setObjectPropertyVariant
    , setObjectPropertyByteArray
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
    , getObjectPropertyObject
    , getObjectPropertyBoxed
    , getObjectPropertyEnum
    , getObjectPropertyFlags
    , getObjectPropertyVariant
    , getObjectPropertyByteArray
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
    , constructObjectPropertyObject
    , constructObjectPropertyBoxed
    , constructObjectPropertyEnum
    , constructObjectPropertyFlags
    , constructObjectPropertyVariant
    , constructObjectPropertyByteArray
    , constructObjectPropertyHash
    ) where

import Control.Monad ((>=>))
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as B

import GI.Utils.BasicTypes
import GI.Utils.BasicConversions
import GI.Utils.ManagedPtr
import GI.Utils.Attributes
import GI.Utils.GValue

import Foreign.Safe hiding (new)
import Foreign.C

#include <glib-object.h>

#if !defined(GLIB_VERSION_2_36)
#literal g_type_init();
#endif

foreign import ccall "g_object_newv" g_object_newv ::
    GType -> CUInt -> Ptr a -> IO (Ptr b)

-- | Construct a GObject given the constructor and a list of settable
-- attributes. AttrOps are always constructible, so we don't need to
-- enforce constraints here.
new :: forall o. GObject o => (ForeignPtr o -> o) ->
       [AttrOp SetAndConstructOp o AttrNew] -> IO o
new constructor attrs = do
  props <- mapM construct attrs
  let nprops = length props
  params <- mallocBytes (nprops*gparameterSize)
  fill params props
  gtype <- gobjectType (undefined :: o)
  result <- g_object_newv gtype (fromIntegral nprops) params
  freeStrings nprops params
  free params
  wrapObject constructor (result :: Ptr o)
  where
    construct :: AttrOp SetAndConstructOp o AttrNew ->
                 IO (String, GValue)
    construct (attr := x) = attrConstruct attr x
    construct (attr :=> x) = x >>= attrConstruct attr

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

foreign import ccall "g_object_set_property" g_object_set_property ::
    Ptr a -> CString -> Ptr GValue -> IO ()

setObjectProperty :: (ManagedPtr a, GObject a) => a -> String -> b ->
                     (GValue -> b -> IO ()) -> GType -> IO ()
setObjectProperty obj propName propValue setter gtype = do
  gvalue <- buildGValue gtype setter propValue
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          withManagedPtr gvalue $ \gvalueptr ->
              g_object_set_property objPtr cPropName gvalueptr

foreign import ccall "g_object_get_property" g_object_get_property ::
    Ptr a -> CString -> Ptr GValue -> IO ()

getObjectProperty :: (GObject a, ManagedPtr a) => a -> String ->
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

setObjectPropertyString :: (GObject a, ManagedPtr a) =>
                           a -> String -> String -> IO ()
setObjectPropertyString obj propName str =
    setObjectProperty obj propName str set_string #const G_TYPE_STRING

constructObjectPropertyString :: String -> String ->
                                 IO (String, GValue)
constructObjectPropertyString propName str =
    constructObjectProperty propName str set_string #const G_TYPE_STRING

getObjectPropertyString :: (ManagedPtr a, GObject a) =>
                           a -> String -> IO String
getObjectPropertyString obj propName =
    getObjectProperty obj propName get_string #const G_TYPE_STRING

setObjectPropertyPtr :: (ManagedPtr a, GObject a) =>
                        a -> String -> Ptr b -> IO ()
setObjectPropertyPtr obj propName ptr =
    setObjectProperty obj propName ptr set_pointer #const G_TYPE_POINTER

constructObjectPropertyPtr :: String -> Ptr b ->
                              IO (String, GValue)
constructObjectPropertyPtr propName ptr =
    constructObjectProperty propName ptr set_pointer #const G_TYPE_POINTER

getObjectPropertyPtr :: (ManagedPtr a, GObject a) =>
                        a -> String -> IO (Ptr b)
getObjectPropertyPtr obj propName =
    getObjectProperty obj propName get_pointer #const G_TYPE_POINTER

setObjectPropertyCInt :: (ManagedPtr a, GObject a) =>
                         a -> String -> Int32 -> IO ()
setObjectPropertyCInt obj propName int =
    setObjectProperty obj propName int set_int32 #const G_TYPE_INT

constructObjectPropertyCInt :: String -> Int32 ->
                                IO (String, GValue)
constructObjectPropertyCInt propName int =
    constructObjectProperty propName int set_int32 #const G_TYPE_INT

getObjectPropertyCInt :: (ManagedPtr a, GObject a) => a -> String -> IO Int32
getObjectPropertyCInt obj propName =
    getObjectProperty obj propName get_int32 #const G_TYPE_INT

setObjectPropertyCUInt :: (ManagedPtr a, GObject a) =>
                          a -> String -> Word32 -> IO ()
setObjectPropertyCUInt obj propName uint =
    setObjectProperty obj propName uint set_uint32 #const G_TYPE_UINT

constructObjectPropertyCUInt :: String -> Word32 ->
                                IO (String, GValue)
constructObjectPropertyCUInt propName uint =
    constructObjectProperty propName uint set_uint32 #const G_TYPE_UINT

getObjectPropertyCUInt :: (ManagedPtr a, GObject a) => a -> String -> IO Word32
getObjectPropertyCUInt obj propName =
    getObjectProperty obj propName get_uint32 #const G_TYPE_UINT

setObjectPropertyInt64 :: (ManagedPtr a, GObject a) =>
                          a -> String -> Int64 -> IO ()
setObjectPropertyInt64 obj propName int64 =
    setObjectProperty obj propName int64 set_int64 #const G_TYPE_INT64

constructObjectPropertyInt64 :: String -> Int64 ->
                                IO (String, GValue)
constructObjectPropertyInt64 propName int64 =
    constructObjectProperty propName int64 set_int64 #const G_TYPE_INT64

getObjectPropertyInt64 :: (ManagedPtr a, GObject a) => a -> String -> IO Int64
getObjectPropertyInt64 obj propName =
    getObjectProperty obj propName get_int64 #const G_TYPE_INT64

setObjectPropertyUInt64 :: (ManagedPtr a, GObject a) =>
                          a -> String -> Word64 -> IO ()
setObjectPropertyUInt64 obj propName uint64 =
    setObjectProperty obj propName uint64 set_uint64 #const G_TYPE_UINT64

constructObjectPropertyUInt64 :: String -> Word64 ->
                                 IO (String, GValue)
constructObjectPropertyUInt64 propName uint64 =
    constructObjectProperty propName uint64 set_uint64 #const G_TYPE_UINT64

getObjectPropertyUInt64 :: (ManagedPtr a, GObject a) => a -> String -> IO Word64
getObjectPropertyUInt64 obj propName =
    getObjectProperty obj propName get_uint64 #const G_TYPE_UINT64

setObjectPropertyFloat :: (ManagedPtr a, GObject a) =>
                           a -> String -> Float -> IO ()
setObjectPropertyFloat obj propName float =
    setObjectProperty obj propName float set_float #const G_TYPE_FLOAT

constructObjectPropertyFloat :: String -> Float ->
                                 IO (String, GValue)
constructObjectPropertyFloat propName float =
    constructObjectProperty propName float set_float #const G_TYPE_FLOAT

getObjectPropertyFloat :: (ManagedPtr a, GObject a) =>
                           a -> String -> IO Float
getObjectPropertyFloat obj propName =
    getObjectProperty obj propName get_float #const G_TYPE_FLOAT

setObjectPropertyDouble :: (ManagedPtr a, GObject a) =>
                            a -> String -> Double -> IO ()
setObjectPropertyDouble obj propName double =
    setObjectProperty obj propName double set_double #const G_TYPE_DOUBLE

constructObjectPropertyDouble :: String -> Double ->
                                  IO (String, GValue)
constructObjectPropertyDouble propName double =
    constructObjectProperty propName double set_double #const G_TYPE_DOUBLE

getObjectPropertyDouble :: (ManagedPtr a, GObject a) =>
                            a -> String -> IO Double
getObjectPropertyDouble obj propName =
    getObjectProperty obj propName get_double #const G_TYPE_DOUBLE

setObjectPropertyBool :: (ManagedPtr a, GObject a) =>
                         a -> String -> Bool -> IO ()
setObjectPropertyBool obj propName bool =
    setObjectProperty obj propName bool set_boolean #const G_TYPE_BOOLEAN

constructObjectPropertyBool :: String -> Bool -> IO (String, GValue)
constructObjectPropertyBool propName bool =
    constructObjectProperty propName bool set_boolean #const G_TYPE_BOOLEAN

getObjectPropertyBool :: (ManagedPtr a, GObject a) => a -> String -> IO Bool
getObjectPropertyBool obj propName =
    getObjectProperty obj propName get_boolean #const G_TYPE_BOOLEAN

setObjectPropertyObject :: (ManagedPtr a, GObject a,
                            ManagedPtr b, GObject b) =>
                           a -> String -> b -> IO ()
setObjectPropertyObject obj propName object = do
  gtype <- gobjectType object
  withManagedPtr object $ \objectPtr ->
      setObjectProperty obj propName objectPtr set_object gtype

constructObjectPropertyObject :: (ManagedPtr a, GObject a) =>
                                 String -> a -> IO (String, GValue)
constructObjectPropertyObject propName object = do
  gtype <- gobjectType object
  withManagedPtr object $ \objectPtr ->
      constructObjectProperty propName objectPtr set_object gtype

getObjectPropertyObject :: forall a b. (ManagedPtr a, GObject a, GObject b) =>
                           a -> String -> (ForeignPtr b -> b) -> IO b
getObjectPropertyObject obj propName constructor = do
  gtype <- gobjectType (undefined :: b)
  getObjectProperty obj propName
                        (\val -> (get_object val :: IO (Ptr b))
                                 >>= newObject constructor)
                      gtype

setObjectPropertyBoxed :: (ManagedPtr a, GObject a, ManagedPtr b, BoxedObject b) =>
                          a -> String -> b -> IO ()
setObjectPropertyBoxed obj propName boxed = do
  gtype <- boxedType boxed
  withManagedPtr boxed $ \boxedPtr ->
        setObjectProperty obj propName boxedPtr set_boxed gtype

constructObjectPropertyBoxed :: (ManagedPtr a, BoxedObject a) => String -> a ->
                                IO (String, GValue)
constructObjectPropertyBoxed propName boxed = do
  gtype <- boxedType boxed
  withManagedPtr boxed $ \boxedPtr ->
      constructObjectProperty propName boxedPtr set_boxed gtype

getObjectPropertyBoxed :: forall a b. (ManagedPtr a, GObject a, BoxedObject b) =>
                          a -> String -> (ForeignPtr b -> b) -> IO b
getObjectPropertyBoxed obj propName constructor = do
  gtype <- boxedType (undefined :: b)
  getObjectProperty obj propName (get_boxed >=> newBoxed constructor) gtype

setObjectPropertyStringArray :: (ManagedPtr a, GObject a) =>
                                a -> String -> [String] -> IO ()
setObjectPropertyStringArray obj propName strv = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  setObjectProperty obj propName cStrv set_boxed #const G_TYPE_STRV
  mapZeroTerminatedCArray free cStrv
  free cStrv

constructObjectPropertyStringArray :: String -> [String] ->
                                      IO (String, GValue)
constructObjectPropertyStringArray propName strv = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  result <- constructObjectProperty propName cStrv set_boxed #const G_TYPE_STRV
  mapZeroTerminatedCArray free cStrv
  free cStrv
  return result

getObjectPropertyStringArray :: (ManagedPtr a, GObject a) =>
                                a -> String -> IO [String]
getObjectPropertyStringArray obj propName =
    getObjectProperty obj propName
                      (get_boxed >=> unpackZeroTerminatedUTF8CArray . castPtr)
                      #const G_TYPE_STRV

setObjectPropertyEnum :: (ManagedPtr a, GObject a, Enum b, BoxedObject b) =>
                         a -> String -> b -> IO ()
setObjectPropertyEnum obj propName enum = do
  gtype <- boxedType enum
  let cEnum = (fromIntegral . fromEnum) enum
  setObjectProperty obj propName cEnum set_enum gtype

constructObjectPropertyEnum :: (Enum a, BoxedObject a) =>
                               String -> a -> IO (String, GValue)
constructObjectPropertyEnum propName enum = do
  gtype <- boxedType enum
  let cEnum = (fromIntegral . fromEnum) enum
  constructObjectProperty propName cEnum set_enum gtype

getObjectPropertyEnum :: forall a b. (ManagedPtr a, GObject a,
                                      Enum b, BoxedObject b) =>
                         a -> String -> IO b
getObjectPropertyEnum obj propName = do
  gtype <- boxedType (undefined :: b)
  getObjectProperty obj propName
                    (\val -> toEnum . fromIntegral <$> get_enum val)
                    gtype

setObjectPropertyFlags :: (IsGFlag b, ManagedPtr a, GObject a) =>
                          a -> String -> [b] -> IO ()
setObjectPropertyFlags obj propName flags =
    let cFlags = gflagsToWord flags
    in setObjectProperty obj propName cFlags set_flags #const G_TYPE_FLAGS

constructObjectPropertyFlags :: IsGFlag a => String -> [a] ->
                                IO (String, GValue)
constructObjectPropertyFlags propName flags =
    let cFlags = gflagsToWord flags
    in constructObjectProperty propName cFlags set_flags #const G_TYPE_FLAGS

getObjectPropertyFlags :: (ManagedPtr a, GObject a, IsGFlag b) =>
                          a -> String -> IO [b]
getObjectPropertyFlags obj propName =
    getObjectProperty obj propName
                          (\val -> wordToGFlags <$> get_flags val)
                          #const G_TYPE_FLAGS

setObjectPropertyVariant :: (ManagedPtr a, GObject a) =>
                            a -> String -> b -> IO ()
setObjectPropertyVariant = error $ "Setting variant types not supported yet."

constructObjectPropertyVariant :: String -> b -> IO (String, GValue)
constructObjectPropertyVariant =
    error $ "Constructing variant types not supported yet."

getObjectPropertyVariant :: (ManagedPtr a, GObject a) => a -> String -> IO b
getObjectPropertyVariant = error $ "Getting variant types not supported yet."

setObjectPropertyByteArray :: (ManagedPtr a, GObject a) =>
                              a -> String -> B.ByteString -> IO ()
setObjectPropertyByteArray obj propName bytes = do
  packed <- packGByteArray bytes
  setObjectProperty obj propName packed set_boxed #const G_TYPE_BYTE_ARRAY
  unrefGByteArray packed

constructObjectPropertyByteArray :: String -> B.ByteString ->
                                    IO (String, GValue)
constructObjectPropertyByteArray propName bytes = do
  packed <- packGByteArray bytes
  result <- constructObjectProperty propName packed
            set_boxed #const G_TYPE_BYTE_ARRAY
  unrefGByteArray packed
  return result

getObjectPropertyByteArray :: (ManagedPtr a, GObject a) =>
                              a -> String -> IO B.ByteString
getObjectPropertyByteArray obj propName =
    getObjectProperty obj propName (get_boxed >=> unpackGByteArray)
                      #const G_TYPE_BYTE_ARRAY

setObjectPropertyHash :: (ManagedPtr a, GObject a) => a -> String -> b -> IO ()
setObjectPropertyHash =
    error $ "Setting GHashTable properties not supported yet."

constructObjectPropertyHash :: String -> b -> IO (String, GValue)
constructObjectPropertyHash =
    error $ "Constructing GHashTable properties not supported yet."

getObjectPropertyHash :: (ManagedPtr a, GObject a) => a -> String -> IO b
getObjectPropertyHash =
    error $ "Getting GHashTable properties not supported yet."
