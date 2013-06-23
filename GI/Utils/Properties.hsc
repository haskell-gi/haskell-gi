{-# LANGUAGE ScopedTypeVariables, GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module GI.Utils.Properties
    ( new

    , setObjectPropertyString
    , setObjectPropertyStringArray
    , setObjectPropertyPtr
    , setObjectPropertyCInt
    , setObjectPropertyCUInt
    , setObjectPropertyCFloat
    , setObjectPropertyCDouble
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
    , getObjectPropertyCFloat
    , getObjectPropertyCDouble
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
    , constructObjectPropertyCFloat
    , constructObjectPropertyCDouble
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
import GI.Utils.ManagedPtr
import GI.Utils.Attributes

import Foreign.Safe hiding (new)
import Foreign.C

#include <glib-object.h>

foreign import ccall unsafe "g_object_newv" g_object_newv ::
    GType -> CUInt -> Ptr a -> IO (Ptr b)

-- | Construct a GObject given the constructor and a list of settable
-- attributes. AttrOps are always constructible, so we don't need to
-- enforce constraints here.
new :: forall o. GObject o => (ForeignPtr o -> o) ->
       [AttrOp JustSetsAttr o NonWritableAttr] -> IO o
new constructor attrs = do
  props <- mapM construct attrs
  let nprops = length props
  params <- mallocBytes (nprops*gparameterSize)
  fill params props
  gtype <- gobjectType (undefined :: o)
  result <- g_object_newv gtype (fromIntegral nprops) params
  freeStrings nprops params
  free params
  mapM_ (freeGValue . snd) props
  wrapObject constructor (result :: Ptr o)
  where
    construct :: AttrOp JustSetsAttr o NonWritableAttr ->
                 IO (String, GValuePtr)
    construct (RWAttr _ _ _ cons :=  x) = cons x
    construct (RCAttr _ _   cons :=  x) = cons x
    construct (WOAttr _   _ cons :=  x) = cons x
    construct (COAttr _     cons :=  x) = cons x
    construct (RWAttr _ _ _ cons :=> x) = x >>= cons
    construct (RCAttr _ _   cons :=> x) = x >>= cons
    construct (WOAttr _   _ cons :=> x) = x >>= cons
    construct (COAttr _     cons :=> x) = x >>= cons

    gvalueSize = #size GValue
    gparameterSize = #size GParameter

    -- Fill the given memory address with the contents of the array of
    -- GParameters.
    fill :: Ptr () -> [(String, GValuePtr)] -> IO ()
    fill _ [] = return ()
    fill dataPtr ((str, gvalue):xs) =
        do cstr <- newCString str
           poke (castPtr dataPtr) cstr
           copyBytes (dataPtr `plusPtr` sizeOf nullPtr) gvalue gvalueSize
           fill (dataPtr `plusPtr` gparameterSize) xs

    -- Free the strings in the GParameter array (the GValues will be
    -- freed separately).
    freeStrings :: Int -> Ptr () -> IO ()
    freeStrings 0 _ = return ()
    freeStrings n dataPtr =
        do cstr <- peek (castPtr dataPtr) :: IO CString
           free cstr
           freeStrings (n-1) (dataPtr `plusPtr` gparameterSize)

foreign import ccall unsafe "alloc_new_GValue" alloc_new_GValue ::
    GType -> IO GValuePtr

foreign import ccall unsafe "g_value_unset" g_value_unset ::
    GValuePtr -> IO ()

foreign import ccall unsafe "g_object_set_property" g_object_set_property ::
    Ptr a -> CString -> GValuePtr -> IO ()

freeGValue :: GValuePtr -> IO ()
freeGValue gvalue = do
  g_value_unset gvalue
  free gvalue

setObjectProperty :: (ManagedPtr a, GObject a) => a -> String -> b ->
                     (GValuePtr -> b -> IO ()) -> GType -> IO ()
setObjectProperty obj propName propValue setter gtype = do
  gvalue <- alloc_new_GValue gtype
  setter gvalue propValue
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          g_object_set_property objPtr cPropName gvalue
  freeGValue gvalue

foreign import ccall unsafe "g_object_get_property" g_object_get_property ::
    Ptr a -> CString -> GValuePtr -> IO ()

getObjectProperty :: (GObject a, ManagedPtr a) => a -> String ->
                     (GValuePtr -> IO b) -> GType -> IO b
getObjectProperty obj propName getter gtype = do
  gvalue <- alloc_new_GValue gtype
  withManagedPtr obj $ \objPtr ->
      withCString propName $ \cPropName ->
          g_object_get_property objPtr cPropName gvalue
  result <- getter gvalue
  freeGValue gvalue
  return result  

constructObjectProperty :: String -> b -> (GValuePtr -> b -> IO ()) ->
                           GType -> IO (String, GValuePtr)
constructObjectProperty propName propValue setter gtype = do
  gvalue <- alloc_new_GValue gtype
  setter gvalue propValue
  return (propName, gvalue)

foreign import ccall unsafe "g_value_set_string" set_string ::
    GValuePtr -> CString -> IO ()
setObjectPropertyString :: (GObject a, ManagedPtr a) =>
                           a -> String -> String -> IO ()
setObjectPropertyString obj propName str =
    withCString str $ \cstr ->
        setObjectProperty obj propName cstr set_string #const G_TYPE_STRING

constructObjectPropertyString :: String -> String ->
                                 IO (String, GValuePtr)
constructObjectPropertyString propName str =
    withCString str $ \cstr ->
        constructObjectProperty propName cstr set_string #const G_TYPE_STRING

foreign import ccall unsafe "g_value_get_string" get_string ::
    GValuePtr -> IO CString
getObjectPropertyString :: (ManagedPtr a, GObject a) =>
                           a -> String -> IO String
getObjectPropertyString obj propName =
    getObjectProperty obj propName (get_string >=> peekCString)
                          #const G_TYPE_STRING

foreign import ccall unsafe "g_value_set_pointer" set_pointer ::
    GValuePtr -> Ptr a -> IO ()
setObjectPropertyPtr :: (ManagedPtr a, GObject a) =>
                        a -> String -> Ptr b -> IO ()
setObjectPropertyPtr obj propName ptr =
    setObjectProperty obj propName ptr set_pointer #const G_TYPE_POINTER

constructObjectPropertyPtr :: String -> Ptr b ->
                              IO (String, GValuePtr)
constructObjectPropertyPtr propName ptr =
    constructObjectProperty propName ptr set_pointer #const G_TYPE_POINTER

foreign import ccall unsafe "g_value_get_pointer" get_pointer ::
    GValuePtr -> IO (Ptr b)
getObjectPropertyPtr :: (ManagedPtr a, GObject a) =>
                        a -> String -> IO (Ptr b)
getObjectPropertyPtr obj propName =
    getObjectProperty obj propName get_pointer #const G_TYPE_POINTER

foreign import ccall unsafe "g_value_set_int" set_int ::
    GValuePtr -> Int32 -> IO ()
setObjectPropertyCInt :: (ManagedPtr a, GObject a) =>
                         a -> String -> Int32 -> IO ()
setObjectPropertyCInt obj propName int =
    setObjectProperty obj propName int set_int #const G_TYPE_INT

constructObjectPropertyCInt :: String -> Int32 ->
                                IO (String, GValuePtr)
constructObjectPropertyCInt propName int =
    constructObjectProperty propName int set_int #const G_TYPE_INT

foreign import ccall unsafe "g_value_get_int" get_int ::
    GValuePtr -> IO Int32
getObjectPropertyCInt :: (ManagedPtr a, GObject a) => a -> String -> IO Int32
getObjectPropertyCInt obj propName =
    getObjectProperty obj propName get_int #const G_TYPE_INT

foreign import ccall unsafe "g_value_set_uint" set_uint ::
    GValuePtr -> Word32 -> IO ()
setObjectPropertyCUInt :: (ManagedPtr a, GObject a) =>
                          a -> String -> Word32 -> IO ()
setObjectPropertyCUInt obj propName uint =
    setObjectProperty obj propName uint set_uint #const G_TYPE_UINT

constructObjectPropertyCUInt :: String -> Word32 ->
                                IO (String, GValuePtr)
constructObjectPropertyCUInt propName uint =
    constructObjectProperty propName uint set_uint #const G_TYPE_UINT

foreign import ccall unsafe "g_value_get_uint" get_uint ::
    GValuePtr -> IO Word32
getObjectPropertyCUInt :: (ManagedPtr a, GObject a) => a -> String -> IO Word32
getObjectPropertyCUInt obj propName =
    getObjectProperty obj propName get_uint #const G_TYPE_UINT

foreign import ccall unsafe "g_value_set_float" set_float ::
    GValuePtr -> CFloat -> IO ()
setObjectPropertyCFloat :: (ManagedPtr a, GObject a) =>
                           a -> String -> CFloat -> IO ()
setObjectPropertyCFloat obj propName float =
    setObjectProperty obj propName float set_float #const G_TYPE_FLOAT

constructObjectPropertyCFloat :: String -> CFloat ->
                                 IO (String, GValuePtr)
constructObjectPropertyCFloat propName float =
    constructObjectProperty propName float set_float #const G_TYPE_FLOAT

foreign import ccall unsafe "g_value_get_float" get_float ::
    GValuePtr -> IO CFloat
getObjectPropertyCFloat :: (ManagedPtr a, GObject a) =>
                           a -> String -> IO CFloat
getObjectPropertyCFloat obj propName =
    getObjectProperty obj propName get_float #const G_TYPE_FLOAT

foreign import ccall unsafe "g_value_set_double" set_double ::
    GValuePtr -> CDouble -> IO ()
setObjectPropertyCDouble :: (ManagedPtr a, GObject a) =>
                            a -> String -> CDouble -> IO ()
setObjectPropertyCDouble obj propName double =
    setObjectProperty obj propName double set_double #const G_TYPE_DOUBLE

constructObjectPropertyCDouble :: String -> CDouble ->
                                  IO (String, GValuePtr)
constructObjectPropertyCDouble propName double =
    constructObjectProperty propName double set_double #const G_TYPE_DOUBLE

foreign import ccall unsafe "g_value_get_double" get_double ::
    GValuePtr -> IO CDouble
getObjectPropertyCDouble :: (ManagedPtr a, GObject a) =>
                            a -> String -> IO CDouble
getObjectPropertyCDouble obj propName =
    getObjectProperty obj propName get_double #const G_TYPE_DOUBLE

foreign import ccall unsafe "g_value_set_boolean" set_boolean ::
    GValuePtr -> CInt -> IO ()
setObjectPropertyBool :: (ManagedPtr a, GObject a) =>
                         a -> String -> Bool -> IO ()
setObjectPropertyBool obj propName bool =
    let cBool = (fromIntegral . fromEnum) bool
    in setObjectProperty obj propName cBool set_boolean #const G_TYPE_BOOLEAN

constructObjectPropertyBool :: String -> Bool -> IO (String, GValuePtr)
constructObjectPropertyBool propName bool =
    let cBool = (fromIntegral . fromEnum) bool
    in constructObjectProperty propName cBool set_boolean #const G_TYPE_BOOLEAN

foreign import ccall unsafe "g_value_get_boolean" get_boolean ::
    GValuePtr -> IO CInt
getObjectPropertyBool :: (ManagedPtr a, GObject a) => a -> String -> IO Bool
getObjectPropertyBool obj propName =
    getObjectProperty obj propName (\val -> (/= 0) <$> get_boolean val)
                          #const G_TYPE_BOOLEAN

foreign import ccall unsafe "g_value_set_object" set_object ::
    GValuePtr -> Ptr a -> IO ()
setObjectPropertyObject :: (ManagedPtr a, GObject a,
                            ManagedPtr b, GObject b) =>
                           a -> String -> b -> IO ()
setObjectPropertyObject obj propName object =
    withManagedPtr object $ \objectPtr ->
        setObjectProperty obj propName objectPtr set_object #const G_TYPE_OBJECT

constructObjectPropertyObject :: (ManagedPtr a, GObject a) =>
                                 String -> a -> IO (String, GValuePtr)
constructObjectPropertyObject propName object =
    withManagedPtr object $ \objectPtr ->
        constructObjectProperty propName objectPtr set_object #const G_TYPE_OBJECT

foreign import ccall unsafe "g_value_get_object" get_object ::
    GValuePtr -> IO (Ptr a)
getObjectPropertyObject :: forall a b. (ManagedPtr a, GObject a, GObject b) =>
                           a -> String -> (ForeignPtr b -> b) -> IO b
getObjectPropertyObject obj propName constructor =
    getObjectProperty obj propName
                      (\val -> (get_object val :: IO (Ptr b))
                               >>= newObject constructor)
                      #const G_TYPE_OBJECT

foreign import ccall unsafe "g_value_set_boxed" set_boxed ::
    GValuePtr -> Ptr a -> IO ()
setObjectPropertyBoxed :: (ManagedPtr a, GObject a, ManagedPtr b) =>
                          a -> String -> b -> IO ()
setObjectPropertyBoxed obj propName boxed =
    withManagedPtr boxed $ \boxedPtr ->
        setObjectProperty obj propName boxedPtr set_boxed #const G_TYPE_BOXED

constructObjectPropertyBoxed :: ManagedPtr a => String -> a ->
                                IO (String, GValuePtr)
constructObjectPropertyBoxed propName boxed =
    withManagedPtr boxed $ \boxedPtr ->
        constructObjectProperty propName boxedPtr set_boxed #const G_TYPE_BOXED

foreign import ccall unsafe "g_value_get_boxed" get_boxed ::
    GValuePtr -> IO (Ptr b)
getObjectPropertyBoxed :: (ManagedPtr a, GObject a, BoxedObject b) =>
                          a -> String -> (ForeignPtr b -> b) -> IO b
getObjectPropertyBoxed obj propName constructor =
    getObjectProperty obj propName (get_boxed >=> newBoxed constructor)
                          #const G_TYPE_BOXED

setObjectPropertyStringArray :: (ManagedPtr a, GObject a) =>
                                a -> String -> [String] -> IO ()
setObjectPropertyStringArray obj propName strv = do
  cStrv <- packZeroTerminatedUTF8CArray strv
  setObjectProperty obj propName cStrv set_boxed #const G_TYPE_STRV
  mapZeroTerminatedCArray free cStrv
  free cStrv

constructObjectPropertyStringArray :: String -> [String] ->
                                      IO (String, GValuePtr)
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

foreign import ccall unsafe "g_value_set_enum" set_enum ::
    GValuePtr -> CUInt -> IO ()
setObjectPropertyEnum :: (ManagedPtr a, GObject a, Enum b, BoxedObject b) =>
                         a -> String -> b -> IO ()
setObjectPropertyEnum obj propName enum = do
  gtype <- boxedType enum
  let cEnum = (fromIntegral . fromEnum) enum
  setObjectProperty obj propName cEnum set_enum gtype

constructObjectPropertyEnum :: (Enum a, BoxedObject a) =>
                               String -> a -> IO (String, GValuePtr)
constructObjectPropertyEnum propName enum = do
  gtype <- boxedType enum
  let cEnum = (fromIntegral . fromEnum) enum
  constructObjectProperty propName cEnum set_enum gtype

foreign import ccall unsafe "g_value_get_enum" get_enum ::
    GValuePtr -> IO CUInt
getObjectPropertyEnum :: forall a b. (ManagedPtr a, GObject a,
                                      Enum b, BoxedObject b) =>
                         a -> String -> IO b
getObjectPropertyEnum obj propName = do
  gtype <- boxedType (undefined :: b)
  getObjectProperty obj propName
                    (\val -> toEnum . fromIntegral <$> get_enum val)
                    gtype

foreign import ccall unsafe "g_value_set_flags" set_flags ::
    GValuePtr -> CUInt -> IO ()
setObjectPropertyFlags :: (ManagedPtr a, GObject a) =>
                          a -> String -> Word -> IO ()
setObjectPropertyFlags obj propName flags =
    let cFlags = fromIntegral flags
    in setObjectProperty obj propName cFlags set_flags #const G_TYPE_FLAGS

constructObjectPropertyFlags :: String -> Word ->
                                IO (String, GValuePtr)
constructObjectPropertyFlags propName flags =
    let cFlags = fromIntegral flags
    in constructObjectProperty propName cFlags set_flags #const G_TYPE_FLAGS

foreign import ccall unsafe "g_value_get_flags" get_flags ::
    GValuePtr -> IO CUInt
getObjectPropertyFlags :: (ManagedPtr a, GObject a) => a -> String -> IO Word
getObjectPropertyFlags obj propName =
    getObjectProperty obj propName (\val -> fromIntegral <$> get_flags val)
                          #const G_TYPE_FLAGS

setObjectPropertyVariant :: (ManagedPtr a, GObject a) =>
                            a -> String -> b -> IO ()
setObjectPropertyVariant = error $ "Setting variant types not supported yet."

constructObjectPropertyVariant :: String -> b -> IO (String, GValuePtr)
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
                                    IO (String, GValuePtr)
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

constructObjectPropertyHash :: String -> b -> IO (String, GValuePtr)
constructObjectPropertyHash =
    error $ "Constructing GHashTable properties not supported yet."

getObjectPropertyHash :: (ManagedPtr a, GObject a) => a -> String -> IO b
getObjectPropertyHash =
    error $ "Getting GHashTable properties not supported yet."
