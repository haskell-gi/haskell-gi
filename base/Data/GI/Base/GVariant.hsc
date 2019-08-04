{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-|
This module contains some helper functions for dealing with GVariant
values. The simplest way of dealing with them is by using the
'IsGVariant' typeclass:

> str <- fromGVariant variant :: IO (Maybe Text)

assuming that the variant is expected to contain a
string in UTF8 encoding. The code becomes even shorter if the type
checker can determine the return type for you:


> readStringVariant :: GVariant -> IO Text
> readStringVariant variant =
>   fromGVariant variant >>= \case
>      Nothing  -> error "Variant was not a string"
>      Just str -> return str

Alternatively, you can use manually the gvariantFrom* and
gvariantTo* family of functions.
-}
module Data.GI.Base.GVariant
    ( IsGVariant(..)
    , IsGVariantBasicType

    , noGVariant

    , gvariantGetTypeString

    -- * Type wrappers
    -- | Some 'GVariant' types are isomorphic to Haskell types, but they
    -- carry some extra information. For example, there is a tuple
    -- singlet type, which is isomorphic to a single Haskell value
    -- with the added bit of information that it is wrapped in a tuple
    -- container. In order to use these values you can use the
    -- following wrappers, which allow the 'IsGVariant' instance to
    -- disambiguate the requested type properly.

    , GVariantSinglet(GVariantSinglet)
    , GVariantDictEntry(GVariantDictEntry)
    , GVariantHandle(GVariantHandle)
    , GVariantObjectPath
    , newGVariantObjectPath
    , gvariantObjectPathToText
    , GVariantSignature
    , newGVariantSignature
    , gvariantSignatureToText

    -- * Manual memory management

    , wrapGVariantPtr
    , newGVariantFromPtr
    , unrefGVariant
    , disownGVariant

    -- * Manual conversions

    -- ** Basic types
    --
    -- | The use of these should be fairly self-explanatory. If you
    -- want to convert a Haskell type into a 'GVariant', use
    -- gvariantTo*. If you want to convert a 'GVariant' into a Haskell
    -- type, use gvariantFrom*. The conversion can fail if the
    -- 'GVariant' is not of the expected type (if you want to convert
    -- a 'GVariant' containing a 'Int16' into a 'Text' value, say), in
    -- which case 'Nothing' will be returned.
    , gvariantToBool
    , gvariantFromBool

    , gvariantToWord8
    , gvariantFromWord8

    , gvariantToInt16
    , gvariantFromInt16

    , gvariantToWord16
    , gvariantFromWord16

    , gvariantToInt32
    , gvariantFromInt32

    , gvariantToWord32
    , gvariantFromWord32

    , gvariantToInt64
    , gvariantFromInt64

    , gvariantToWord64
    , gvariantFromWord64

    , gvariantToHandle
    , gvariantFromHandle

    , gvariantToDouble
    , gvariantFromDouble

    , gvariantToText
    , gvariantFromText

    , gvariantToObjectPath
    , gvariantFromObjectPath

    , gvariantToSignature
    , gvariantFromSignature

    -- ** Container type conversions
    , gvariantToGVariant
    , gvariantFromGVariant

    , gvariantToBytestring
    , gvariantFromBytestring

    , gvariantFromMaybe
    , gvariantToMaybe

    , gvariantFromDictEntry
    , gvariantToDictEntry

    , gvariantFromMap
    , gvariantToMap

    , gvariantFromList
    , gvariantToList

    , gvariantFromTuple
    , gvariantToTuple
    ) where

#include <glib-object.h>

import Control.Monad (when, void, (>=>))
import Control.Exception.Base (bracket)

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Data.Int
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M

import System.IO.Unsafe (unsafePerformIO)
import Foreign.C
import Foreign.Ptr

import Data.GI.Base.BasicTypes (GVariant(..))
import Data.GI.Base.BasicConversions
import Data.GI.Base.ManagedPtr (withManagedPtr, withManagedPtrList,
                                newManagedPtr', disownManagedPtr)
import Data.GI.Base.Utils (freeMem)

-- | An alias for @Nothing :: Maybe GVariant@ to save some typing.
noGVariant :: Maybe GVariant
noGVariant = Nothing

-- | The typeclass for types that can be automatically marshalled into
-- 'GVariant' using 'toGVariant' and 'fromGVariant'.
class IsGVariant a where
    -- | Convert a value of the given type into a GVariant.
    toGVariant   :: a -> IO GVariant
    -- | Try to decode a 'GVariant' into a target type. If the
    -- conversion fails we return 'Nothing'. The type that was
    -- expected can be obtained by calling 'toGVariantFormatString',
    -- and the actual type as understood by the 'GVariant' code can be
    -- obtained by calling 'gvariantToTypeString'.
    fromGVariant :: GVariant -> IO (Maybe a)
    -- | The expected format string for this type (the argument is
    -- ignored).
    toGVariantFormatString :: a -> Text

-- Same as fromGVariant, for cases where we have checked that things
-- have the right type in advance.
unsafeFromGVariant :: IsGVariant a => GVariant -> IO a
unsafeFromGVariant gv =
    fromGVariant gv >>= \case
                 Nothing -> error "Error decoding GVariant. This is a bug in haskell-gi, please report it."
                 Just value -> return value

-- | The typeclass for basic type 'GVariant' types, i.e. those that
-- are not containers.
class Ord a => IsGVariantBasicType a

-- | Haskell has no notion of one element tuples, but GVariants do, so
-- the following allows for marshalling one element tuples properly
-- using 'fromGVariant' and 'toGVariant'. For instance, to construct a
-- single element tuple containing a string, you could do
--
-- > toGVariant (GVariantSinglet "Test")
newtype GVariantSinglet a = GVariantSinglet a
    deriving (Eq, Show)

data GVariantType

foreign import ccall "g_variant_type_new" g_variant_type_new ::
    CString -> IO (Ptr GVariantType)

foreign import ccall "g_variant_type_free" g_variant_type_free ::
    Ptr GVariantType -> IO ()

foreign import ccall "g_variant_is_of_type" g_variant_is_of_type ::
    Ptr GVariant -> Ptr GVariantType -> IO #{type gboolean}

withGVariantType :: Text -> (Ptr GVariantType -> IO a) -> IO a
withGVariantType text action = withTextCString text $ \textPtr ->
                               bracket (g_variant_type_new textPtr)
                                       g_variant_type_free
                                       action

gvariantIsOfType :: Text -> GVariant -> IO Bool
gvariantIsOfType typeString variant =
    withGVariantType typeString $
        \typePtr ->
            (toEnum . fromIntegral) <$> withManagedPtr variant
                                        (\vptr -> g_variant_is_of_type
                                                  vptr typePtr)

withExplicitType :: Text -> (Ptr GVariant -> IO a) -> GVariant -> IO (Maybe a)
withExplicitType format action variant = do
  check <- gvariantIsOfType format variant
  if check
  then Just <$> withManagedPtr variant action
  else return Nothing

withTypeCheck :: forall a. (IsGVariant a) =>
                 (Ptr GVariant -> IO a) -> GVariant -> IO (Maybe a)
withTypeCheck = withExplicitType $ toGVariantFormatString (undefined :: a)

foreign import ccall "g_variant_get_type_string" g_variant_get_type_string
    :: Ptr GVariant -> IO CString

-- | Get the expected type of a 'GVariant', in 'GVariant'
-- notation. See
-- <https://developer.gnome.org/glib/stable/glib-GVariantType.html>
-- for the meaning of the resulting format string.
gvariantGetTypeString :: GVariant -> IO Text
gvariantGetTypeString variant =
    withManagedPtr variant (g_variant_get_type_string >=> cstringToText)

foreign import ccall "g_variant_is_floating" g_variant_is_floating ::
    Ptr GVariant -> IO CInt
foreign import ccall "g_variant_ref_sink" g_variant_ref_sink ::
    Ptr GVariant -> IO (Ptr GVariant)
foreign import ccall "g_variant_ref" g_variant_ref ::
    Ptr GVariant -> IO (Ptr GVariant)
foreign import ccall "g_variant_unref" g_variant_unref ::
    Ptr GVariant -> IO ()
foreign import ccall "&g_variant_unref" ptr_to_g_variant_unref ::
    FunPtr (Ptr GVariant -> IO ())

-- | Take ownership of a passed in 'Ptr' (typically created just for
-- us, so if it is floating we sink it).
wrapGVariantPtr :: Ptr GVariant -> IO GVariant
wrapGVariantPtr ptr = do
  floating <- g_variant_is_floating ptr
  when (floating /= 0) $ void $ g_variant_ref_sink ptr
  fPtr <- newManagedPtr' ptr_to_g_variant_unref ptr
  return $! GVariant fPtr

-- | Construct a Haskell wrapper for the given 'GVariant', without
-- assuming ownership.
newGVariantFromPtr :: Ptr GVariant -> IO GVariant
newGVariantFromPtr ptr = do
  fPtr <- g_variant_ref ptr >>= newManagedPtr' ptr_to_g_variant_unref
  return $! GVariant fPtr

-- | Remove a reference to the given 'GVariant'.
unrefGVariant :: GVariant -> IO ()
unrefGVariant gv = withManagedPtr gv g_variant_unref

-- | Disown a `GVariant`, i.e. do not unref the underlying object when
-- the Haskell object is garbage collected.
disownGVariant :: GVariant -> IO (Ptr GVariant)
disownGVariant = disownManagedPtr

instance IsGVariant Bool where
    toGVariant = gvariantFromBool
    fromGVariant = gvariantToBool
    toGVariantFormatString _ = "b"
instance IsGVariantBasicType Bool

foreign import ccall "g_variant_new_boolean" new_bool
    :: #{type gboolean} -> IO (Ptr GVariant)

gvariantFromBool :: Bool -> IO GVariant
gvariantFromBool = (new_bool . fromIntegral . fromEnum) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_boolean" get_bool
    :: Ptr GVariant -> IO #{type gboolean}

gvariantToBool :: GVariant -> IO (Maybe Bool)
gvariantToBool = withTypeCheck $ get_bool >=> (return . toEnum . fromIntegral)

instance IsGVariant Word8 where
    toGVariant = gvariantFromWord8
    fromGVariant = gvariantToWord8
    toGVariantFormatString _ = "y"
instance IsGVariantBasicType Word8

foreign import ccall "g_variant_new_byte" new_byte
    :: #{type guchar} -> IO (Ptr GVariant)

gvariantFromWord8 :: Word8 -> IO GVariant
gvariantFromWord8 = (new_byte . fromIntegral) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_byte" get_byte
    :: Ptr GVariant -> IO #{type guchar}

gvariantToWord8 :: GVariant -> IO (Maybe Word8)
gvariantToWord8 = withTypeCheck $ get_byte >=> (return . fromIntegral)

instance IsGVariant Int16 where
    toGVariant = gvariantFromInt16
    fromGVariant = gvariantToInt16
    toGVariantFormatString _ = "n"
instance IsGVariantBasicType Int16

foreign import ccall "g_variant_new_int16" new_int16
    :: #{type gint16} -> IO (Ptr GVariant)

gvariantFromInt16 :: Int16 -> IO GVariant
gvariantFromInt16 = (new_int16 . fromIntegral) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_int16" get_int16
    :: Ptr GVariant -> IO #{type gint16}

gvariantToInt16 :: GVariant -> IO (Maybe Int16)
gvariantToInt16 = withTypeCheck $ get_int16 >=> (return . fromIntegral)

instance IsGVariant Word16 where
    toGVariant = gvariantFromWord16
    fromGVariant = gvariantToWord16
    toGVariantFormatString _ = "q"
instance IsGVariantBasicType Word16

foreign import ccall "g_variant_new_uint16" new_uint16
    :: #{type guint16} -> IO (Ptr GVariant)

gvariantFromWord16 :: Word16 -> IO GVariant
gvariantFromWord16 = new_uint16 . fromIntegral >=> wrapGVariantPtr

foreign import ccall "g_variant_get_uint16" get_uint16
    :: Ptr GVariant -> IO #{type guint16}

gvariantToWord16 :: GVariant -> IO (Maybe Word16)
gvariantToWord16 = withTypeCheck $ get_uint16 >=> (return . fromIntegral)

instance IsGVariant Int32 where
    toGVariant = gvariantFromInt32
    fromGVariant = gvariantToInt32
    toGVariantFormatString _ = "i"
instance IsGVariantBasicType Int32

foreign import ccall "g_variant_new_int32" new_int32
    :: #{type gint16} -> IO (Ptr GVariant)

gvariantFromInt32 :: Int32 -> IO GVariant
gvariantFromInt32 = (new_int32 . fromIntegral) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_int32" get_int32
    :: Ptr GVariant -> IO #{type gint32}

gvariantToInt32 :: GVariant -> IO (Maybe Int32)
gvariantToInt32 = withTypeCheck $ get_int32 >=> (return . fromIntegral)

instance IsGVariant Word32 where
    toGVariant = gvariantFromWord32
    fromGVariant = gvariantToWord32
    toGVariantFormatString _ = "u"
instance IsGVariantBasicType Word32

foreign import ccall "g_variant_new_uint32" new_uint32
    :: #{type guint32} -> IO (Ptr GVariant)

gvariantFromWord32 :: Word32 -> IO GVariant
gvariantFromWord32 = (new_uint32 . fromIntegral) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_uint32" get_uint32
    :: Ptr GVariant -> IO #{type guint32}

gvariantToWord32 :: GVariant -> IO (Maybe Word32)
gvariantToWord32 = withTypeCheck $ get_uint32 >=> (return . fromIntegral)

instance IsGVariant Int64 where
    toGVariant = gvariantFromInt64
    fromGVariant = gvariantToInt64
    toGVariantFormatString _ = "x"
instance IsGVariantBasicType Int64

foreign import ccall "g_variant_new_int64" new_int64
    :: #{type gint64} -> IO (Ptr GVariant)

gvariantFromInt64 :: Int64 -> IO GVariant
gvariantFromInt64 = (new_int64 . fromIntegral) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_int64" get_int64
    :: Ptr GVariant -> IO #{type gint64}

gvariantToInt64 :: GVariant -> IO (Maybe Int64)
gvariantToInt64 = withTypeCheck $ get_int64 >=> (return . fromIntegral)

instance IsGVariant Word64 where
    toGVariant = gvariantFromWord64
    fromGVariant = gvariantToWord64
    toGVariantFormatString _ = "t"
instance IsGVariantBasicType Word64

foreign import ccall "g_variant_new_uint64" new_uint64
    :: #{type guint64} -> IO (Ptr GVariant)

gvariantFromWord64 :: Word64 -> IO GVariant
gvariantFromWord64 = (new_uint64 . fromIntegral) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_uint64" get_uint64
    :: Ptr GVariant -> IO #{type guint64}

gvariantToWord64 :: GVariant -> IO (Maybe Word64)
gvariantToWord64 = withTypeCheck $ get_uint64 >=> (return . fromIntegral)

newtype GVariantHandle = GVariantHandle Int32
    deriving (Eq, Ord, Show)

instance IsGVariant GVariantHandle where
    toGVariant (GVariantHandle h) = gvariantFromHandle h
    fromGVariant = gvariantToHandle >=> (return . (GVariantHandle <$>))
    toGVariantFormatString _ = "h"
instance IsGVariantBasicType GVariantHandle

foreign import ccall "g_variant_new_handle" new_handle
    :: #{type gint32} -> IO (Ptr GVariant)

-- | Convert a DBus handle (an 'Int32') into a 'GVariant'.
gvariantFromHandle :: Int32 -> IO GVariant
gvariantFromHandle h = (new_handle . fromIntegral) h >>= wrapGVariantPtr

foreign import ccall "g_variant_get_handle" get_handle
    :: Ptr GVariant -> IO #{type gint32}

-- | Extract the DBus handle (an 'Int32') inside a 'GVariant'.
gvariantToHandle :: GVariant -> IO (Maybe Int32)
gvariantToHandle =
  withExplicitType (toGVariantFormatString (undefined :: GVariantHandle)) $
                   get_handle >=> (return . fromIntegral)

instance IsGVariant Double where
    toGVariant = gvariantFromDouble
    fromGVariant = gvariantToDouble
    toGVariantFormatString _ = "d"
instance IsGVariantBasicType Double

foreign import ccall "g_variant_new_double" new_double
    :: #{type gdouble} -> IO (Ptr GVariant)

gvariantFromDouble :: Double -> IO GVariant
gvariantFromDouble = (new_double . realToFrac) >=> wrapGVariantPtr

foreign import ccall "g_variant_get_double" get_double
    :: Ptr GVariant -> IO #{type gdouble}

gvariantToDouble :: GVariant -> IO (Maybe Double)
gvariantToDouble = withTypeCheck $ get_double >=> (return . realToFrac)

instance IsGVariant Text where
    toGVariant = gvariantFromText
    fromGVariant = gvariantToText
    toGVariantFormatString _ = "s"
instance IsGVariantBasicType Text

foreign import ccall "g_variant_get_string" _get_string
    :: Ptr GVariant -> Ptr #{type gsize} -> IO CString

get_string :: Ptr GVariant -> IO CString
get_string v = _get_string v nullPtr

-- | Decode an UTF-8 encoded string 'GVariant' into 'Text'.
gvariantToText :: GVariant -> IO (Maybe Text)
gvariantToText = withTypeCheck $ get_string >=> cstringToText

foreign import ccall "g_variant_new_take_string" take_string
    :: CString -> IO (Ptr GVariant)

-- | Encode a 'Text' into an UTF-8 encoded string 'GVariant'.
gvariantFromText :: Text -> IO GVariant
gvariantFromText = textToCString >=> take_string >=> wrapGVariantPtr

foreign import ccall "g_variant_is_object_path" g_variant_is_object_path ::
    CString -> IO #{type gboolean}

-- | An object representing a DBus object path, which is a particular
-- type of 'GVariant' too. (Just a string with some specific
-- requirements.) In order to construct/deconstruct a
-- 'GVariantObjectPath' one can use 'newGVariantObjectPath'
-- and 'gvariantObjectPathToText'.
newtype GVariantObjectPath = GVariantObjectPath Text
    deriving (Ord, Eq, Show)

-- | Try to construct a DBus object path. If the passed string is not
-- a valid object path 'Nothing' will be returned.
newGVariantObjectPath :: Text -> Maybe GVariantObjectPath
newGVariantObjectPath p = unsafePerformIO $
   withTextCString p $ \cstr -> do
     isObjectPath <- toEnum . fromIntegral <$> g_variant_is_object_path cstr
     if isObjectPath
     then return $ Just (GVariantObjectPath p)
     else return Nothing

-- | Return the 'Text' representation of a 'GVariantObjectPath'.
gvariantObjectPathToText :: GVariantObjectPath -> Text
gvariantObjectPathToText (GVariantObjectPath p) = p

instance IsGVariant GVariantObjectPath where
    toGVariant = gvariantFromObjectPath
    fromGVariant = gvariantToObjectPath >=> return . (GVariantObjectPath <$>)
    toGVariantFormatString _ = "o"
instance IsGVariantBasicType GVariantObjectPath

foreign import ccall "g_variant_new_object_path" new_object_path
    :: CString -> IO (Ptr GVariant)

-- | Construct a 'GVariant' containing an object path. In order to
-- build a 'GVariantObjectPath' value see 'newGVariantObjectPath'.
gvariantFromObjectPath :: GVariantObjectPath -> IO GVariant
gvariantFromObjectPath (GVariantObjectPath p) =
    withTextCString p $ new_object_path >=> wrapGVariantPtr

-- | Extract a 'GVariantObjectPath' from a 'GVariant', represented as
-- its underlying 'Text' representation.
gvariantToObjectPath :: GVariant -> IO (Maybe Text)
gvariantToObjectPath =
    withExplicitType (toGVariantFormatString (undefined :: GVariantObjectPath))
                         (get_string >=> cstringToText)

foreign import ccall "g_variant_is_signature" g_variant_is_signature ::
    CString -> IO #{type gboolean}

-- | An object representing a DBus signature, which is a particular
-- type of 'GVariant' too. (Just a string with some specific
-- requirements.) In order to construct/deconstruct a
-- 'GVariantSignature' one can use 'newGVariantSignature' and
-- 'gvariantSignatureToText'.
newtype GVariantSignature = GVariantSignature Text
    deriving (Ord, Eq, Show)

-- | Try to construct a DBus object path. If the passed string is not
-- a valid DBus signature 'Nothing' will be returned.
newGVariantSignature :: Text -> Maybe GVariantSignature
newGVariantSignature p = unsafePerformIO $
   withTextCString p $ \cstr -> do
     isSignature <- toEnum . fromIntegral <$> g_variant_is_signature cstr
     if isSignature
     then return $ Just (GVariantSignature p)
     else return Nothing

-- | Return the 'Text' representation of a 'GVariantSignature'.
gvariantSignatureToText :: GVariantSignature -> Text
gvariantSignatureToText (GVariantSignature p) = p

instance IsGVariant GVariantSignature where
    toGVariant = gvariantFromSignature
    fromGVariant = gvariantToSignature >=> return . (GVariantSignature <$>)
    toGVariantFormatString _ = "g"
instance IsGVariantBasicType GVariantSignature

foreign import ccall "g_variant_new_signature" new_signature
    :: CString -> IO (Ptr GVariant)

-- | Construct a 'GVariant' containing an DBus signature. In order to
-- build a 'GVariantSignature' value see 'newGVariantSignature'.
gvariantFromSignature :: GVariantSignature -> IO GVariant
gvariantFromSignature (GVariantSignature p) =
    withTextCString p $ new_signature >=> wrapGVariantPtr

-- | Extract a 'GVariantSignature' from a 'GVariant', represented as
-- 'Text'.
gvariantToSignature :: GVariant -> IO (Maybe Text)
gvariantToSignature =
    withExplicitType (toGVariantFormatString (undefined :: GVariantSignature))
                         $ get_string >=> cstringToText

instance IsGVariant GVariant where
    toGVariant = gvariantFromGVariant
    fromGVariant = gvariantToGVariant
    toGVariantFormatString _ = "v"

foreign import ccall "g_variant_new_variant" new_variant
    :: Ptr GVariant -> IO (Ptr GVariant)

-- | Box a 'GVariant' inside another 'GVariant'.
gvariantFromGVariant :: GVariant -> IO GVariant
gvariantFromGVariant v = withManagedPtr v $ new_variant >=> wrapGVariantPtr

foreign import ccall "g_variant_get_variant" get_variant
    :: Ptr GVariant -> IO (Ptr GVariant)

-- | Unbox a 'GVariant' contained inside another 'GVariant'.
gvariantToGVariant :: GVariant -> IO (Maybe GVariant)
gvariantToGVariant = withTypeCheck $ get_variant >=> wrapGVariantPtr

instance IsGVariant ByteString where
    toGVariant = gvariantFromBytestring
    fromGVariant = gvariantToBytestring
    toGVariantFormatString _ = "ay"

foreign import ccall "g_variant_get_bytestring" get_bytestring
    :: Ptr GVariant -> IO CString

-- | Extract a zero terminated list of bytes into a 'ByteString'.
gvariantToBytestring :: GVariant -> IO (Maybe ByteString)
gvariantToBytestring = withTypeCheck (get_bytestring >=> cstringToByteString)

foreign import ccall "g_variant_new_bytestring" new_bytestring
    :: CString -> IO (Ptr GVariant)

-- | Encode a 'ByteString' into a list of bytes 'GVariant'.
gvariantFromBytestring :: ByteString -> IO GVariant
gvariantFromBytestring bs = wrapGVariantPtr =<<
                              B.useAsCString bs new_bytestring


foreign import ccall "g_variant_n_children" g_variant_n_children
    :: Ptr GVariant -> IO #{type gsize}

foreign import ccall "g_variant_get_child_value" g_variant_get_child_value
    :: Ptr GVariant -> #{type gsize} -> IO (Ptr GVariant)

-- No type checking is done here, it is assumed that the caller knows
-- that the passed variant is indeed of a container type.
gvariant_get_children :: (Ptr GVariant) -> IO [GVariant]
gvariant_get_children vptr = do
      n_children <- g_variant_n_children vptr
      -- n_children is an unsigned type (Word64 in 64 bit
      -- architectures), so if it is 0 and we substract one we would
      -- wrap around to 2^64-1.
      if n_children /= 0
        then mapM ((g_variant_get_child_value vptr) >=> wrapGVariantPtr)
             [0..(n_children-1)]
        else return []

instance IsGVariant a => IsGVariant (Maybe a) where
    toGVariant   = gvariantFromMaybe
    fromGVariant = gvariantToMaybe
    toGVariantFormatString _ = "m" <> toGVariantFormatString (undefined :: a)

foreign import ccall "g_variant_new_maybe" g_variant_new_maybe ::
    Ptr GVariantType -> Ptr GVariant -> IO (Ptr GVariant)

-- | Convert a 'Maybe' value into a corresponding 'GVariant' of maybe
-- type.
gvariantFromMaybe :: forall a. IsGVariant a => Maybe a -> IO GVariant
gvariantFromMaybe m = do
  let fmt = toGVariantFormatString (undefined :: a)
  withGVariantType fmt $ \tPtr ->
      case m of
        Just child -> do
               childVariant <- toGVariant child
               withManagedPtr childVariant
                      (g_variant_new_maybe tPtr >=> wrapGVariantPtr)
        Nothing -> g_variant_new_maybe tPtr nullPtr >>= wrapGVariantPtr

-- | Try to decode a maybe 'GVariant' into the corresponding 'Maybe'
-- type. If the conversion is successful this returns @Just x@, where
-- @x@ itself is of 'Maybe' type. So, in particular, @Just Nothing@
-- indicates a successful call, and means that the GVariant of maybe
-- type was empty.
gvariantToMaybe :: forall a. IsGVariant a => GVariant -> IO (Maybe (Maybe a))
gvariantToMaybe v = do
  let fmt = toGVariantFormatString (undefined :: Maybe a)
  withExplicitType fmt gvariant_get_children v >>=
   \case
     Just [] -> return (Just Nothing)
     Just [child] -> fromGVariant child >>=
                     \case
                       Nothing -> return Nothing
                       Just result -> return (Just (Just result))
     Just _ -> error "gvariantToMaybe :: the impossible happened, this is a bug."
     Nothing -> return Nothing

-- | A DictEntry 'GVariant' is isomorphic to a two-tuple. Wrapping the
-- values into a 'GVariantDictentry' allows the 'IsGVariant' instance
-- to do the right thing.
data GVariantDictEntry key value = GVariantDictEntry key value
                                   deriving (Eq, Show)

instance (IsGVariant a, IsGVariantBasicType a, IsGVariant b) =>
    IsGVariant (GVariantDictEntry a b) where
        toGVariant (GVariantDictEntry key value) =
            gvariantFromDictEntry key value
        fromGVariant gv =
            ((uncurry GVariantDictEntry) <$>) <$> gvariantToDictEntry gv
        toGVariantFormatString _ = "{"
                                   <> toGVariantFormatString (undefined :: a)
                                   <> toGVariantFormatString (undefined :: b)
                                   <> "}"

foreign import ccall "g_variant_new_dict_entry" g_variant_new_dict_entry ::
    Ptr GVariant -> Ptr GVariant -> IO (Ptr GVariant)

-- | Construct a 'GVariant' of type DictEntry from the given 'key' and
-- 'value'. The key must be a basic 'GVariant' type, i.e. not a
-- container. This is determined by whether it belongs to the
-- 'IsGVariantBasicType' typeclass. On the other hand 'value' is an
-- arbitrary 'GVariant', and in particular it can be a container type.
gvariantFromDictEntry :: (IsGVariant key, IsGVariantBasicType key,
                          IsGVariant value) =>
                         key -> value -> IO GVariant
gvariantFromDictEntry key value = do
  keyVar <- toGVariant key
  valueVar <- toGVariant value
  withManagedPtr keyVar $ \keyPtr ->
      withManagedPtr valueVar $ \valuePtr ->
          g_variant_new_dict_entry keyPtr valuePtr >>= wrapGVariantPtr

-- | Unpack a DictEntry variant into 'key' and 'value', which are
-- returned as a two element tuple in case of success.
gvariantToDictEntry :: forall key value.
                       (IsGVariant key, IsGVariantBasicType key,
                        IsGVariant value) =>
                       GVariant -> IO (Maybe (key, value))
gvariantToDictEntry =
    withExplicitType fmt $ \varPtr -> do
      [key, value] <- gvariant_get_children varPtr
      (,) <$> unsafeFromGVariant key <*> unsafeFromGVariant value
    where
      fmt = toGVariantFormatString (undefined :: GVariantDictEntry key value)

instance (IsGVariant a, IsGVariantBasicType a, IsGVariant b) =>
    IsGVariant (M.Map a b) where
        toGVariant = gvariantFromMap
        fromGVariant = gvariantToMap
        toGVariantFormatString _ = "a{"
                                   <> toGVariantFormatString (undefined :: a)
                                   <> toGVariantFormatString (undefined :: b)
                                   <> "}"

-- | Pack a 'Map' into a 'GVariant' for dictionary type, which is just
-- an array of 'GVariantDictEntry'.
gvariantFromMap :: (IsGVariant key, IsGVariantBasicType key,
                    IsGVariant value) =>
                   M.Map key value -> IO GVariant
gvariantFromMap m = gvariantFromList $
                       map (uncurry GVariantDictEntry) (M.toList m)

-- | Unpack a 'GVariant' into a 'M.Map'. Notice that this assumes that
-- all the elements in the 'GVariant' array of 'GVariantDictEntry' are
-- of the same type, which is not necessary for a generic 'GVariant',
-- so this is somewhat restrictive. For the general case it is
-- necessary to use 'gvariantToList' plus 'gvariantToDictEntry'
-- directly.
gvariantToMap :: forall key value.
                 (IsGVariant key, IsGVariantBasicType key,
                  IsGVariant value) =>
                 GVariant -> IO (Maybe (M.Map key value))
gvariantToMap = gvariantToList >=> (return . (fromDictEntryList <$>))
    where fromDictEntryList :: [GVariantDictEntry key value] ->
                               M.Map key value
          fromDictEntryList = M.fromList . (map tuplefy)
          tuplefy :: GVariantDictEntry key value -> (key, value)
          tuplefy (GVariantDictEntry key value) = (key, value)

instance IsGVariant a => IsGVariant [a] where
    toGVariant   = gvariantFromList
    fromGVariant = gvariantToList
    toGVariantFormatString _ = "a" <> toGVariantFormatString (undefined :: a)

foreign import ccall "g_variant_new_array" g_variant_new_array ::
    Ptr GVariantType -> Ptr (Ptr GVariant) -> #{type gsize} -> IO (Ptr GVariant)

-- | Given a list of elements construct a 'GVariant' array containing
-- them.
gvariantFromList :: forall a. IsGVariant a => [a] -> IO GVariant
gvariantFromList children = do
  let fmt = toGVariantFormatString (undefined :: a)
  mapM toGVariant children >>= \childVariants ->
      withManagedPtrList childVariants $ \childrenPtrs -> do
          withGVariantType fmt $ \childType -> do
             packed <- packPtrArray childrenPtrs
             result <- g_variant_new_array childType packed
                            (fromIntegral $ length children)
             freeMem packed
             wrapGVariantPtr result

-- | Unpack a 'GVariant' array into its elements.
gvariantToList :: forall a. IsGVariant a => GVariant -> IO (Maybe [a])
gvariantToList = withExplicitType (toGVariantFormatString (undefined :: [a]))
                 (gvariant_get_children >=> mapM unsafeFromGVariant)

foreign import ccall "g_variant_new_tuple" g_variant_new_tuple
        :: Ptr (Ptr GVariant) -> #{type gsize} -> IO (Ptr GVariant)

-- | Given a list of 'GVariant', construct a 'GVariant' tuple
-- containing the elements in the list.
gvariantFromTuple :: [GVariant] -> IO GVariant
gvariantFromTuple children =
    withManagedPtrList children $ \childrenPtrs -> do
      packed <- packPtrArray childrenPtrs
      result <- g_variant_new_tuple packed (fromIntegral $ length children)
      freeMem packed
      wrapGVariantPtr result

-- | Extract the children of a 'GVariant' tuple into a list.
gvariantToTuple :: GVariant -> IO (Maybe [GVariant])
gvariantToTuple = withExplicitType "r" gvariant_get_children

-- | The empty tuple GVariant, mostly useful for type checking.
instance IsGVariant () where
    toGVariant _ = gvariantFromTuple []
    fromGVariant = withTypeCheck (const $ return ())
    toGVariantFormatString _ = "()"

-- | One element tuples.
instance IsGVariant a => IsGVariant (GVariantSinglet a) where
    toGVariant (GVariantSinglet s) = gvariantFromSinglet s
    fromGVariant = gvariantToSinglet >=> return . (GVariantSinglet <$>)
    toGVariantFormatString _ = "("
                               <> toGVariantFormatString (undefined :: a)
                               <> ")"

gvariantFromSinglet :: IsGVariant a => a -> IO GVariant
gvariantFromSinglet s = do
  sv <- toGVariant s
  gvariantFromTuple [sv]

gvariantToSinglet :: forall a. IsGVariant a => GVariant -> IO (Maybe a)
gvariantToSinglet = withExplicitType fmt
                    (gvariant_get_children
                     >=> return . head
                     >=> unsafeFromGVariant)
    where fmt = toGVariantFormatString (undefined :: GVariantSinglet a)

instance (IsGVariant a, IsGVariant b) => IsGVariant (a,b) where
    toGVariant = gvariantFromTwoTuple
    fromGVariant = gvariantToTwoTuple
    toGVariantFormatString _ = "("
                               <> toGVariantFormatString (undefined :: a)
                               <> toGVariantFormatString (undefined :: b)
                               <> ")"

gvariantFromTwoTuple :: (IsGVariant a, IsGVariant b) =>
                        (a,b) -> IO GVariant
gvariantFromTwoTuple (a, b) = do
  va <- toGVariant a
  vb <- toGVariant b
  gvariantFromTuple [va, vb]

gvariantToTwoTuple :: forall a b. (IsGVariant a, IsGVariant b) =>
                      GVariant -> IO (Maybe (a,b))
gvariantToTwoTuple variant = do
  let expectedType = toGVariantFormatString (undefined :: (a,b))
  maybeChildren <- withExplicitType expectedType gvariant_get_children variant
  if isJust maybeChildren
  then do
    let (Just [a1,a2]) = maybeChildren
    (ma1, ma2) <- (,) <$> fromGVariant a1 <*> fromGVariant a2
    return $ if isJust ma1 && isJust ma2
             then Just (fromJust ma1, fromJust ma2)
             else Nothing
  else return Nothing

instance (IsGVariant a, IsGVariant b, IsGVariant c) => IsGVariant (a,b,c) where
    toGVariant = gvariantFromThreeTuple
    fromGVariant = gvariantToThreeTuple
    toGVariantFormatString _ = "("
                               <> toGVariantFormatString (undefined :: a)
                               <> toGVariantFormatString (undefined :: b)
                               <> toGVariantFormatString (undefined :: c)
                               <> ")"

gvariantFromThreeTuple :: (IsGVariant a, IsGVariant b, IsGVariant c) =>
                        (a,b,c) -> IO GVariant
gvariantFromThreeTuple (a, b, c) = do
  va <- toGVariant a
  vb <- toGVariant b
  vc <- toGVariant c
  gvariantFromTuple [va, vb, vc]

gvariantToThreeTuple :: forall a b c. (IsGVariant a, IsGVariant b,
                                                  IsGVariant c) =>
                      GVariant -> IO (Maybe (a,b,c))
gvariantToThreeTuple variant = do
  let expectedType = toGVariantFormatString (undefined :: (a,b,c))
  maybeChildren <- withExplicitType expectedType gvariant_get_children variant
  if isJust maybeChildren
  then do
    let (Just [a1,a2,a3]) = maybeChildren
    (ma1, ma2, ma3) <- (,,) <$> fromGVariant a1
                            <*> fromGVariant a2
                            <*> fromGVariant a3
    return $ if isJust ma1 && isJust ma2 && isJust ma3
             then Just (fromJust ma1, fromJust ma2, fromJust ma3)
             else Nothing
  else return Nothing

instance (IsGVariant a, IsGVariant b, IsGVariant c, IsGVariant d) =>
    IsGVariant (a,b,c,d) where
    toGVariant = gvariantFromFourTuple
    fromGVariant = gvariantToFourTuple
    toGVariantFormatString _ = "("
                               <> toGVariantFormatString (undefined :: a)
                               <> toGVariantFormatString (undefined :: b)
                               <> toGVariantFormatString (undefined :: c)
                               <> toGVariantFormatString (undefined :: d)
                               <> ")"

gvariantFromFourTuple :: (IsGVariant a, IsGVariant b, IsGVariant c,
                          IsGVariant d) => (a,b,c,d) -> IO GVariant
gvariantFromFourTuple (a, b, c, d) = do
  va <- toGVariant a
  vb <- toGVariant b
  vc <- toGVariant c
  vd <- toGVariant d
  gvariantFromTuple [va, vb, vc, vd]

gvariantToFourTuple :: forall a b c d. (IsGVariant a, IsGVariant b,
                                        IsGVariant c, IsGVariant d) =>
                      GVariant -> IO (Maybe (a,b,c,d))
gvariantToFourTuple variant = do
  let expectedType = toGVariantFormatString (undefined :: (a,b,c,d))
  maybeChildren <- withExplicitType expectedType gvariant_get_children variant
  if isJust maybeChildren
  then do
    let (Just [a1,a2,a3,a4]) = maybeChildren
    (ma1, ma2, ma3,ma4) <- (,,,) <$> fromGVariant a1
                                 <*> fromGVariant a2
                                 <*> fromGVariant a3
                                 <*> fromGVariant a4
    return $ if isJust ma1 && isJust ma2 && isJust ma3 && isJust ma4
             then Just (fromJust ma1, fromJust ma2, fromJust ma3, fromJust ma4)
             else Nothing
  else return Nothing

instance (IsGVariant a, IsGVariant b, IsGVariant c, IsGVariant d, IsGVariant e)
    => IsGVariant (a,b,c,d,e) where
    toGVariant = gvariantFromFiveTuple
    fromGVariant = gvariantToFiveTuple
    toGVariantFormatString _ = "("
                               <> toGVariantFormatString (undefined :: a)
                               <> toGVariantFormatString (undefined :: b)
                               <> toGVariantFormatString (undefined :: c)
                               <> toGVariantFormatString (undefined :: d)
                               <> toGVariantFormatString (undefined :: e)
                               <> ")"

gvariantFromFiveTuple :: (IsGVariant a, IsGVariant b, IsGVariant c,
                          IsGVariant d, IsGVariant e) =>
                         (a,b,c,d,e) -> IO GVariant
gvariantFromFiveTuple (a, b, c, d, e) = do
  va <- toGVariant a
  vb <- toGVariant b
  vc <- toGVariant c
  vd <- toGVariant d
  ve <- toGVariant e
  gvariantFromTuple [va, vb, vc, vd, ve]

gvariantToFiveTuple :: forall a b c d e.
                       (IsGVariant a, IsGVariant b, IsGVariant c,
                        IsGVariant d, IsGVariant e) =>
                      GVariant -> IO (Maybe (a,b,c,d,e))
gvariantToFiveTuple variant = do
  let expectedType = toGVariantFormatString (undefined :: (a,b,c,d,e))
  maybeChildren <- withExplicitType expectedType gvariant_get_children variant
  if isJust maybeChildren
  then do
    let (Just [a1,a2,a3,a4,a5]) = maybeChildren
    (ma1, ma2, ma3, ma4, ma5) <- (,,,,) <$> fromGVariant a1
                                        <*> fromGVariant a2
                                        <*> fromGVariant a3
                                        <*> fromGVariant a4
                                        <*> fromGVariant a5
    return $ if isJust ma1 && isJust ma2 && isJust ma3 &&
                              isJust ma4 && isJust ma5
             then Just (fromJust ma1, fromJust ma2, fromJust ma3,
                        fromJust ma4, fromJust ma5)
             else Nothing
  else return Nothing
