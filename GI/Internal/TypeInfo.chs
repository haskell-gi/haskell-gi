
module GI.Internal.TypeInfo
    ( TypeTag(..)
    , typeInfoIsPointer
    , typeInfoTag
    , typeInfoParamType
    , typeInfoInterface
    , typeInfoArrayLength
    , typeInfoArrayFixedSize
    , typeInfoIsZeroTerminated
    , typeInfoArrayType
    , typeTagToString
    )
where

import Foreign
import Foreign.C

import Control.Applicative ((<$>))

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_type_info" #}

{# enum GIArrayType as ArrayType {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}
{# enum GITypeTag as TypeTag {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: TypeInfoClass base => base -> Ptr ()
stupidCast ti = castPtr p
  where (TypeInfo p) = typeInfo ti

typeInfoIsPointer :: TypeInfoClass tic => tic -> Bool
typeInfoIsPointer ti = unsafePerformIO $
    (/= 0) <$> {# call is_pointer #} (stupidCast ti)

typeInfoTag :: TypeInfoClass tic => tic -> TypeTag
typeInfoTag ti = unsafePerformIO $ toEnum . fromIntegral <$>
    {# call get_tag #} (stupidCast ti)

typeInfoParamType :: TypeInfoClass tic => tic -> Int -> TypeInfo
typeInfoParamType ti n = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_param_type #} (stupidCast ti) (fromIntegral n)

typeInfoInterface :: TypeInfoClass tic => tic -> InterfaceInfo
typeInfoInterface ti = unsafePerformIO $ fromBaseInfo <$> baseInfo <$>
    {# call get_interface #} (stupidCast ti)

typeInfoArrayLength :: TypeInfoClass tic => tic -> Int
typeInfoArrayLength ti = unsafePerformIO $ fromIntegral <$>
    {# call get_array_length #} (stupidCast ti)

typeInfoArrayFixedSize :: TypeInfoClass tic => tic -> Int
typeInfoArrayFixedSize ti = unsafePerformIO $ fromIntegral <$>
    {# call get_array_fixed_size #} (stupidCast ti)

typeInfoIsZeroTerminated :: TypeInfoClass tic => tic -> Bool
typeInfoIsZeroTerminated ti = unsafePerformIO $ (/= 0) <$>
    {# call is_zero_terminated #} (stupidCast ti)

typeInfoArrayType :: TypeInfoClass tic => tic -> ArrayType
typeInfoArrayType ti = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_array_type #} (stupidCast ti)

typeTagToString :: TypeTag -> String
typeTagToString tag = unsafePerformIO $ peekCString =<<
    {# call g_type_tag_to_string #} (fromIntegral $ fromEnum tag)
