
module GI.Internal.EnumInfo
    ( enumInfoValues
    , enumInfoErrorDomain
    , enumInfoStorageType
    , valueInfoValue
    ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Util (getList)

import GI.Internal.Types
import GI.Internal.TypeInfo

#include <girepository.h>

{# context prefix="g_enum_info" #}

stupidEnumCast :: EnumInfoClass enum => enum -> Ptr ()
stupidEnumCast enum = castPtr p
    where (EnumInfo p) = enumInfo enum

stupidValueCast :: ValueInfoClass val => val -> Ptr ()
stupidValueCast val = castPtr p
    where (ValueInfo p) = valueInfo val

enumInfoValues :: EnumInfoClass enum => enum -> [ValueInfo]
enumInfoValues ei = unsafePerformIO $ map (ValueInfo <$> castPtr) <$>
    getList {# call get_n_values #} {# call get_value #} (stupidEnumCast ei)

enumInfoErrorDomain :: EnumInfoClass enum => enum -> Maybe String
enumInfoErrorDomain ei = unsafePerformIO $ do
    result <- {# call get_error_domain #} (stupidEnumCast ei)
    if result == nullPtr
       then return Nothing
       else Just <$> peekCString result

valueInfoValue :: ValueInfoClass val => val -> Int64
valueInfoValue vi = unsafePerformIO $ fromIntegral <$>
    {# call g_value_info_get_value #} (stupidValueCast vi)

enumInfoStorageType :: EnumInfoClass enum => enum -> TypeTag
enumInfoStorageType ei = toEnum . fromIntegral . unsafePerformIO $
                         {# call get_storage_type #} (stupidEnumCast ei)
