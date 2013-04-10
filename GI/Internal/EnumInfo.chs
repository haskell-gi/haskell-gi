
module GI.Internal.EnumInfo
    ( enumInfoValues
    , valueInfoValue
    ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

import GI.Util (getList)

{# import GI.Internal.Types #}

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

valueInfoValue :: ValueInfoClass val => val -> Word64
valueInfoValue vi = unsafePerformIO $ fromIntegral <$>
    {# call g_value_info_get_value #} (stupidValueCast vi)
