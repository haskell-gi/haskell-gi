
module GI.Internal.FieldInfo
    ( FieldInfoFlag(..)
    , fieldInfoFlags
    , fieldInfoOffset
    , fieldInfoType
    ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

import GI.Util (toFlags)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_field_info" #}

{# enum GIFieldInfoFlags as FieldInfoFlag {underscoreToCase} with prefix="GI"
    deriving (Show, Eq) #}

stupidCast :: FieldInfoClass fic => fic -> Ptr ()
stupidCast fi = castPtr p
    where (FieldInfo p) = fieldInfo fi

fieldInfoFlags :: FieldInfoClass fic => fic -> [FieldInfoFlag]
fieldInfoFlags fi = unsafePerformIO $ toFlags <$>
    {# call get_flags #} (stupidCast fi)

fieldInfoOffset :: FieldInfoClass fic => fic -> Int
fieldInfoOffset fi = unsafePerformIO $ fromIntegral <$>
    {# call get_offset #} (stupidCast fi)

fieldInfoType :: FieldInfoClass fic => fic -> TypeInfo
fieldInfoType fi = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_type #} (stupidCast fi)

