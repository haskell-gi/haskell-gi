
module GI.Internal.PropertyInfo
    ( ParamFlag(..)
    , propertyInfoFlags
    , propertyInfoType
    )
where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

import GI.Internal.ParamFlag
import GI.Util (toFlags)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_property_info" #}

{-
This doesn't work; see ParamFlag.hs.

{# enum GParamFlags as ParamFlag {underscoreToCase} with prefix="G"
    deriving (Show, Eq) #}
-}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: PropertyInfoClass pic => pic -> Ptr ()
stupidCast pi = castPtr p
  where (PropertyInfo p) = propertyInfo pi

propertyInfoFlags :: PropertyInfoClass pic => pic -> [ParamFlag]
propertyInfoFlags pi = unsafePerformIO $ toFlags <$>
    {# call get_flags #} (stupidCast pi)

propertyInfoType :: PropertyInfoClass pic => pic -> TypeInfo
propertyInfoType pi = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_type #} (stupidCast pi)

