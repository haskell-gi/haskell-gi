module GI.Internal.BaseInfo
  ( baseInfoIsDeprecated
  , baseInfoName
  , baseInfoNamespace
  , baseInfoType
  , InfoType(..)
  )
where

import Foreign
import Foreign.C

import Control.Applicative ((<$>))

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_base_info" #}

{# enum GIInfoType as InfoType {underscoreToCase} with prefix="GI"
  deriving (Show, Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: BaseInfoClass base
           => base
           -> ValueInfo
stupidCast base = ValueInfo (castPtr p)
  where
    (BaseInfo p) = baseInfo base

baseInfoIsDeprecated bi = unsafePerformIO $
    (/= 0) `fmap` {# call g_base_info_is_deprecated #} (stupidCast bi)

baseInfoName :: BaseInfoClass base
             => base
             -> String
baseInfoName bi = unsafePerformIO $ do
    ret <- {# call g_base_info_get_name #} (stupidCast bi)
    peekCString ret

baseInfoNamespace :: BaseInfoClass base
                  => base
                  -> String
baseInfoNamespace bi = unsafePerformIO $ do
    ret <- {# call g_base_info_get_namespace #} (stupidCast bi)
    peekCString ret

baseInfoType :: BaseInfoClass base
             => base
             -> InfoType
baseInfoType bi = unsafePerformIO $ do
    toEnum . fromIntegral <$> {# call get_type #} (stupidCast bi)
