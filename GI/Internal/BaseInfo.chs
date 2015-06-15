module GI.Internal.BaseInfo
  ( infoIsDeprecated
  , infoName
  , infoNamespace
  , infoType
  , infoAttribute
  , InfoType(..)
  )
where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_base_info" #}

{# enum GIInfoType as InfoType {underscoreToCase} with prefix="GI"
  deriving (Show, Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: InfoClass info
           => info
           -> ValueInfo
stupidCast info = ValueInfo (castPtr p)
  where (BaseInfo p) = baseInfo info

infoIsDeprecated :: InfoClass info
                 => info
                 -> Bool
infoIsDeprecated i = unsafePerformIO $
    (/= 0) <$> {# call g_base_info_is_deprecated #} (stupidCast i)

infoName :: InfoClass info
         => info
         -> String
infoName i = unsafePerformIO $ do
    ret <- {# call g_base_info_get_name #} (stupidCast i)
    peekCString ret

infoNamespace :: InfoClass info
              => info
              -> String
infoNamespace i = unsafePerformIO $ do
    ret <- {# call g_base_info_get_namespace #} (stupidCast i)
    peekCString ret

infoType :: InfoClass info
         => info
         -> InfoType
infoType i = unsafePerformIO $ do
    toEnum . fromIntegral <$> {# call get_type #} (stupidCast i)

infoAttribute :: InfoClass info
              => info
              -> String
              -> Maybe String
infoAttribute i name = unsafePerformIO $ do
    result <- withCString name $ {# call get_attribute #} (stupidCast i)
    if result == nullPtr
       then return Nothing
       else Just <$> peekCString result
