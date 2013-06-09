
module GI.Internal.RegisteredTypeInfo
    ( registeredTypeInfoTypeName
    , registeredTypeInfoGType
    , registeredTypeInfoTypeInit
    )
where

import Control.Applicative ((<$>))
import Foreign.Safe
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_registered_type_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: RegisteredTypeInfoClass base => base -> Ptr ()
stupidCast rti = castPtr p
  where (RegisteredTypeInfo p) = registeredTypeInfo rti

registeredTypeInfoTypeName :: RegisteredTypeInfoClass rtic => rtic -> String
registeredTypeInfoTypeName rti = unsafePerformIO $ do
    name <- {# call get_type_name #} (stupidCast rti)
    if name == nullPtr
        then return "?"
        else peekCString name

registeredTypeInfoTypeInit :: RegisteredTypeInfoClass rtic => rtic -> Maybe String
registeredTypeInfoTypeInit rti =
    unsafePerformIO $ do
      typeInit <- {# call get_type_init #} (stupidCast rti)
      if typeInit == nullPtr
      then return Nothing
      else Just <$> peekCString typeInit

registeredTypeInfoGType :: RegisteredTypeInfoClass rtic => rtic -> Integer
registeredTypeInfoGType rti = unsafePerformIO $ fromIntegral <$>
    {# call get_g_type #} (stupidCast rti)
