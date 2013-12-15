
module GI.Internal.UnionInfo
    ( unionInfoFields
    , unionInfoSize
    , unionInfoMethods
    ) where

import Control.Applicative ((<$>))
import Foreign.Safe
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

{# import GI.Internal.Types #}
import GI.Util (getList)

#include <girepository.h>

{# context prefix="g_union_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: UnionInfoClass sic => sic -> Ptr ()
stupidCast si = castPtr p
  where (UnionInfo p) = unionInfo si

unionInfoFields :: UnionInfoClass uic => uic -> [FieldInfo]
unionInfoFields ui = unsafePerformIO $
    map (FieldInfo <$> castPtr) <$>
    getList {# call get_n_fields #} {# call get_field #} (stupidCast ui)

unionInfoSize :: UnionInfoClass uic => uic -> Int
unionInfoSize ui = unsafePerformIO $ fromIntegral <$>
                    {# call get_size #} (stupidCast ui)

unionInfoMethods :: UnionInfoClass sic => sic -> [FunctionInfo]
unionInfoMethods ui = unsafePerformIO $
    map (FunctionInfo <$> castPtr) <$>
    getList {# call get_n_methods #} {# call get_method #} (stupidCast ui)
