module GI.Internal.FunctionInfo
    ( FunctionInfoFlag(..)
    , functionInfoSymbol
    , functionInfoFlags
    -- XXX: Write these.
    -- , functionInfoProperty
    -- , functionInfoVFunc
    )
where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Util (toFlags)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_function_info" #}

{# enum GIFunctionInfoFlags as FunctionInfoFlag {underscoreToCase}
   with prefix="GI"
   deriving (Show, Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: FunctionInfoClass fic => fic -> Ptr ()
stupidCast fi = castPtr p
  where (FunctionInfo p) = functionInfo fi

functionInfoSymbol :: FunctionInfoClass fic => fic -> String
functionInfoSymbol fi = unsafePerformIO $ peekCString =<<
    {# call get_symbol #} (stupidCast fi)

functionInfoFlags :: FunctionInfoClass fic => fic -> [FunctionInfoFlag]
functionInfoFlags fi = unsafePerformIO $ toFlags <$>
    {# call get_flags #} (stupidCast fi)
