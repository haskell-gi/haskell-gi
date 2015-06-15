module GI.Internal.ArgInfo
    ( Direction(..)
    , Scope(..)
    , Transfer(..)
    , argInfoDirection
    , argInfoIsCallerAllocates
    , argInfoIsReturnValue
    , argInfoIsOptional
    , argInfoMayBeNull
    , argInfoOwnershipTransfer
    , argInfoScope
    , argInfoClosure
    , argInfoDestroy
    , argInfoType
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_arg_info" #}

{# enum GIDirection as Direction {underscoreToCase} with prefix="GI" deriving (Show, Eq, Ord) #}
{# enum GIScopeType as Scope {underscoreToCase} with prefix="GI" deriving (Show, Eq, Ord) #}
{# enum GITransfer as Transfer {underscoreToCase} with prefix="GI" deriving (Show, Eq, Ord) #}

stupidCast :: ArgInfoClass arg => arg -> Ptr ()
stupidCast arg = castPtr p
    where (ArgInfo p) = argInfo arg

argInfoDirection :: ArgInfoClass arg => arg -> Direction
argInfoDirection ai = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_direction #} (stupidCast ai)

argInfoIsCallerAllocates :: ArgInfoClass arg => arg -> Bool
argInfoIsCallerAllocates ai = unsafePerformIO $ (/= 0) <$>
    {# call is_caller_allocates #} (stupidCast ai)

argInfoIsReturnValue :: ArgInfoClass arg => arg -> Bool
argInfoIsReturnValue ai = unsafePerformIO $ (/= 0) <$>
    {# call is_return_value #} (stupidCast ai)

argInfoIsOptional :: ArgInfoClass arg => arg -> Bool
argInfoIsOptional ai = unsafePerformIO $ (/= 0) <$>
    {# call is_optional #} (stupidCast ai)

argInfoMayBeNull :: ArgInfoClass arg => arg -> Bool
argInfoMayBeNull ai = unsafePerformIO $ (/= 0) <$>
    {# call may_be_null #} (stupidCast ai)

argInfoOwnershipTransfer :: ArgInfoClass arg => arg -> Transfer
argInfoOwnershipTransfer ai = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_ownership_transfer #} (stupidCast ai)

argInfoScope :: ArgInfoClass arg => arg -> Scope
argInfoScope ai = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_scope #} (stupidCast ai)

argInfoClosure :: ArgInfoClass arg => arg -> Int
argInfoClosure ai = unsafePerformIO $ fromIntegral <$>
    {# call get_closure #} (stupidCast ai)

argInfoDestroy :: ArgInfoClass arg => arg -> Int
argInfoDestroy ai = unsafePerformIO $ fromIntegral <$>
    {# call get_destroy #} (stupidCast ai)

argInfoType :: ArgInfoClass arg => arg -> TypeInfo
argInfoType ai = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_type #} (stupidCast ai)
