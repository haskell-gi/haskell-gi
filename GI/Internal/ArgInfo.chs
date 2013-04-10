
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
    , argInfoType
    ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_arg_info" #}

{# enum GIDirection as Direction {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}
{# enum GIScopeType as Scope {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}
{# enum GITransfer as Transfer {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}

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
    {# call is_optional #} (stupidCast ai)

argInfoOwnershipTransfer :: ArgInfoClass arg => arg -> Transfer
argInfoOwnershipTransfer ai = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_ownership_transfer #} (stupidCast ai)

argInfoScope :: ArgInfoClass arg => arg -> Scope
argInfoScope ai = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_scope #} (stupidCast ai)

{-
XXX
argInfoClosure
argInfoDestroy
-}

argInfoType :: ArgInfoClass arg => arg -> TypeInfo
argInfoType ai = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_type #} (stupidCast ai)

