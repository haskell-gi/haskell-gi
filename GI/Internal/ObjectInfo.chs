
module GI.Internal.ObjectInfo
    ( objectInfoFields
    , objectInfoMethods
    , objectInfoProperties
    , objectInfoSignals
    , objectInfoConstants
    , objectInfoInterfaces
    , objectInfoParent
    , objectInfoTypeInit
    , objectInfoTypeName
    -- , objectInfoVFuncs
    -- XXX: lots more stuff missing
    ) where

import Control.Applicative ((<$>))
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Util (getList)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_object_info" #}

stupidCast :: ObjectInfoClass oic => oic -> Ptr ()
stupidCast oi = castPtr p
    where (ObjectInfo p) = objectInfo oi

objectInfoFields :: ObjectInfoClass oic => oic -> [FieldInfo]
objectInfoFields oi = unsafePerformIO $
    map (FieldInfo <$> castPtr) <$>
    getList {# call get_n_fields #} {# call get_field #} (stupidCast oi)

objectInfoMethods :: ObjectInfoClass oic => oic -> [FunctionInfo]
objectInfoMethods oi = unsafePerformIO $
    map (FunctionInfo <$> castPtr) <$>
    getList {# call get_n_methods #} {# call get_method #} (stupidCast oi)

objectInfoProperties :: ObjectInfoClass oic => oic -> [PropertyInfo]
objectInfoProperties oi = unsafePerformIO $
    map (PropertyInfo <$> castPtr) <$>
    getList {# call get_n_properties #} {# call get_property #} (stupidCast oi)

objectInfoInterfaces :: ObjectInfoClass oic => oic -> [InterfaceInfo]
objectInfoInterfaces oi = unsafePerformIO $
    map (InterfaceInfo <$> castPtr) <$>
    getList {# call get_n_interfaces #} {# call get_interface #} (stupidCast oi)

objectInfoSignals :: ObjectInfoClass oic => oic -> [SignalInfo]
objectInfoSignals oi = unsafePerformIO $
    map (SignalInfo <$> castPtr) <$>
    getList {# call get_n_signals #} {# call get_signal #} (stupidCast oi)

objectInfoConstants :: ObjectInfoClass oic => oic -> [ConstantInfo]
objectInfoConstants oi = unsafePerformIO $
    map (ConstantInfo <$> castPtr) <$>
    getList {# call get_n_constants #} {# call get_constant #} (stupidCast oi)

objectInfoParent :: ObjectInfoClass oic => oic -> Maybe ObjectInfo
objectInfoParent oi = unsafePerformIO $ do
    parent <- {# call get_parent #} (stupidCast oi)
    if parent == nullPtr then
        return Nothing
    else
        return $ Just $ (ObjectInfo . castPtr) parent

objectInfoTypeInit :: ObjectInfoClass oic => oic -> String
objectInfoTypeInit oi = unsafePerformIO $ do
    result <- {# call get_type_init #} (stupidCast oi)
    peekCString result

objectInfoTypeName :: ObjectInfoClass oic => oic -> String
objectInfoTypeName oi = unsafePerformIO $ do
    result <- {# call get_type_name #} (stupidCast oi)
    peekCString result
