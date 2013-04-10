
module GI.Internal.InterfaceInfo
    ( interfaceInfoPrerequisites
    , interfaceInfoProperties
    , interfaceInfoMethods
    -- , interfaceInfoSignals
    -- , interfaceInfoVFuncs
    , interfaceInfoConstants
    )
where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

import GI.Util (getList)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_interface_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: InterfaceInfoClass iic => iic -> Ptr ()
stupidCast ii = castPtr p
  where (InterfaceInfo p) = interfaceInfo ii

interfaceInfoPrerequisites :: InterfaceInfoClass iic => iic -> [BaseInfo]
interfaceInfoPrerequisites ii = unsafePerformIO $
    map baseInfo <$>
    getList {# call get_n_prerequisites #} {# call get_prerequisite #}
        (stupidCast ii)

interfaceInfoMethods :: InterfaceInfoClass iic => iic -> [FunctionInfo]
interfaceInfoMethods ii = unsafePerformIO $
    map (FunctionInfo <$> castPtr) <$>
    getList {# call get_n_methods #} {# call get_method #} (stupidCast ii)

interfaceInfoConstants :: InterfaceInfoClass iic => iic -> [ConstantInfo]
interfaceInfoConstants ii = unsafePerformIO $
    map (ConstantInfo <$> castPtr) <$>
    getList {# call get_n_constants #} {# call get_constant #} (stupidCast ii)

interfaceInfoProperties :: InterfaceInfoClass iic => iic -> [PropertyInfo]
interfaceInfoProperties ii = unsafePerformIO $
    map (PropertyInfo <$> castPtr) <$>
    getList {# call get_n_properties #} {# call get_property #}
        (stupidCast ii)

{-
gint                g_interface_info_get_n_properties   (GIInterfaceInfo *info);
GIPropertyInfo *    g_interface_info_get_property       (GIInterfaceInfo *info,
                                                         gint n);
gint                g_interface_info_get_n_signals      (GIInterfaceInfo *info);
GISignalInfo *      g_interface_info_get_signal         (GIInterfaceInfo *info,
                                                         gint n);
gint                g_interface_info_get_n_vfuncs       (GIInterfaceInfo *info);
GIVFuncInfo *       g_interface_info_get_vfunc          (GIInterfaceInfo *info,
                                                         gint n);
GIStructInfo *      g_interface_info_get_iface_struct   (GIInterfaceInfo *info);
-}
