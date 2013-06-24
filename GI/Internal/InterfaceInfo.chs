
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
import Foreign.Safe
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Util (getList)
import GI.GType (GType)

{# import GI.Internal.Types #}
{# import GI.Internal.Typelib #}

#include <girepository.h>

{# context prefix="g_interface_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: InterfaceInfoClass iic => iic -> Ptr ()
stupidCast ii = castPtr p
  where (InterfaceInfo p) = interfaceInfo ii

-- g_interface_info_get_prerequisites does not seem very reliable, so
-- we go through the gobject type machinery instead.
foreign import ccall unsafe "g_type_interface_prerequisites"
        g_type_interface_prerequisites :: GType -> Ptr a -> IO (Ptr GType)

foreign import ccall unsafe "g_registered_type_info_get_g_type"
        g_registered_type_info_get_g_type :: Ptr () -> IO GType

interfaceInfoPrerequisites :: InterfaceInfoClass iic => iic -> [BaseInfo]
interfaceInfoPrerequisites ii = unsafePerformIO $ do
    gtype <- g_registered_type_info_get_g_type (stupidCast ii)
    nprereqs' <- malloc :: IO (Ptr CUInt)
    prereqs <- g_type_interface_prerequisites gtype nprereqs'
    nprereqs <- peek nprereqs'
    free nprereqs'
    if prereqs == nullPtr || nprereqs == 0
    then return []
    else go prereqs nprereqs
         where go :: Ptr GType -> CUInt -> IO [BaseInfo]
               go _ 0 = return []
               go ptr n = do
                 gtype <- peek ptr
                 pi <- findByGType gtype
                 case pi of
                   Just bi -> (bi :) <$> go (ptr `plusPtr` sizeOf gtype) (n-1)
                   Nothing -> go (ptr `plusPtr` sizeOf gtype) (n-1)

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
