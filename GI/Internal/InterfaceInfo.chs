module GI.Internal.InterfaceInfo
    ( interfaceInfoPrerequisites
    , interfaceInfoProperties
    , interfaceInfoMethods
    , interfaceInfoSignals
    -- , interfaceInfoVFuncs
    , interfaceInfoConstants
    )
where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Util (getList)
import GI.Utils.BasicTypes (GType(..), CGType)

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
        g_type_interface_prerequisites :: CGType -> Ptr a -> IO (Ptr CGType)

foreign import ccall unsafe "g_registered_type_info_get_g_type"
        g_registered_type_info_get_g_type :: Ptr () -> IO CGType

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
         where go :: Ptr CGType -> CUInt -> IO [BaseInfo]
               go _ 0 = return []
               go ptr n = do
                 gtype <- peek ptr
                 pi <- findByGType (GType gtype)
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

interfaceInfoSignals :: InterfaceInfoClass oic => oic -> [SignalInfo]
interfaceInfoSignals ii = unsafePerformIO $
    map (SignalInfo <$> castPtr) <$>
    getList {# call get_n_signals #} {# call get_signal #} (stupidCast ii)

{-
gint                g_interface_info_get_n_vfuncs       (GIInterfaceInfo *info);
GIVFuncInfo *       g_interface_info_get_vfunc          (GIInterfaceInfo *info,
                                                         gint n);
GIStructInfo *      g_interface_info_get_iface_struct   (GIInterfaceInfo *info);
-}
