{-# LANGUAGE OverloadedStrings #-}

-- | Dynamically load/unload dynamic libraries. We use these libraries
-- to look up GTypes when generating code.
module GI.DynLib
    ( loadDynLib
    , unloadDynLib
    , DynLib
    , findGType
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)

import Foreign.Ptr (FunPtr, nullFunPtr, nullPtr)
import Foreign.C.String (CString, withCString)
import qualified System.Posix.DynamicLinker as DL
import qualified System.Posix.DynamicLinker.Prim as DLP

import Data.Text (Text)
import qualified Data.Text as T

import GI.Utils.BasicTypes (GType(..), CGType)

-- | Opaque type encapsulating a loaded dynamic library.
newtype DynLib = DynLib DL.DL

-- | Try to load a dynamic library.
loadDynLib :: Bool -> Text -> IO (Maybe DynLib)
loadDynLib verbose name = do
  when verbose $ putStrLn ("Loading dynlib " ++ show name)
  withCString (T.unpack name) $ \cstr -> do
    dl <- DLP.c_dlopen cstr (DLP.packRTLDFlags [DL.RTLD_LAZY, DL.RTLD_GLOBAL])
    if dl /= nullPtr
    then (return . Just . DynLib . DLP.DLHandle) dl
    else do
      when verbose $
        putStrLn ("Could not load dynamic library " ++ show name)
      return Nothing

type GTypeInit = IO CGType
foreign import ccall "dynamic" gtypeInit :: FunPtr GTypeInit -> GTypeInit

-- | Given the "type_init" function for a 'GType', try to resolve it
-- in the given list of loaded dynamic libraries, and error out if
-- that is not possible.
findGType :: [DynLib] -> Text -> IO GType
findGType libs typeInit = withCString (T.unpack typeInit) (go libs)
    where go :: [DynLib] -> CString -> IO GType
          go [] _ = error $ "Could not resolve " ++ show typeInit
          go ((DynLib l):ls) ctypeInit = do
            funPtr <- DLP.c_dlsym (DL.undl l) ctypeInit
            if funPtr /= nullFunPtr
            then GType <$> gtypeInit funPtr
            else go ls ctypeInit

-- | Unload the given loaded dynamic library.
unloadDynLib :: DynLib -> IO ()
unloadDynLib (DynLib dl) = DL.dlclose dl
