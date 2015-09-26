{-# LANGUAGE OverloadedStrings #-}

-- | Dynamically load/unload dynamic libraries. We use these libraries
-- to look up GTypes when generating code.
module GI.DynLib
    ( loadDynLib
    , DynLib
    , DynLibCache
    , newDynLibCache
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

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import GI.Utils.BasicTypes (GType(..), CGType)

-- | Opaque type encapsulating a loaded dynamic library.
newtype DynLib = DynLib DL.DL

-- | We keep track of the libraries that we have already loaded, in
-- order to avoid loading them twice.
newtype DynLibCache = DynLibCache (IORef (M.Map Text DynLib))

-- | A new (empty) cache of loaded dynamic libraries.
newDynLibCache :: IO DynLibCache
newDynLibCache = DynLibCache <$> newIORef M.empty

-- | Try to load a dynamic library.
loadDynLib :: Bool -> DynLibCache -> Text -> IO (Maybe DynLib)
loadDynLib verbose (DynLibCache libRef) name = do
  libCache <- readIORef libRef
  case M.lookup name libCache of
    Just l -> do
      when (verbose) $ putStrLn ("Loading " ++ show name ++ " from cache.")
      return (Just l)
    Nothing -> do
      when verbose $ putStrLn ("Loading dynlib " ++ show name)
      withCString (T.unpack name) $ \cstr -> do
        dl <- DLP.c_dlopen cstr (DLP.packRTLDFlags [DL.RTLD_LAZY, DL.RTLD_GLOBAL])
        if dl /= nullPtr
        then do
          let lib = (DynLib . DLP.DLHandle) dl
          writeIORef libRef (M.insert name lib libCache)
          (return . Just) lib
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
