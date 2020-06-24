{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
-- | A minimal wrapper for libgirepository.
module Data.GI.CodeGen.LibGIRepository
    ( girRequire
    , Typelib
    , setupTypelibSearchPath
    , FieldInfo(..)
    , girStructFieldInfo
    , girUnionFieldInfo
    , girLoadGType
    , girIsSymbolResolvable
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad (forM, (>=>))
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString, withCString)
import Foreign (nullPtr, Ptr, FunPtr, peek)

import System.Environment (lookupEnv)
import System.FilePath (searchPathSeparator)

import Data.GI.Base.BasicConversions (withTextCString, cstringToText)
import Data.GI.Base.BasicTypes (TypedObject(..), GBoxed,
                                GType(..), CGType, ManagedPtr)
import Data.GI.Base.GError (GError, checkGError)
import Data.GI.Base.ManagedPtr (wrapBoxed, withManagedPtr)
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Base.Utils (allocMem, freeMem)
import Data.GI.CodeGen.Util (splitOn)

-- | Wrapper for 'GIBaseInfo'
newtype BaseInfo = BaseInfo (ManagedPtr BaseInfo)

-- | Wrapper for 'GITypelib', remembering the originating namespace
-- and version.
data Typelib = Typelib { typelibNamespace       :: Text
                       , typelibVersion         :: Text
                       , _typelibPtr            :: Ptr Typelib
                       }

instance Show Typelib where
  show t = T.unpack (typelibNamespace t) ++ "-" ++ T.unpack (typelibVersion t)

-- | Extra info about a field in a struct or union which is not easily
-- determined from the GIR file. (And which we determine by using
-- libgirepository.)
data FieldInfo = FieldInfo {
      fieldInfoOffset    :: Int
    }

-- | The (empty) set of parent types for `BaseInfo` visible to the
-- Haskell type system.
instance HasParentTypes BaseInfo
type instance ParentTypes BaseInfo = '[]

foreign import ccall "g_base_info_gtype_get_type" c_g_base_info_gtype_get_type :: IO GType

instance TypedObject BaseInfo where
  glibType = c_g_base_info_gtype_get_type

-- | `BaseInfo`s are registered as boxed in the GLib type system.
instance GBoxed BaseInfo

foreign import ccall "g_irepository_prepend_search_path" g_irepository_prepend_search_path :: CString -> IO ()

-- | Add the given directory to the typelib search path, this is a
-- thin wrapper over `g_irepository_prepend_search_path`.
girPrependSearchPath :: FilePath -> IO ()
girPrependSearchPath fp = withCString fp g_irepository_prepend_search_path

foreign import ccall "g_irepository_require" g_irepository_require ::
    Ptr () -> CString -> CString -> CInt -> Ptr (Ptr GError)
    -> IO (Ptr Typelib)

-- | A convenience function for setting up the typelib search path
-- from the environment. Note that for efficiency reasons this should
-- only be called once per program run. If the list of paths passed in
-- is empty, the environment variable @HASKELL_GI_TYPELIB_SEARCH_PATH@
-- will be checked. In either case the system directories will be
-- searched after the passed in directories.
setupTypelibSearchPath :: [FilePath] -> IO ()
setupTypelibSearchPath [] = do
  env <- lookupEnv "HASKELL_GI_TYPELIB_SEARCH_PATH"
  case env of
    Nothing -> return ()
    Just paths -> mapM_ girPrependSearchPath (splitOn searchPathSeparator paths)
setupTypelibSearchPath paths = mapM_ girPrependSearchPath paths

-- | Ensure that the given version of the namespace is loaded. If that
-- is not possible we error out.
girRequire :: Text -> Text -> IO Typelib
girRequire ns version =
    withTextCString ns $ \cns ->
    withTextCString version $ \cversion -> do
        typelib <- checkGError (g_irepository_require nullPtr cns cversion 0)
                               (\gerror -> error $ "Could not load typelib for "
                                           ++ show ns ++ " version "
                                           ++ show version ++ ".\n"
                                           ++ "Error was: " ++ show gerror)
        return (Typelib ns version typelib)

foreign import ccall "g_irepository_find_by_name" g_irepository_find_by_name ::
    Ptr () -> CString -> CString -> IO (Ptr BaseInfo)

-- | Find a given baseinfo by name, or give an error if it cannot be
-- found.
girFindByName :: Text -> Text -> IO BaseInfo
girFindByName ns name =
    withTextCString ns $ \cns ->
    withTextCString name $ \cname -> do
      ptr <- g_irepository_find_by_name nullPtr cns cname
      if ptr == nullPtr
      then error ("Could not find " ++ T.unpack ns ++ "::" ++ T.unpack name)
      else wrapBoxed BaseInfo ptr

foreign import ccall "g_field_info_get_offset" g_field_info_get_offset ::
    Ptr BaseInfo -> IO CInt
foreign import ccall "g_base_info_get_name" g_base_info_get_name ::
    Ptr BaseInfo -> IO CString

-- | Get the extra information for the given field.
getFieldInfo :: BaseInfo -> IO (Text, FieldInfo)
getFieldInfo field = withManagedPtr field $ \fi -> do
     fname <- (g_base_info_get_name fi >>= cstringToText)
     fOffset <- g_field_info_get_offset fi
     return (fname, FieldInfo { fieldInfoOffset = fromIntegral fOffset })

foreign import ccall "g_struct_info_get_size" g_struct_info_get_size ::
    Ptr BaseInfo -> IO CSize
foreign import ccall "g_struct_info_get_n_fields" g_struct_info_get_n_fields ::
    Ptr BaseInfo -> IO CInt
foreign import ccall "g_struct_info_get_field" g_struct_info_get_field ::
    Ptr BaseInfo -> CInt -> IO (Ptr BaseInfo)

-- | Find out the size of a struct, and the map from field names to
-- offsets inside the struct.
girStructFieldInfo :: Text -> Text -> IO (Int, M.Map Text FieldInfo)
girStructFieldInfo ns name = do
  baseinfo <- girFindByName ns name
  withManagedPtr baseinfo $ \si -> do
     size <- g_struct_info_get_size si
     nfields <- g_struct_info_get_n_fields si
     fieldInfos <- forM [0..(nfields-1)]
           (g_struct_info_get_field si >=> wrapBoxed BaseInfo >=> getFieldInfo)
     return (fromIntegral size, M.fromList fieldInfos)

foreign import ccall "g_union_info_get_size" g_union_info_get_size ::
    Ptr BaseInfo -> IO CSize
foreign import ccall "g_union_info_get_n_fields" g_union_info_get_n_fields ::
    Ptr BaseInfo -> IO CInt
foreign import ccall "g_union_info_get_field" g_union_info_get_field ::
    Ptr BaseInfo -> CInt -> IO (Ptr BaseInfo)

-- | Find out the size of a union, and the map from field names to
-- offsets inside the union.
girUnionFieldInfo :: Text -> Text -> IO (Int, M.Map Text FieldInfo)
girUnionFieldInfo ns name = do
  baseinfo <- girFindByName ns name
  withManagedPtr baseinfo $ \ui -> do
     size <- g_union_info_get_size ui
     nfields <- g_union_info_get_n_fields ui
     fieldInfos <- forM [0..(nfields-1)] (
           g_union_info_get_field ui >=> wrapBoxed BaseInfo >=> getFieldInfo)
     return (fromIntegral size, M.fromList fieldInfos)

foreign import ccall "g_typelib_symbol" g_typelib_symbol ::
    Ptr Typelib -> CString -> Ptr (FunPtr a) -> IO CInt

-- | Try to load a symbol from the dynamic library associated to the
-- given typelib.
girLookupSymbol :: forall a. Typelib -> Text -> IO (Maybe (FunPtr a))
girLookupSymbol (Typelib _ _ typelib) symbol = do
  funPtrPtr <- allocMem :: IO (Ptr (FunPtr a))
  result <- withTextCString symbol $ \csymbol ->
                      g_typelib_symbol typelib csymbol funPtrPtr
  funPtr <- peek funPtrPtr
  freeMem funPtrPtr
  if result /= 1
    then return Nothing
    else return (Just funPtr)

-- | Load a symbol from the dynamic library associated to the given
-- typelib. If the symbol does not exist this will raise an error.
girSymbol :: Typelib -> Text -> IO (FunPtr a)
girSymbol typelib@(Typelib ns version _) symbol = do
  maybeSymbol <- girLookupSymbol typelib symbol
  case maybeSymbol of
    Just funPtr -> return funPtr
    Nothing -> error ("Could not resolve symbol " ++ show symbol ++ " in namespace "
              ++ show (ns <> "-" <> version))

type GTypeInit = IO CGType
foreign import ccall "dynamic" gtypeInit :: FunPtr GTypeInit -> GTypeInit

-- | Load a GType given the `Typelib` where it lives and the type init
-- function.
girLoadGType :: Typelib -> Text -> IO GType
girLoadGType typelib typeInit =
  GType <$> (girSymbol typelib typeInit >>= gtypeInit)

-- | Check whether a symbol is present in the dynamical liberary.
girIsSymbolResolvable :: Typelib -> Text -> IO Bool
girIsSymbolResolvable typelib symbol = do
  maybeSymbol <- girLookupSymbol typelib symbol
  return (isJust maybeSymbol)
