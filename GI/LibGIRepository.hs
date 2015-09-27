-- | A minimal wrapper for libgirepository.
module GI.LibGIRepository
    ( girRequire
    , girStructSizeAndOffsets
    , girUnionSizeAndOffsets
    ) where

import Control.Monad (void, forM)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString)
import Foreign (nullPtr, Ptr, ForeignPtr)

import GI.Utils.BasicConversions (withTextCString, cstringToText)
import GI.Utils.BasicTypes (BoxedObject(..), GType(..))
import GI.Utils.GError (GError, checkGError)
import GI.Utils.ManagedPtr (wrapBoxed, withManagedPtr)

-- | Wrapper for 'GIBaseInfo'
newtype BaseInfo = BaseInfo (ForeignPtr BaseInfo)

foreign import ccall "g_base_info_gtype_get_type" c_g_base_info_gtype_get_type :: IO GType

instance BoxedObject BaseInfo where
    boxedType _ = c_g_base_info_gtype_get_type

foreign import ccall "g_irepository_require" g_irepository_require ::
    Ptr () -> CString -> CString -> CInt -> Ptr (Ptr GError)
    -> IO (Ptr ())

-- | Ensure that the given version of the namespace is loaded. If that
-- is not possible we error out.
girRequire :: Text -> Text -> IO ()
girRequire ns version =
    withTextCString ns $ \cns ->
    withTextCString version $ \cversion ->
        void $ checkGError (g_irepository_require nullPtr cns cversion 0)
                           (error $ "Could not load typelib for " ++ show ns ++
                                      " version " ++ show version)

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

foreign import ccall "g_struct_info_get_size" g_struct_info_get_size ::
    Ptr BaseInfo -> IO CSize
foreign import ccall "g_struct_info_get_n_fields" g_struct_info_get_n_fields ::
    Ptr BaseInfo -> IO CInt
foreign import ccall "g_struct_info_get_field" g_struct_info_get_field ::
    Ptr BaseInfo -> CInt -> IO (Ptr BaseInfo)

-- | Find out the size of a struct, and the map from field names to
-- offsets inside the struct.
girStructSizeAndOffsets :: Text -> Text -> IO (Int, M.Map Text Int)
girStructSizeAndOffsets ns name = do
  baseinfo <- girFindByName ns name
  withManagedPtr baseinfo $ \si -> do
     size <- g_struct_info_get_size si
     nfields <- g_struct_info_get_n_fields si
     fieldOffsets <- forM [0..(nfields-1)] $ \i -> do
                       fieldInfo <- (g_struct_info_get_field si i
                                     >>= wrapBoxed BaseInfo)
                       withManagedPtr fieldInfo $ \fi -> do
                         fname <- (g_base_info_get_name fi >>= cstringToText)
                         fOffset <- g_field_info_get_offset fi
                         return (fname, fromIntegral fOffset)
     return (fromIntegral size, M.fromList fieldOffsets)


foreign import ccall "g_union_info_get_size" g_union_info_get_size ::
    Ptr BaseInfo -> IO CSize
foreign import ccall "g_union_info_get_n_fields" g_union_info_get_n_fields ::
    Ptr BaseInfo -> IO CInt
foreign import ccall "g_union_info_get_field" g_union_info_get_field ::
    Ptr BaseInfo -> CInt -> IO (Ptr BaseInfo)

-- | Find out the size of a union, and the map from field names to
-- offsets inside the union.
girUnionSizeAndOffsets :: Text -> Text -> IO (Int, M.Map Text Int)
girUnionSizeAndOffsets ns name = do
  baseinfo <- girFindByName ns name
  withManagedPtr baseinfo $ \ui -> do
     size <- g_union_info_get_size ui
     nfields <- g_union_info_get_n_fields ui
     fieldOffsets <- forM [0..(nfields-1)] $ \i -> do
                       fieldInfo <- (g_union_info_get_field ui i
                                     >>= wrapBoxed BaseInfo)
                       withManagedPtr fieldInfo $ \fi -> do
                         fname <- (g_base_info_get_name fi >>= cstringToText)
                         fOffset <- g_field_info_get_offset fi
                         return (fname, fromIntegral fOffset)
     return (fromIntegral size, M.fromList fieldOffsets)
