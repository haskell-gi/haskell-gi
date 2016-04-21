{-# LANGUAGE ScopedTypeVariables #-}
-- | A minimal wrapper for libgirepository.
module Data.GI.CodeGen.LibGIRepository
    ( girRequire
    , girStructSizeAndOffsets
    , girUnionSizeAndOffsets
    , girLoadGType
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad (forM, when)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString)
import Foreign (nullPtr, Ptr, ForeignPtr, FunPtr, peek)

import Data.GI.Base.BasicConversions (withTextCString, cstringToText)
import Data.GI.Base.BasicTypes (BoxedObject(..), GType(..), CGType)
import Data.GI.Base.GError (GError, checkGError)
import Data.GI.Base.ManagedPtr (wrapBoxed, withManagedPtr)
import Data.GI.Base.Utils (allocMem, freeMem)

-- | Wrapper for 'GIBaseInfo'
newtype BaseInfo = BaseInfo (ForeignPtr BaseInfo)

-- | Wrapper for 'GITypelib'
newtype Typelib = Typelib (Ptr Typelib)

foreign import ccall "g_base_info_gtype_get_type" c_g_base_info_gtype_get_type :: IO GType

instance BoxedObject BaseInfo where
    boxedType _ = c_g_base_info_gtype_get_type

foreign import ccall "g_irepository_require" g_irepository_require ::
    Ptr () -> CString -> CString -> CInt -> Ptr (Ptr GError)
    -> IO (Ptr Typelib)

-- | Ensure that the given version of the namespace is loaded. If that
-- is not possible we error out.
girRequire :: Text -> Text -> IO Typelib
girRequire ns version =
    withTextCString ns $ \cns ->
    withTextCString version $ \cversion -> do
        typelib <- checkGError (g_irepository_require nullPtr cns cversion 0)
                               (error $ "Could not load typelib for "
                                          ++ show ns ++ " version "
                                          ++ show version)
        return (Typelib typelib)

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

foreign import ccall "g_typelib_symbol" g_typelib_symbol ::
    Ptr Typelib -> CString -> Ptr (FunPtr a) -> IO CInt

-- | Load a symbol from the dynamic library associated to the given namespace.
girSymbol :: forall a. Text -> Text -> IO (FunPtr a)
girSymbol ns symbol = do
  typelib <- withTextCString ns $ \cns ->
                    checkGError (g_irepository_require nullPtr cns nullPtr 0)
                                (error $ "Could not load typelib " ++ show ns)
  funPtrPtr <- allocMem :: IO (Ptr (FunPtr a))
  result <- withTextCString symbol $ \csymbol ->
                      g_typelib_symbol typelib csymbol funPtrPtr
  when (result /= 1) $
       error ("Could not resolve symbol " ++ show symbol ++ " in namespace "
              ++ show ns)
  funPtr <- peek funPtrPtr
  freeMem funPtrPtr
  return funPtr

type GTypeInit = IO CGType
foreign import ccall "dynamic" gtypeInit :: FunPtr GTypeInit -> GTypeInit

-- | Load a GType given the namespace where it lives and the type init
-- function.
girLoadGType :: Text -> Text -> IO GType
girLoadGType ns typeInit = do
  funPtr <- girSymbol ns typeInit
  GType <$> gtypeInit funPtr
