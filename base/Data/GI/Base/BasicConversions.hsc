{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Data.GI.Base.BasicConversions
    ( gflagsToWord
    , wordToGFlags

    , packGList
    , unpackGList
    , packGSList
    , unpackGSList
    , packGArray
    , unpackGArray
    , unrefGArray
    , packGPtrArray
    , unpackGPtrArray
    , unrefPtrArray
    , packGByteArray
    , unpackGByteArray
    , unrefGByteArray
    , packGHashTable
    , unpackGHashTable
    , unrefGHashTable
    , packByteString
    , packZeroTerminatedByteString
    , unpackByteStringWithLength
    , unpackZeroTerminatedByteString
    , packFileNameArray
    , packZeroTerminatedFileNameArray
    , unpackZeroTerminatedFileNameArray
    , unpackFileNameArrayWithLength
    , packUTF8CArray
    , packZeroTerminatedUTF8CArray
    , unpackUTF8CArrayWithLength
    , unpackZeroTerminatedUTF8CArray
    , packStorableArray
    , packZeroTerminatedStorableArray
    , unpackStorableArrayWithLength
    , unpackZeroTerminatedStorableArray
    , packMapStorableArray
    , packMapZeroTerminatedStorableArray
    , unpackMapStorableArrayWithLength
    , unpackMapZeroTerminatedStorableArray
    , packPtrArray
    , packZeroTerminatedPtrArray
    , unpackPtrArrayWithLength
    , unpackZeroTerminatedPtrArray
    , packBlockArray
    , unpackBlockArrayWithLength
    , unpackBoxedArrayWithLength

    , stringToCString
    , cstringToString
    , textToCString
    , withTextCString
    , cstringToText
    , byteStringToCString
    , cstringToByteString

    , mapZeroTerminatedCArray
    , mapCArrayWithLength
    , mapGArray
    , mapPtrArray
    , mapGList
    , mapGSList
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception.Base (bracket)
import Control.Monad (foldM)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF

import Foreign.Ptr (Ptr, plusPtr, nullPtr, nullFunPtr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import Foreign.C.Types (CInt(..), CUInt(..), CSize(..), CChar(..))
import Foreign.C.String (CString, withCString, peekCString)
import Data.Word
import Data.Int (Int32)
import Data.Bits (Bits, (.|.), (.&.), shift)

import Data.GI.Base.BasicTypes
import Data.GI.Base.CallStack (HasCallStack)
import Data.GI.Base.GHashTable (GEqualFunc, GHashFunc)
import Data.GI.Base.ManagedPtr (copyBoxedPtr)
import Data.GI.Base.Utils (allocBytes, callocBytes, memcpy, freeMem,
                           checkUnexpectedReturnNULL)

#include <glib-object.h>

gflagsToWord :: (Num b, IsGFlag a) => [a] -> b
gflagsToWord flags = fromIntegral (go flags)
    where go (f:fs) = fromEnum f .|. go fs
          go [] = 0

wordToGFlags :: (Storable a, Integral a, Bits a, IsGFlag b) => a -> [b]
wordToGFlags w = go 0
    where
      nbits = (sizeOf w)*8
      go k
          | k == nbits = []
          | otherwise = if mask .&. w /= 0
                        then toEnum (fromIntegral mask) : go (k+1)
                        else go (k+1)
          where mask = shift 1 k

foreign import ccall "g_list_prepend" g_list_prepend ::
    Ptr (GList (Ptr a)) -> Ptr a -> IO (Ptr (GList (Ptr a)))

-- | Given a Haskell list of items, construct a GList with those values.
packGList   :: [Ptr a] -> IO (Ptr (GList (Ptr a)))
packGList l = foldM g_list_prepend nullPtr $ reverse l

-- | Given a GSList construct the corresponding Haskell list.
unpackGList   :: Ptr (GList (Ptr a)) -> IO [Ptr a]
unpackGList gsl
    | gsl == nullPtr = return []
    | otherwise =
        do x <- peek (castPtr gsl)
           next <- peek (gsl `plusPtr` sizeOf x)
           xs <- unpackGList next
           return $ x : xs

-- Same thing for singly linked lists

foreign import ccall "g_slist_prepend" g_slist_prepend ::
    Ptr (GSList (Ptr a)) -> Ptr a -> IO (Ptr (GSList (Ptr a)))

-- | Given a Haskell list of items, construct a GSList with those values.
packGSList   :: [Ptr a] -> IO (Ptr (GSList (Ptr a)))
packGSList l = foldM g_slist_prepend nullPtr $ reverse l

-- | Given a GSList construct the corresponding Haskell list.
unpackGSList   :: Ptr (GSList (Ptr a)) -> IO [Ptr a]
unpackGSList gsl = unpackGList (castPtr gsl)

foreign import ccall "g_array_new" g_array_new ::
   CInt -> CInt -> CUInt -> IO (Ptr (GArray ()))
foreign import ccall "g_array_set_size" g_array_set_size ::
    Ptr (GArray ()) -> CUInt -> IO (Ptr (GArray ()))
foreign import ccall "g_array_unref" unrefGArray ::
   Ptr (GArray a) -> IO ()

packGArray :: forall a. Storable a => [a] -> IO (Ptr (GArray a))
packGArray elems = do
  let elemsize = sizeOf (elems!!0)
  array <- g_array_new 0 0 (fromIntegral elemsize)
  _ <- g_array_set_size array (fromIntegral $ length elems)
  dataPtr <- peek (castPtr array :: Ptr (Ptr a))
  fill dataPtr elems
  return $ castPtr array
  where
    fill            :: Ptr a -> [a] -> IO ()
    fill _ []       = return ()
    fill ptr (x:xs) =
        do poke ptr x
           fill (ptr `plusPtr` (sizeOf x)) xs

unpackGArray :: forall a. Storable a => Ptr (GArray a) -> IO [a]
unpackGArray array = do
  dataPtr <- peek (castPtr array :: Ptr (Ptr a))
  nitems <- peek (array `plusPtr` sizeOf dataPtr)
  go dataPtr nitems
    where go :: Ptr a -> Int -> IO [a]
          go _ 0 = return []
          go ptr n = do
            x <- peek ptr
            (x:) <$> go (ptr `plusPtr` sizeOf x) (n-1)

foreign import ccall "g_ptr_array_new" g_ptr_array_new ::
    IO (Ptr (GPtrArray ()))
foreign import ccall "g_ptr_array_set_size" g_ptr_array_set_size ::
    Ptr (GPtrArray ()) -> CUInt -> IO (Ptr (GPtrArray ()))
foreign import ccall "g_ptr_array_unref" unrefPtrArray ::
   Ptr (GPtrArray a) -> IO ()

packGPtrArray :: [Ptr a] -> IO (Ptr (GPtrArray (Ptr a)))
packGPtrArray elems = do
  array <- g_ptr_array_new
  _ <- g_ptr_array_set_size array (fromIntegral $ length elems)
  dataPtr <- peek (castPtr array :: Ptr (Ptr (Ptr a)))
  fill dataPtr elems
  return $ castPtr array
  where
    fill            :: Ptr (Ptr a) -> [Ptr a] -> IO ()
    fill _ []       = return ()
    fill ptr (x:xs) =
        do poke ptr x
           fill (ptr `plusPtr` (sizeOf x)) xs

unpackGPtrArray :: Ptr (GPtrArray (Ptr a)) -> IO [Ptr a]
unpackGPtrArray array = do
  dataPtr <- peek (castPtr array :: Ptr (Ptr (Ptr a)))
  nitems <- peek (array `plusPtr` sizeOf dataPtr)
  go dataPtr nitems
    where go :: Ptr (Ptr a) -> CUInt -> IO [Ptr a]
          go _ 0 = return []
          go ptr n = do
            x <- peek ptr
            (x:) <$> go (ptr `plusPtr` sizeOf x) (n-1)

foreign import ccall "g_byte_array_new" g_byte_array_new ::
    IO (Ptr GByteArray)
foreign import ccall "g_byte_array_append" g_byte_array_append ::
    Ptr GByteArray -> Ptr a -> CUInt -> IO (Ptr GByteArray)
foreign import ccall "g_byte_array_unref" unrefGByteArray ::
   Ptr GByteArray -> IO ()

packGByteArray :: ByteString -> IO (Ptr GByteArray)
packGByteArray bs = do
  array <- g_byte_array_new
  let (ptr, offset, length) = BI.toForeignPtr bs
  _ <- withForeignPtr ptr $ \dataPtr ->
                    g_byte_array_append array (dataPtr `plusPtr` offset)
                                        (fromIntegral length)
  return array

unpackGByteArray :: Ptr GByteArray -> IO ByteString
unpackGByteArray array = do
  dataPtr <- peek (castPtr array :: Ptr (Ptr CChar))
  length <- peek (array `plusPtr` (sizeOf dataPtr)) :: IO CUInt
  B.packCStringLen (dataPtr, fromIntegral length)

foreign import ccall "g_hash_table_new_full" g_hash_table_new_full ::
    GHashFunc a -> GEqualFunc a -> GDestroyNotify a -> GDestroyNotify b ->
                 IO (Ptr (GHashTable a b))
foreign import ccall "g_hash_table_insert" g_hash_table_insert ::
    Ptr (GHashTable a b) -> PtrWrapped a -> PtrWrapped b -> IO #{type gboolean}

packGHashTable :: GHashFunc a -> GEqualFunc a ->
                  Maybe (GDestroyNotify a) -> Maybe (GDestroyNotify b) ->
                  [(PtrWrapped a, PtrWrapped b)] -> IO (Ptr (GHashTable a b))
packGHashTable keyHash keyEqual keyDestroy elemDestroy pairs = do
  let keyDPtr = fromMaybe nullFunPtr keyDestroy
      elemDPtr = fromMaybe nullFunPtr elemDestroy
  ht <- g_hash_table_new_full keyHash keyEqual keyDPtr elemDPtr
  mapM_ (uncurry (g_hash_table_insert ht)) pairs
  return ht

foreign import ccall "g_hash_table_get_keys" g_hash_table_get_keys ::
    Ptr (GHashTable a b) -> IO (Ptr (GList (Ptr a)))
foreign import ccall "g_hash_table_lookup" g_hash_table_lookup ::
    Ptr (GHashTable a b) -> PtrWrapped a -> IO (PtrWrapped b)
unpackGHashTable :: Ptr (GHashTable a b) -> IO [(PtrWrapped a, PtrWrapped b)]
unpackGHashTable ht = do
  keysGList <- g_hash_table_get_keys ht
  keys <- (map (PtrWrapped . castPtr)) <$> unpackGList keysGList
  g_list_free keysGList
  -- At this point we could use g_hash_table_get_values, since the
  -- current implementation in GLib returns elements in the same order
  -- as g_hash_table_get_keys. But to be on the safe side, since the
  -- ordering is not specified in the documentation, we do the
  -- following, which is (quite) slower but manifestly safe.
  elems <- mapM (g_hash_table_lookup ht) keys
  return (zip keys elems)

foreign import ccall "g_hash_table_unref" unrefGHashTable ::
   Ptr (GHashTable a b) -> IO ()

packByteString :: ByteString -> IO (Ptr Word8)
packByteString bs = do
  let (ptr, offset, length) = BI.toForeignPtr bs
  mem <- allocBytes length
  withForeignPtr ptr $ \dataPtr ->
      memcpy mem (dataPtr `plusPtr` offset) (fromIntegral length)
  return mem

packZeroTerminatedByteString :: ByteString -> IO (Ptr Word8)
packZeroTerminatedByteString bs = do
  let (ptr, offset, length) = BI.toForeignPtr bs
  mem <- allocBytes (length+1)
  withForeignPtr ptr $ \dataPtr ->
      memcpy mem (dataPtr `plusPtr` offset) (fromIntegral length)
  poke (mem `plusPtr` (offset+length)) (0 :: Word8)
  return mem

unpackByteStringWithLength :: Integral a => a -> Ptr Word8 -> IO ByteString
unpackByteStringWithLength length ptr =
  B.packCStringLen (castPtr ptr, fromIntegral length)

unpackZeroTerminatedByteString :: Ptr Word8 -> IO ByteString
unpackZeroTerminatedByteString ptr =
  B.packCString (castPtr ptr)

packStorableArray :: Storable a => [a] -> IO (Ptr a)
packStorableArray = packMapStorableArray id

packZeroTerminatedStorableArray :: (Num a, Storable a) => [a] -> IO (Ptr a)
packZeroTerminatedStorableArray = packMapZeroTerminatedStorableArray id

unpackStorableArrayWithLength :: (Integral a, Storable b) =>
                                 a -> Ptr b -> IO [b]
unpackStorableArrayWithLength = unpackMapStorableArrayWithLength id

unpackZeroTerminatedStorableArray :: (Eq a, Num a, Storable a) =>
                                     Ptr a -> IO [a]
unpackZeroTerminatedStorableArray = unpackMapZeroTerminatedStorableArray id

packMapStorableArray :: forall a b. Storable b => (a -> b) -> [a] -> IO (Ptr b)
packMapStorableArray fn items = do
  let nitems = length items
  mem <- allocBytes $ (sizeOf (undefined::b)) * nitems
  fill mem (map fn items)
  return mem
  where fill            :: Ptr b -> [b] -> IO ()
        fill _ []       = return ()
        fill ptr (x:xs) = do
          poke ptr x
          fill (ptr `plusPtr` sizeOf x) xs

packMapZeroTerminatedStorableArray :: forall a b. (Num b, Storable b) =>
                                      (a -> b) -> [a] -> IO (Ptr b)
packMapZeroTerminatedStorableArray fn items = do
  let nitems = length items
  mem <- allocBytes $ (sizeOf (undefined::b)) * (nitems+1)
  fill mem (map fn items)
  return mem
  where fill            :: Ptr b -> [b] -> IO ()
        fill ptr []     = poke ptr 0
        fill ptr (x:xs) = do
          poke ptr x
          fill (ptr `plusPtr` sizeOf x) xs

unpackMapStorableArrayWithLength :: forall a b c. (Integral a, Storable b) =>
                                    (b -> c) -> a -> Ptr b -> IO [c]
unpackMapStorableArrayWithLength fn n ptr = map fn <$> go (fromIntegral n) ptr
    where go :: Int -> Ptr b -> IO [b]
          go 0 _ = return []
          go n ptr = do
            x <- peek ptr
            (x:) <$> go (n-1) (ptr `plusPtr` sizeOf x)

unpackMapZeroTerminatedStorableArray :: forall a b. (Eq a, Num a, Storable a) =>
                                        (a -> b) -> Ptr a -> IO [b]
unpackMapZeroTerminatedStorableArray fn ptr = map fn <$> go ptr
    where go :: Ptr a -> IO [a]
          go ptr = do
            x <- peek ptr
            if x == 0
            then return []
            else (x:) <$> go (ptr `plusPtr` sizeOf x)

packUTF8CArray :: [Text] -> IO (Ptr CString)
packUTF8CArray items = do
  let nitems = length items
  mem <- allocBytes $ nitems * (sizeOf (nullPtr :: CString))
  fill mem items
  return mem
    where fill            :: Ptr CString -> [Text] -> IO ()
          fill _ []       = return ()
          fill ptr (x:xs) =
              do cstring <- textToCString x
                 poke ptr cstring
                 fill (ptr `plusPtr` sizeOf cstring) xs

packZeroTerminatedUTF8CArray :: [Text] -> IO (Ptr CString)
packZeroTerminatedUTF8CArray items = do
    let nitems = length items
    mem <- allocBytes $ (sizeOf (nullPtr :: CString)) * (nitems+1)
    fill mem items
    return mem
    where fill :: Ptr CString -> [Text] -> IO ()
          fill ptr [] = poke ptr nullPtr
          fill ptr (x:xs) = do cstring <- textToCString x
                               poke ptr cstring
                               fill (ptr `plusPtr` sizeOf cstring) xs

unpackZeroTerminatedUTF8CArray :: HasCallStack => Ptr CString -> IO [Text]
unpackZeroTerminatedUTF8CArray listPtr = go listPtr
    where go :: Ptr CString -> IO [Text]
          go ptr = do
            cstring <- peek ptr
            if cstring == nullPtr
               then return []
               else (:) <$> cstringToText cstring
                        <*> go (ptr `plusPtr` sizeOf cstring)

unpackUTF8CArrayWithLength :: (HasCallStack, Integral a) =>
                              a -> Ptr CString -> IO [Text]
unpackUTF8CArrayWithLength n ptr = go (fromIntegral n) ptr
    where go       :: Int -> Ptr CString -> IO [Text]
          go 0 _   = return []
          go n ptr = do
            cstring <- peek ptr
            (:) <$> cstringToText cstring
                    <*> go (n-1) (ptr `plusPtr` sizeOf cstring)

packFileNameArray :: [String] -> IO (Ptr CString)
packFileNameArray items = do
  let nitems = length items
  mem <- allocBytes $ nitems * (sizeOf (nullPtr :: CString))
  fill mem items
  return mem
    where fill            :: Ptr CString -> [String] -> IO ()
          fill _ []       = return ()
          fill ptr (x:xs) =
              do cstring <- stringToCString x
                 poke ptr cstring
                 fill (ptr `plusPtr` sizeOf cstring) xs

packZeroTerminatedFileNameArray :: [String] -> IO (Ptr CString)
packZeroTerminatedFileNameArray items = do
    let nitems = length items
    mem <- allocBytes $ (sizeOf (nullPtr :: CString)) * (nitems+1)
    fill mem items
    return mem
    where fill :: Ptr CString -> [String] -> IO ()
          fill ptr [] = poke ptr nullPtr
          fill ptr (x:xs) = do cstring <- stringToCString x
                               poke ptr cstring
                               fill (ptr `plusPtr` sizeOf cstring) xs

unpackZeroTerminatedFileNameArray :: HasCallStack => Ptr CString -> IO [String]
unpackZeroTerminatedFileNameArray listPtr = go listPtr
    where go :: Ptr CString -> IO [String]
          go ptr = do
            cstring <- peek ptr
            if cstring == nullPtr
               then return []
               else (:) <$> cstringToString cstring
                        <*> go (ptr `plusPtr` sizeOf cstring)

unpackFileNameArrayWithLength :: (HasCallStack, Integral a) =>
                                 a -> Ptr CString -> IO [String]
unpackFileNameArrayWithLength n ptr = go (fromIntegral n) ptr
    where go       :: Int -> Ptr CString -> IO [String]
          go 0 _   = return []
          go n ptr = do
            cstring <- peek ptr
            (:) <$> cstringToString cstring
                    <*> go (n-1) (ptr `plusPtr` sizeOf cstring)

foreign import ccall "g_strdup" g_strdup :: CString -> IO CString

-- We need to use the GLib allocator for constructing CStrings, since
-- the ownership of the string may be transferred to the GLib side,
-- which will free it with g_free.
stringToCString :: String -> IO CString
stringToCString str = withCString str g_strdup

cstringToString :: HasCallStack => CString -> IO String
cstringToString cstr = do
  checkUnexpectedReturnNULL (T.pack "cstringToString") cstr
  peekCString cstr

foreign import ccall "g_strndup" g_strndup ::
    CString -> #{type gsize} -> IO CString

-- | Convert `Text` into a `CString`, using the GLib allocator.
textToCString :: Text -> IO CString
textToCString str = TF.withCStringLen str $ \(cstr, len) ->
  -- Because withCStringLen returns NULL for a zero-length Text, and
  -- g_strndup returns NULL for NULL, even if n==0.
  if cstr /= nullPtr
  then g_strndup cstr (fromIntegral len)
  else callocBytes 1

withTextCString :: Text -> (CString -> IO a) -> IO a
withTextCString text action = bracket (textToCString text) freeMem action

foreign import ccall "strlen" c_strlen ::
    CString -> IO (CSize)

cstringToText :: HasCallStack => CString -> IO Text
cstringToText cstr = do
  checkUnexpectedReturnNULL (T.pack "cstringToText") cstr
  len <- c_strlen cstr
  let cstrlen = (cstr, fromIntegral len)
  TF.peekCStringLen cstrlen

byteStringToCString :: ByteString -> IO CString
byteStringToCString bs = B.useAsCString bs g_strdup

cstringToByteString :: HasCallStack => CString -> IO ByteString
cstringToByteString cstr = do
  checkUnexpectedReturnNULL (T.pack "cstringToByteString") cstr
  B.packCString cstr

packPtrArray :: [Ptr a] -> IO (Ptr (Ptr a))
packPtrArray items = do
  let nitems = length items
  mem <- allocBytes $ (sizeOf (nullPtr :: Ptr a)) * nitems
  fill mem items
  return mem
  where fill :: Ptr (Ptr a) -> [Ptr a] -> IO ()
        fill _ [] = return ()
        fill ptr (x:xs) = do poke ptr x
                             fill (ptr `plusPtr` sizeOf x) xs

packZeroTerminatedPtrArray :: [Ptr a] -> IO (Ptr (Ptr a))
packZeroTerminatedPtrArray items = do
  let nitems = length items
  mem <- allocBytes $ (sizeOf (nullPtr :: Ptr a)) * (nitems+1)
  fill mem items
  return mem
  where fill            :: Ptr (Ptr a) -> [Ptr a] -> IO ()
        fill ptr []     = poke ptr nullPtr
        fill ptr (x:xs) = do poke ptr x
                             fill (ptr `plusPtr` sizeOf x) xs

unpackPtrArrayWithLength :: Integral a => a -> Ptr (Ptr b) -> IO [Ptr b]
unpackPtrArrayWithLength n ptr = go (fromIntegral n) ptr
    where go       :: Int -> Ptr (Ptr a) -> IO [Ptr a]
          go 0 _   = return []
          go n ptr = (:) <$> peek ptr
                     <*> go (n-1) (ptr `plusPtr` sizeOf (nullPtr :: Ptr a))

unpackZeroTerminatedPtrArray :: Ptr (Ptr a) -> IO [Ptr a]
unpackZeroTerminatedPtrArray ptr = go ptr
    where go :: Ptr (Ptr a) -> IO [Ptr a]
          go ptr = do
            p <- peek ptr
            if p == nullPtr
            then return []
            else (p:) <$> go (ptr `plusPtr` sizeOf p)

mapZeroTerminatedCArray :: (Ptr a -> IO b) -> Ptr (Ptr a) -> IO ()
mapZeroTerminatedCArray f dataPtr
    | (dataPtr == nullPtr) = return ()
    | otherwise =
        do ptr <- peek dataPtr
           if ptr == nullPtr
           then return ()
           else do
             _ <- f ptr
             mapZeroTerminatedCArray f (dataPtr `plusPtr` sizeOf ptr)

-- | Given a set of pointers to blocks of memory of the specified
-- size, copy the contents of these blocks to a freshly-allocated
-- (with `allocBytes`) continuous area of memory.
packBlockArray :: Int -> [Ptr a] -> IO (Ptr a)
packBlockArray size items = do
  let nitems = length items
  mem <- allocBytes $ size * nitems
  fill mem items
  return mem
  where fill :: Ptr a -> [Ptr a] -> IO ()
        fill _ [] = return ()
        fill ptr (x:xs) = do memcpy ptr x size
                             fill (ptr `plusPtr` size) xs

foreign import ccall "g_memdup" g_memdup ::
    Ptr a -> CUInt -> IO (Ptr a)

unpackBlockArrayWithLength :: Integral a => Int -> a -> Ptr b -> IO [Ptr b]
unpackBlockArrayWithLength size n ptr = go size (fromIntegral n) ptr
    where go       :: Int -> Int -> Ptr b -> IO [Ptr b]
          go _ 0 _   = return []
          go size n ptr = do
            buf <- g_memdup ptr (fromIntegral size)
            (buf :) <$> go size (n-1) (ptr `plusPtr` size)

unpackBoxedArrayWithLength :: forall a b. (Integral a, GBoxed b) =>
                              Int -> a -> Ptr b -> IO [Ptr b]
unpackBoxedArrayWithLength size n ptr = go size (fromIntegral n) ptr
    where go       :: Int -> Int -> Ptr b -> IO [Ptr b]
          go _ 0 _   = return []
          go size n ptr = do
            buf <- copyBoxedPtr ptr
            (buf :) <$> go size (n-1) (ptr `plusPtr` size)

mapCArrayWithLength :: (Storable a, Integral b) =>
                       b -> (a -> IO c) -> Ptr a -> IO ()
mapCArrayWithLength n f dataPtr
    | (dataPtr == nullPtr) = return ()
    | (n <= 0) = return ()
    | otherwise =
        do ptr <- peek dataPtr
           _ <- f ptr
           mapCArrayWithLength (n-1) f (dataPtr `plusPtr` sizeOf ptr)

mapGArray :: forall a b. Storable a => (a -> IO b) -> Ptr (GArray a) -> IO ()
mapGArray f array
    | (array == nullPtr) = return ()
    | otherwise =
        do dataPtr <- peek (castPtr array :: Ptr (Ptr a))
           nitems <- peek (array `plusPtr` sizeOf dataPtr)
           go dataPtr nitems
               where go :: Ptr a -> Int -> IO ()
                     go _ 0 = return ()
                     go ptr n = do
                       x <- peek ptr
                       _ <- f x
                       go (ptr `plusPtr` sizeOf x) (n-1)

mapPtrArray :: (Ptr a -> IO b) -> Ptr (GPtrArray (Ptr a)) -> IO ()
mapPtrArray f array = mapGArray f (castPtr array)

mapGList :: (Ptr a -> IO b) -> Ptr (GList (Ptr a)) -> IO ()
mapGList f glist
    | (glist == nullPtr) = return ()
    | otherwise =
        do ptr <- peek (castPtr glist)
           next <- peek (glist `plusPtr` sizeOf ptr)
           _ <- f ptr
           mapGList f next

mapGSList :: (Ptr a -> IO b) -> Ptr (GSList (Ptr a)) -> IO ()
mapGSList f gslist = mapGList f (castPtr gslist)
