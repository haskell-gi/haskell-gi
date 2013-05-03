module GI.Internal.BasicTypes
    ( Type
    , GArray(..)
    , GHashTable(..)
    , GList(..)
    , packGList
    , unpackGList
    , GSList(..)
    , packGSList
    , unpackGSList
    ) where

import Foreign
import Control.Monad (foldM)

type Type = Word
data GArray a = GArray (Ptr (GArray a))
data GHashTable a b = GHashTable (Ptr (GHashTable a b))
data GList a = GList (Ptr (GList a))
data GSList a = GSList (Ptr (GSList a))

foreign import ccall unsafe "g_list_prepend" g_list_prepend ::
    Ptr (GList (Ptr a)) -> Ptr a -> IO (Ptr (GList (Ptr a)))

foreign import ccall unsafe "g_list_reverse" g_list_reverse ::
    Ptr (GList a) -> IO (Ptr (GList a))

foreign import ccall unsafe "g_list_delete_link" g_list_delete_link ::
    Ptr (GList a) -> Ptr (GList a) -> IO (Ptr (GList a))

-- Given a Haskell list of items, construct a GList with those values.
packGList   :: [Ptr a] -> IO (Ptr (GList (Ptr a)))
packGList l = foldM g_list_prepend nullPtr $ reverse l

-- Given a GList construct the corresponding Haskell list.
unpackGList   :: Ptr (GList (Ptr a)) -> IO [Ptr a]
unpackGList gl = do
  glist' <- g_list_reverse gl
  extractList glist' []
    where
      extractList gl xs
        | gl == nullPtr = return xs
        | otherwise     = do
            x <- peek (castPtr gl)
            gl' <- g_list_delete_link gl gl
            extractList gl' (x : xs)

-- Same thing for singly linked lists

foreign import ccall unsafe "g_slist_prepend" g_slist_prepend ::
    Ptr (GSList (Ptr a)) -> Ptr a -> IO (Ptr (GSList (Ptr a)))

foreign import ccall unsafe "g_slist_reverse" g_slist_reverse ::
    Ptr (GSList a) -> IO (Ptr (GSList a))

foreign import ccall unsafe "g_slist_delete_link" g_slist_delete_link ::
    Ptr (GSList a) -> Ptr (GSList a) -> IO (Ptr (GSList a))

-- Given a Haskell list of items, construct a GSList with those values.
packGSList   :: [Ptr a] -> IO (Ptr (GSList (Ptr a)))
packGSList l = foldM g_slist_prepend nullPtr $ reverse l

-- Given a GSList construct the corresponding Haskell list.
unpackGSList   :: Ptr (GSList (Ptr a)) -> IO [Ptr a]
unpackGSList gsl
    | gsl == nullPtr = return []
    | otherwise = 
        do x <- peek (castPtr gsl)
           next <- peekElemOff (castPtr gsl) 1 :: IO (Ptr (GSList (Ptr a)))
           xs <- unpackGSList next
           return $ x : xs