module GI.Util
  ( maybeWithCString
  , getList
  , toFlags
  , split
  )
where

import Foreign
import Foreign.C

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString = maybe ($ nullPtr) withCString

getList :: (a -> IO CInt) -> (a -> CInt -> IO b) -> a -> IO [b]
getList getN getOne x = do
    n <- getN x
    mapM (getOne x) [0..n - 1]

toFlags :: Enum a => CInt -> [a]
toFlags n = loop n ((sizeOf n)*8 - 1) -- Number of bits in the argument
    where loop _ (-1) = []
          loop n e =
              let rest = loop n (e - 1)
               in if testBit n e then toEnum (2 ^ e) : rest else rest

split c s = split' s "" []
    where split' [] w ws = reverse (reverse w : ws)
          split' (x:xs) w ws =
              if x == c then split' xs "" (reverse w:ws)
                  else split' xs (x:w) ws
