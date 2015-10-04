module GI.Util
  ( maybeWithCString
  , getList
  , toFlags
  , split

  , prime
  , unprime
  , parenthesize

  , padTo
  , withComment
  ) where

import Foreign
import Foreign.C

import Data.List (unfoldr)

padTo n s = s ++ replicate (n - length s) ' '
withComment a b = padTo 40 a ++ "-- " ++ b

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString = maybe ($ nullPtr) withCString

getList :: (a -> IO CInt) -> (a -> CInt -> IO b) -> a -> IO [b]
getList getN getOne x = do
    n <- getN x
    mapM (getOne x) [0..n - 1]

toFlags :: Enum a => CInt -> [a]
toFlags n = loop n (sizeOf n * 8 - 1) -- Number of bits in the argument
    where loop _ (-1) = []
          loop n e =
              let rest = loop n (e - 1)
               in if testBit n e then toEnum (2 ^ e) : rest else rest

-- Splits a string separated by the given separator into a list of
-- constituents. For example: split '.' "A.BC.D" = ["A", "BC", "D"]
split :: Char -> String -> [String]
split sep = unfoldr span'
    where span' :: String -> Maybe (String, String)
          span' [] = Nothing
          span' s@(x:xs)
              | x == sep  = Just $ span (/= sep) xs
              | otherwise = Just $ span (/= sep) s

prime :: String -> String
prime = (++ "'")

-- Remove the prime at the end of the given string
unprime :: String -> String
unprime "" = error "Empty variable to unprime!"
unprime primed =
    case last primed of
      '\'' -> init primed
      _ -> error $ primed ++ " is not primed!"

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"
