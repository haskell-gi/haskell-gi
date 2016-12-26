module Data.GI.CodeGen.Util
  ( prime
  , parenthesize

  , padTo
  , withComment

  , ucFirst
  , lcFirst

  , modifyQualified

  , tshow
  , terror

  , splitOn
  ) where

import Data.Monoid ((<>))
import Data.Char (toLower, toUpper)
import Data.Text (Text)
import qualified Data.Text as T

padTo :: Int -> Text -> Text
padTo n s = s <> T.replicate (n - T.length s) " "

withComment :: Text -> Text -> Text
withComment a b = padTo 40 a <> "-- " <> b

prime :: Text -> Text
prime = (<> "'")

parenthesize :: Text -> Text
parenthesize s = "(" <> s <> ")"

-- | Construct the `Text` representation of a showable.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Throw an error with the given `Text`.
terror :: Text -> a
terror = error . T.unpack

-- | Capitalize the first character of the given string.
ucFirst :: Text -> Text
ucFirst "" = ""
ucFirst t = T.cons (toUpper $ T.head t) (T.tail t)

-- | Make the first character of the given string lowercase.
lcFirst :: Text -> Text
lcFirst "" = ""
lcFirst t = T.cons (toLower $ T.head t) (T.tail t)

-- | Apply the given modification function to the given symbol. If the
-- symbol is qualified the modification will only apply to the last
-- component.
modifyQualified :: (Text -> Text) -> Text -> Text
modifyQualified f = T.intercalate "." . modify . T.splitOn "."
    where modify :: [Text] -> [Text]
          modify [] = []
          modify (a:[]) = f a : []
          modify (a:as) = a : modify as

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)
