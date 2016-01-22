module GI.Util
  ( prime
  , parenthesize

  , padTo
  , withComment

  , ucFirst
  , lcFirst

  , tshow
  , terror
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
ucFirst "" = terror "ucFirst: empty text!"
ucFirst t = T.cons (toUpper $ T.head t) (T.tail t)

-- | Make the first character of the given string lowercase.
lcFirst :: Text -> Text
lcFirst "" = terror "lcFirst: empty string"
lcFirst t = T.cons (toLower $ T.head t) (T.tail t)
