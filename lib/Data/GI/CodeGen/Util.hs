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

  , utf8ReadFile
  , utf8WriteFile

  , splitOn

  , printWarning
  ) where

import GHC.Stack (HasCallStack)

#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import Data.Char (toLower, toUpper)

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import qualified System.Console.ANSI as A
import System.IO (stderr, hFlush)

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

-- | Read a file assuming it is UTF-8 encoded. If decoding fails this
-- calls `error`.
utf8ReadFile :: FilePath -> IO T.Text
utf8ReadFile fname = do
  bytes <- B.readFile fname
  case TE.decodeUtf8' bytes of
    Right text -> return text
    Left error -> terror ("Input file " <> tshow fname <>
                          " seems not to be valid UTF-8. Error was:\n" <>
                          tshow error)

-- | Write the given `Text` into an UTF-8 encoded file.
utf8WriteFile :: FilePath -> T.Text -> IO ()
utf8WriteFile fname text = B.writeFile fname (TE.encodeUtf8 text)

-- | Print a (colored) warning message to stderr
printWarning :: Text -> IO ()
printWarning warning = do
  inColour <- A.hSupportsANSIColor stderr
  if not inColour
    then TIO.hPutStrLn stderr warning
    else do
      A.hSetSGR stderr [A.SetConsoleIntensity A.BoldIntensity,
                        A.SetColor A.Foreground A.Vivid A.Yellow]
      TIO.hPutStr stderr "Warning: "
      A.hSetSGR stderr [A.SetColor A.Foreground A.Vivid A.White]
      TIO.hPutStrLn stderr warning
      A.hSetSGR stderr [A.Reset]
      hFlush stderr

-- | Throw an error with the given `Text`.
terror :: HasCallStack => Text -> a
terror errMsg =
  let fmt = A.setSGRCode [A.SetConsoleIntensity A.BoldIntensity,
                          A.SetColor A.Foreground A.Vivid A.Red]
            ++ "ERROR: "
            ++ A.setSGRCode [A.SetColor A.Foreground A.Vivid A.White]
            ++ T.unpack errMsg
            ++ A.setSGRCode [A.SetConsoleIntensity A.NormalIntensity,
                             A.SetColor A.Foreground A.Vivid A.Blue]
            ++ "\nPlease report this at https://github.com/haskell-gi/haskell-gi/issues"
            ++ A.setSGRCode [A.Reset]
  in error fmt
