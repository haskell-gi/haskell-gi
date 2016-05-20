#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

import System.Environment (getArgs)

import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)

bumpMinor :: Text -> Text
bumpMinor = T.intercalate "." . map (T.pack . show)
            . increaseMinor
            . map (read . T.unpack) . T.splitOn "."
            where increaseMinor :: [Int] -> [Int]
                  increaseMinor [] = []
                  increaseMinor [m] = [m+1]
                  increaseMinor (v:vs) = v : increaseMinor vs

increaseVersion :: Text -> Text
increaseVersion (T.stripPrefix "version:" -> Just v) =
    "version:            " <> bumpMinor v
increaseVersion other = other

modifyFile :: FilePath -> IO ()
modifyFile fp = do
  contents <- TIO.readFile fp
  TIO.writeFile fp (T.unlines . map increaseVersion . T.lines $ contents)

main :: IO ()
main = getArgs >>= mapM_ modifyFile
