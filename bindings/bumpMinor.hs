#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP

import ProjectInfo (ProjectInfo(..), prettyConfig)

bumpMinor :: Text -> Text
bumpMinor = T.intercalate "." . map (T.pack . show)
            . increaseMinor
            . map (read . T.unpack) . T.splitOn "."
            where increaseMinor :: [Int] -> [Int]
                  increaseMinor [] = []
                  increaseMinor [m] = [m+1]
                  increaseMinor (v:vs) = v : increaseMinor vs

modifyFile :: FilePath -> IO ()
modifyFile fname = do
  contents <- BL.readFile fname
  case A.eitherDecode contents of
    Left err -> error ("Could not parse " <> show fname <> ": " <> err)
    Right info ->
        BL.writeFile fname (AP.encodePretty' prettyConfig
                              (info {version = bumpMinor (version info)}))

main :: IO ()
main = getArgs >>= mapM_ modifyFile
