#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))

import Control.Monad (forM_)
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP

import ProjectInfo (ProjectInfo(..), emptyProjectInfo)

cabalProcessLine :: Text -> ProjectInfo -> ProjectInfo
cabalProcessLine (T.stripPrefix "name:" -> Just n) info =
    info {name = T.strip n}
cabalProcessLine (T.stripPrefix "version:" -> Just v) info =
    info {version = T.strip v}
cabalProcessLine (T.stripPrefix "synopsis:" -> Just s) info =
    info {synopsis = T.strip s}
cabalProcessLine (T.stripPrefix "description:" -> Just d) info =
    info {description = T.strip d}
cabalProcessLine (T.stripPrefix "pkgconfig-depends:" -> Just p) info =
    info {pkgconfigDepends = T.strip p}
cabalProcessLine (T.splitOn "==" -> [T.stripPrefix "gi-" -> Just p,
                                     T.stripSuffix "," -> Just ver]) info =
    info {giDepends = giDepends info ++ ["gi-" <> p <> "==" <> ver]}
cabalProcessLine (T.stripPrefix "build-depends: base >=" -> Just bv) info =
    info {baseVersion = case T.stripSuffix "," bv of
                          Nothing -> "base >=" <> bv
                          Just bv' -> "base >=" <> bv' }
cabalProcessLine (T.stripPrefix "base >=" -> Just bv) info =
    info {baseVersion = case T.stripSuffix "," bv of
                          Nothing -> "base >=" <> bv
                          Just bv' -> "base >=" <> bv' }
cabalProcessLine _ info = info

extractCabalInfo :: FilePath -> IO ProjectInfo
extractCabalInfo fname = do
  buf <- TIO.readFile fname
  return (foldr (cabalProcessLine . T.strip) emptyProjectInfo (T.lines buf))

setupProcessLine :: Text -> ProjectInfo -> ProjectInfo
setupProcessLine (T.stripPrefix "where name = " -> Just n) info =
    info {girName = read (T.unpack n)}
setupProcessLine (T.stripPrefix "version = " -> Just v) info =
    info {girVersion = read (T.unpack v)}
setupProcessLine (T.stripPrefix "overridesFile = " -> Just o) info =
    info {girOverrides = read (T.unpack o)}
setupProcessLine _ info = info

extractSetupInfo :: FilePath -> ProjectInfo -> IO ProjectInfo
extractSetupInfo fname info = do
  buf <- TIO.readFile fname
  return (foldr (setupProcessLine . T.strip) info (T.lines buf))

writeGIInfo :: FilePath -> ProjectInfo -> IO ()
writeGIInfo fname cabalInfo =
    B.writeFile fname (AP.encodePretty' config cabalInfo)
     where config = AP.Config { AP.confIndent = 4
                              , AP.confCompare = AP.keyOrder
                                ["name", "version", "description", "synopsis",
                                 "girName", "girVersion",
                                 "girOverrides", "pkgConfig", "depends",
                                 "baseVersion"]}

main :: IO ()
main = do
  cabals <- getArgs
  forM_ cabals $ \cabal -> do
         let dir = takeDirectory cabal
             setup = dir </> "Setup.hs"
         info <- extractCabalInfo cabal >>= extractSetupInfo setup

         writeGIInfo (dir </> "pkg.info") info
