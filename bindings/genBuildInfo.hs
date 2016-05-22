#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings, ViewPatterns, DeriveGeneric, DeriveAnyClass #-}

-- | Generate the cabal info for the given subdirectories, assuming
-- the existence of appropriate "GIR.info" files.

import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))

import Control.Monad (forM_)
import GHC.Generics

import qualified Data.Aeson as A
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.GI.CodeGen.ProjectInfo as PI

import ProjectInfo (ProjectInfo(..))

import qualified Data.ByteString.Lazy as LB

readGIRInfo :: FilePath -> IO ProjectInfo
readGIRInfo fname = do
  buf <- LB.readFile fname
  case A.eitherDecode buf of
    Left err -> error ("Could not parse \"" <> fname <> "\": " <> err)
    Right info -> return info

writeCabal :: FilePath -> ProjectInfo -> IO ()
writeCabal fname info =
    TIO.writeFile fname $ T.unlines
       [ "name:                 " <> name info
       , "version:              " <> version info
       , "synopsis:             " <> synopsis info
       , "description:          " <> description info
       , "homepage:             " <> PI.homepage
       , "license:              " <> PI.license
       , "license-file:         LICENSE"
       , "author:               " <> PI.authors
       , "maintainer:           " <> PI.maintainers
       , "category:             " <> PI.category
       , "build-type:           Custom"
       , "cabal-version:        >= 1.22"
       , case girOverrides info of
           Nothing -> ""
           Just f -> "\n" <> "extra-source-files: " <> f <> "\n"
       , "library"
       , "      default-language: " <> PI.defaultLanguage
       , "      default-extensions: " <> T.intercalate ", " PI.defaultExtensions
       , "      other-extensions: " <> T.intercalate ", " PI.otherExtensions
       , "      ghc-options: " <> T.intercalate " " PI.ghcOptions
       , ""
       , "      pkgconfig-depends: " <> pkgconfigDepends info
       , "      build-depends: " <>
         T.intercalate ",\n                     "
              ([baseVersion info
               , "haskell-gi-base >= 0.17 && < 1"
               -- It would be cleaner to do this using setup-depends,
               -- but stack gets very confused by it.
               , "haskell-gi >= 0.17.3 && < 1"
               ] ++ giDepends info ++ PI.standardDeps)
       ]

writeSetup :: FilePath -> ProjectInfo -> IO ()
writeSetup fname info =
    TIO.writeFile fname $ T.unlines
           [ "{-# LANGUAGE OverloadedStrings #-}"
           , ""
           , "import Data.GI.CodeGen.CabalHooks (setupHaskellGIBinding)"
           , ""
           , "main :: IO ()"
           , "main = setupHaskellGIBinding name version verbose overridesFile outputDir"
           , "  where name = " <> tshow (girName info)
           , "        version = " <> tshow (girVersion info)
           , "        overridesFile = " <> tshow (girOverrides info)
           , "        verbose = False"
           , "        outputDir = Nothing"
           ]
    where tshow :: Show a => a -> Text
          tshow = T.pack . show

writeLicense :: FilePath -> IO ()
writeLicense fname = TIO.writeFile fname PI.licenseText

main :: IO ()
main = do
  dirs <- getArgs
  forM_ dirs $ \dir -> do
         info <- readGIRInfo (dir </> "pkg.info")
         writeCabal (dir </> T.unpack (name info) <.> "cabal") info
         writeSetup (dir </> "Setup.hs") info
         writeLicense (dir </> "LICENSE")
