#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings, ViewPatterns, DeriveGeneric, DeriveAnyClass #-}

-- | Generate the cabal info for the given subdirectories, assuming
-- the existence of appropriate "GIR.info" files.

import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Console.GetOpt
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Exit (exitFailure)

import Control.Monad (forM_, when)
import GHC.Generics

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.GI.CodeGen.ProjectInfo as PI

import ProjectInfo (ProjectInfo(..))

import qualified Data.ByteString.Lazy as LB

-- | Which kind of project to generate. Unfortunately these are not
-- mutually compatible due to limitations in stack... see
-- https://github.com/commercialhaskell/stack/issues/2094
data ProjectType = CabalNewBuild
                 | Stack
                   deriving (Eq, Show)

readGIRInfo :: FilePath -> IO ProjectInfo
readGIRInfo fname = do
  buf <- LB.readFile fname
  case A.eitherDecode buf of
    Left err -> error ("Could not parse \"" <> fname <> "\": " <> err)
    Right info -> return info

writeCabal :: ProjectType -> FilePath -> ProjectInfo -> IO ()
writeCabal pType fname info =
    B.writeFile fname $ TE.encodeUtf8 $ T.unlines $
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
       ] <>
       (if pType == CabalNewBuild
       then [ "cabal-version:        >= 1.24"
             , case girOverrides info of
                Nothing -> ""
                Just ov -> "\nextra-source-files: " <> ov <> "\n"
            , "custom-setup"
            , "      setup-depends: base >= 4.7 && < 5,"
            , "                     Cabal >= 1.24,"
            , "                     haskell-gi >= 0.20 && < 1"
            , "" ]
       else [ "cabal-version:        >= 1.22"
            , case girOverrides info of
                Nothing -> "\nextra-source-files: stack.yaml\n"
                Just ov -> "\nextra-source-files: stack.yaml " <> ov <> "\n"
            ])
       <>
       [ "Flag overloaded-methods"
       , "      Description: Generate support for overloaded methods."
       , ""
       , "Flag overloaded-properties"
       , "      Description: Generate support for overloaded properties."
       , ""
       , "Flag overloaded-signals"
       , "      Description: Generate support for overloaded signals."
       , ""
       , "library"
       , "      default-language: " <> PI.defaultLanguage
       , "      default-extensions: " <> T.intercalate ", " PI.defaultExtensions
       , "      other-extensions: " <> T.intercalate ", " PI.otherExtensions
       , "      ghc-options: " <> T.intercalate " " PI.ghcOptions
       , ""
       , "      pkgconfig-depends: " <> pkgconfigDepends info
       , "      build-depends: " <>
         T.intercalate ",\n                     "
              ([baseVersion info] <>
               (if pType == CabalNewBuild
                then ["haskell-gi-base >= 0.20 && < 1"]
                else ["haskell-gi-base >= 0.20 && < 1"
                     ,"haskell-gi >= 0.20 && < 1"])
               ++ giDepends info ++ PI.standardDeps)
       ]

writeSetup :: FilePath -> ProjectInfo -> IO ()
writeSetup fname info =
    B.writeFile fname $ TE.encodeUtf8 $ T.unlines
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
writeLicense fname = B.writeFile fname (TE.encodeUtf8 PI.licenseText)

writeStackYaml :: FilePath -> IO ()
writeStackYaml fname =
    B.writeFile fname $ TE.encodeUtf8 $ T.unlines
         [ "packages:"
         , "- '.'"
         , "explicit-setup-deps:"
         , "  ! '*': true"
         , "resolver: nightly-2016-12-05"
         ]

data Options = Options {
      optProjectType :: ProjectType
      }

defaultOptions :: Options
defaultOptions = Options {
                   optProjectType = CabalNewBuild
                 }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "s" ["stack"] (NoArg $ \opt -> opt {optProjectType = Stack})
    "generate a stack project"]

showHelp :: String
showHelp = concatMap optAsLine optDescrs
  where optAsLine (Option flag (long:_) _ desc) =
          "  -" ++ flag ++ "|--" ++ long ++ "\t" ++ desc ++ "\n"
        optAsLine _ = error "showHelp"

main :: IO ()
main = do
  args <- getArgs

  let (actions, dirs, errors) = getOpt RequireOrder optDescrs args
      options  = foldl (.) id actions defaultOptions

  when ((not . null) errors) $ do
         mapM_ (hPutStr stderr) errors
         exitFailure

  when (null dirs) $ do
         hPutStrLn stderr "usage: genBuildInfo [options] dir1 [dir2 [...]]"
         hPutStr stderr showHelp
         exitFailure

  forM_ dirs $ \dir -> do
         info <- readGIRInfo (dir </> "pkg.info")
         writeCabal (optProjectType options)
                    (dir </> T.unpack (name info) <.> "cabal") info
         writeSetup (dir </> "Setup.hs") info
         writeLicense (dir </> "LICENSE")
         when (optProjectType options == Stack) $
              writeStackYaml (dir </> "stack.yaml")
