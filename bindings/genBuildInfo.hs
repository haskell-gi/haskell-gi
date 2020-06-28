{-# LANGUAGE OverloadedStrings #-}

-- | Generate the cabal info for the given subdirectories, assuming
-- the existence of appropriate "pkg.info" files.

import System.Environment (getArgs,getExecutablePath)
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Exit (exitFailure, ExitCode(..))
import System.Process (rawSystem)

import Control.Monad (forM_)

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.CodeGen.CabalHooks (configureDryRun)
import qualified Data.GI.CodeGen.ProjectInfo as PI
import Data.GI.CodeGen.Util (ucFirst)

import ProjectInfo (ProjectInfo(..))

import qualified Data.ByteString.Lazy as LB

readGIRInfo :: FilePath -> IO ProjectInfo
readGIRInfo fname = do
  buf <- LB.readFile fname
  case A.eitherDecode buf of
    Left err -> error ("Could not parse \"" <> fname <> "\": " <> err)
    Right info -> return info

writeCabal :: FilePath -> ProjectInfo -> [Text] -> IO ()
writeCabal fname info exposed =
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
       , "cabal-version:        1.24"
       , let commonFiles = "README.md ChangeLog.md stack.yaml"
         in case girOverrides info of
           Nothing -> "\nextra-source-files: " <> commonFiles <> "\n"
           Just ov -> "\nextra-source-files: " <> commonFiles <> " "
                      <> T.pack ov <> "\n"
       , "custom-setup"
       , "      setup-depends: " <>
         T.intercalate ",\n                     "
           ([ "base >= 4.9 && < 5"
            , "Cabal >= 1.24"
            , "haskell-gi >= 0.24.1 && < 0.25"]
            <> giDepends info)
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
              ([ baseVersion info
               , "haskell-gi-base >= 0.24 && < 0.25"
               -- Workaround for cabal new-build not picking up
               -- setup-depends dependencies when constructing the
               -- build plan.
               , "haskell-gi >= 0.24.1 && < 0.25"
               -- See https://github.com/haskell-gi/haskell-gi/issues/124
               -- for the reasoning behind this.
               , "haskell-gi-overloading < 1.1" ]
               <> giDepends info <> PI.standardDeps)
       , ""
       -- GHC 8.2.x panics when building the overloaded bindings
       -- https://ghc.haskell.org/trac/ghc/ticket/14382
       , "      -- Disable overloading when compiling under GHC 8.2.x"
       , "      -- see https://ghc.haskell.org/trac/ghc/ticket/14382"
       , "      if impl(ghc == 8.2.*)"
       , "              build-depends: haskell-gi-overloading == 0.0"
       , ""
       , "      -- Note that the following list of exposed modules and autogen"
       , "      -- modules is for documentation purposes only, so that some"
       , "      -- documentation appears in hackage. The actual list of modules"
       , "      -- to be built will be built at configure time, based on the"
       , "      -- available introspection data."
       , ""
       , "      exposed-modules: " <>
         T.intercalate ",\n                       " exposed
       , ""
       , "      autogen-modules: " <>
         T.intercalate ",\n                       " exposed
       ]

writeSetup :: FilePath -> ProjectInfo -> S.Set Text -> IO ()
writeSetup fname info deps =
    B.writeFile fname $ TE.encodeUtf8 $ T.unlines
           [ "{-# LANGUAGE OverloadedStrings #-}"
           , ""
           , "import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))"
           , ""
           , T.unlines (map buildInfo (S.toList deps))
           , ""
           , "main :: IO ()"
           , "main = setupBinding name version verbose overridesFile inheritedOverrides outputDir"
           , "  where name = " <> tshow (girName info)
           , "        version = " <> tshow (girVersion info)
           , "        overridesFile = " <> tshow (girOverrides info)
           , "        verbose = False"
           , "        outputDir = Nothing"
           , "        inheritedOverrides = ["
             <> T.intercalate ", " (map inheritedOverride (S.toList deps))
             <> "]"
           ]
    where tshow :: Show a => a -> Text
          tshow = T.pack . show

          buildInfo :: Text -> Text
          buildInfo dep = let capDep = ucFirst dep in
            "import qualified GI." <> capDep <> ".Config as " <> capDep

          inheritedOverride :: Text -> Text
          inheritedOverride dep = let capDep = ucFirst dep in
            "TaggedOverride \"inherited:" <> capDep <> "\" "
                      <> capDep <> ".overrides"

exposedModulesAndDeps :: FilePath -> ProjectInfo -> IO ([Text], S.Set Text)
exposedModulesAndDeps dir info =
  configureDryRun (girName info) (girVersion info)
                  ((dir </>) <$> girOverrides info) []

writeLicense :: FilePath -> ProjectInfo -> IO ()
writeLicense fname info = B.writeFile fname (TE.encodeUtf8 $ PI.licenseText (name info))

writeStackYaml :: FilePath -> IO ()
writeStackYaml fname =
    B.writeFile fname $ TE.encodeUtf8 $ T.unlines
         [ "packages:"
         , "- '.'"
         , "resolver: lts-13.7"
         ]

writeReadme :: FilePath -> ProjectInfo -> IO ()
writeReadme fname info =
  let docUrl = "https://hackage.haskell.org/package/" <>
        name info <> "-" <> version info <>
        "/docs/GI-" <> ucFirst (girName info) <> ".html"
  in B.writeFile fname $ TE.encodeUtf8 $ T.unlines
  [ "# Documentation"
  , "Autogenerated documentation for this package can be found at"
  , ""
  , "[" <> docUrl <> "](" <> docUrl <> ")"
  , ""
  , "For general documentation on using [haskell-gi](https://github.com/haskell-gi/haskell-gi) based bindings, see [the project page](https://github.com/haskell-gi/haskell-gi) or [the Wiki](https://github.com/haskell-gi/haskell-gi/wiki)."
  ]

genBindingsInDir :: FilePath -> IO ()
genBindingsInDir dir = do
  info <- readGIRInfo (dir </> "pkg.info")
  putStr $ "Generating " <> T.unpack (name info) <> "-" <> T.unpack (version info) <> " ..."
  hFlush stdout
  (exposed, deps) <- exposedModulesAndDeps dir info
  writeCabal (dir </> T.unpack (name info) <.> "cabal") info exposed
  writeSetup (dir </> "Setup.hs") info deps
  writeLicense (dir </> "LICENSE") info
  writeStackYaml (dir </> "stack.yaml")
  writeReadme (dir </> "README.md") info
  putStrLn " done."

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] -> do hPutStrLn stderr "usage: genBuildInfo [options] dir1 [dir2 [...]]"
             exitFailure
    [dir] -> genBindingsInDir dir
    dirs -> forM_ dirs $ \dir -> do
      -- We need to spawn new processes here, since code generation
      -- loads the library, and multiple versions of the same library
      -- cannot be simultaneously loaded.
      self <- getExecutablePath
      exitCode <- rawSystem self [dir]
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure err -> error ("Code generation failed: " <> show err)
