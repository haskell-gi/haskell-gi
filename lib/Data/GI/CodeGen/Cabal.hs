module Data.GI.CodeGen.Cabal
    ( genCabalProject
    , cabalConfig
    , setupHs
    , tryPkgConfig
    ) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Version (Version(..))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read

import Data.GI.CodeGen.API (GIRInfo(..))
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.Overrides (cabalPkgVersion)
import Data.GI.CodeGen.PkgConfig (pkgConfigGetVersion)
import qualified Data.GI.CodeGen.ProjectInfo as PI
import Data.GI.CodeGen.Util (padTo, tshow)

import Paths_haskell_gi (version)

cabalConfig :: Text
cabalConfig = T.unlines ["optimization: False"]

setupHs :: Text
setupHs = T.unlines ["#!/usr/bin/env runhaskell",
                     "import Distribution.Simple",
                     "main = defaultMain"]

haskellGIAPIVersion :: Int
haskellGIAPIVersion = (head . versionBranch) version

-- | Obtain the minor version. That is, if the given version numbers
-- are x.y.z, so branch is [x,y,z], we return y.
minorVersion :: [Int] -> Int
minorVersion (_:y:_) = y
minorVersion v = error $ "Programming error: the haskell-gi version does not have at least two components: " ++ show v ++ "."

-- | Obtain the haskell-gi minor version. Notice that we only append
-- the minor version here, ignoring revisions. (So if the version is
-- x.y.z, we drop the "z" part.) This gives us a mechanism for
-- releasing bug-fix releases of haskell-gi without increasing the
-- necessary dependency on haskell-gi-base, which only depends on x.y.
haskellGIMinor :: Int
haskellGIMinor = minorVersion (versionBranch version)

{- |

If the haskell-gi version is of the form x.y[.z] and the pkgconfig
version of the package being wrapped is a.b.c, this gives something of
the form x.a.b.y.

This strange seeming-rule is so that the packages that we produce
follow the PVP, assuming that the package being wrapped follows the
usual semantic versioning convention (http://semver.org) that
increases in "a" indicate non-backwards compatible changes, increases
in "b" backwards compatible additions to the API, and increases in "c"
denote API compatible changes (so we do not need to regenerate
bindings for these, at least in principle, so we do not encode them in
the cabal version).

In order to follow the PVP, then everything we need to do in the
haskell-gi side is to increase x everytime the generated API changes
(for a fixed a.b.c version).

In any case, if such "strange" package numbers are undesired, or the
wrapped package does not follow semver, it is possible to add an
explicit cabal-pkg-version override. This needs to be maintained by
hand (including in the list of dependencies of packages depending on
this one), so think carefully before using this override!

-}
giModuleVersion :: Int -> Int -> Text
giModuleVersion major minor =
    (T.intercalate "." . map tshow) [haskellGIAPIVersion, major, minor,
                                     haskellGIMinor]

-- | Determine the next version for which the minor of the package has
-- been bumped.
giNextMinor :: Int -> Int -> Text
giNextMinor major minor = (T.intercalate "." . map tshow)
                          [haskellGIAPIVersion, major, minor+1]

-- | Info for a given package.
data PkgInfo = PkgInfo { pkgName  :: Text
                       , pkgMajor :: Int
                       , pkgMinor :: Int
                       } deriving Show

-- | Determine the pkg-config name and installed version (major.minor
-- only) for a given module, or throw an exception if that fails.
tryPkgConfig :: GIRInfo -> Bool -> M.Map Text Text -> IO (Either Text PkgInfo)
tryPkgConfig gir verbose overridenNames = do
  let name = girNSName gir
      version = girNSVersion gir
      packages = girPCPackages gir

  pkgConfigGetVersion name version packages verbose overridenNames >>= \case
           Just (n,v) ->
               case readMajorMinor v of
                 Just (major, minor) ->
                   return $ Right (PkgInfo { pkgName = n
                                           , pkgMajor = major
                                           , pkgMinor = minor})
                 Nothing -> return $ Left $ "Cannot parse version \"" <> v <>
                            "\" for module " <> name
           Nothing -> return $ Left $
                      "Could not determine the pkg-config name corresponding to \"" <> name <> "\".\n" <>
                      "Try adding an override with the proper package name:\n"
                      <> "pkg-config-name " <> name <> " [matching pkg-config name here]"

-- | Given a string a.b.c..., representing a version number, determine
-- the major and minor versions, i.e. "a" and "b". If successful,
-- return (a,b).
readMajorMinor :: Text -> Maybe (Int, Int)
readMajorMinor version =
    case T.splitOn "." version of
      (a:b:_) -> (,) <$> readMaybe (T.unpack a) <*> readMaybe (T.unpack b)
      _ -> Nothing

-- | Generate the cabal project.
genCabalProject :: (GIRInfo, PkgInfo) -> [(GIRInfo, PkgInfo)] ->
                   [Text] -> BaseVersion -> CodeGen ()
genCabalProject (gir, PkgInfo {pkgName = pcName, pkgMajor = major,
                               pkgMinor = minor})
  deps exposedModules minBaseVersion = do
      cfg <- config
      let name = girNSName gir

      line $ "-- Autogenerated, do not edit."
      line $ padTo 20 "name:" <> "gi-" <> T.toLower name

      let cabalVersion = fromMaybe (giModuleVersion major minor)
                                    (cabalPkgVersion $ overrides cfg)
      line $ padTo 20 "version:" <> cabalVersion
      line $ padTo 20 "synopsis:" <> name
               <> " bindings"
      line $ padTo 20 "description:" <> "Bindings for " <> name
               <> ", autogenerated by haskell-gi."
      line $ padTo 20 "homepage:" <> PI.homepage
      line $ padTo 20 "license:" <> PI.license
      line $ padTo 20 "license-file:" <> "LICENSE"
      line $ padTo 20 "author:" <> PI.authors
      line $ padTo 20 "maintainer:" <> PI.maintainers
      line $ padTo 20 "category:" <> PI.category
      line $ padTo 20 "build-type:" <> "Simple"
      line $ padTo 20 "cabal-version:" <> ">=1.10"
      blank
      line $ "library"
      indent $ do
        line $ padTo 20 "default-language:" <> PI.defaultLanguage
        line $ padTo 20 "default-extensions:" <>
             T.intercalate ", " PI.defaultExtensions
        line $ padTo 20 "other-extensions:" <>
             T.intercalate ", " PI.otherExtensions
        line $ padTo 20 "ghc-options:" <> T.intercalate " " PI.ghcOptions
        line $ padTo 20 "exposed-modules:" <> head exposedModules
        forM_ (tail exposedModules) $ \mod ->
              line $ padTo 20 "" <> mod
        line $ padTo 20 "pkgconfig-depends:" <> pcName <> " >= " <>
          tshow major <> "." <> tshow minor
        line $ "build-depends:"
        indent $ do
          line $ "haskell-gi-base >= "
                   <> tshow haskellGIAPIVersion <> "." <> tshow haskellGIMinor
                   <> " && < " <> tshow (haskellGIAPIVersion + 1) <> ","
          forM_ deps $ \(dep, PkgInfo _ depMajor depMinor) -> do
              let depName = girNSName dep
              line $ "gi-" <> T.toLower depName <> " >= "
                <> giModuleVersion depMajor depMinor
                <> " && < "
                <> giNextMinor depMajor depMinor
                <> ","
          forM_ PI.standardDeps (line . (<> ","))
          line $ "base >= " <> showBaseVersion minBaseVersion <> " && <5"
