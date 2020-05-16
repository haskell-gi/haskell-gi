{-# LANGUAGE OverloadedStrings #-}
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)
import System.Process

main :: IO ()
main = do
    gobjectIntrospectionLibs <- pkgConfigLibs "gobject-introspection-1.0"
--    traverse_ putStrLn args -- optionally print arguments
    doctest (gobjectIntrospectionLibs ++ args)
  where
    args = flags ++ pkgs ++ module_sources

pkgConfigLibs :: String -> IO [String]
pkgConfigLibs pkg = words <$> readProcess "pkg-config" ["--libs", pkg] ""
