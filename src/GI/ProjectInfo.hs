{-# LANGUAGE TemplateHaskell #-}
-- | Project information to include in generated bindings, should be
-- kept in sync with haskell-gi.cabal
module GI.ProjectInfo
    ( homepage
    , authors
    , license
    , licenseText
    , maintainers
    ) where

import Data.FileEmbed (embedStringFile)

homepage :: String
homepage = "https://github.com/haskell-gi/haskell-gi"

authors :: String
authors = "Will Thompson, Iñaki García Etxebarria and Jonas Platte"

maintainers :: String
maintainers = "Iñaki García Etxebarria (garetxe@gmail.com)"

license :: String
license = "LGPL-2.1"

licenseText :: String
licenseText = $(embedStringFile "LICENSE")
