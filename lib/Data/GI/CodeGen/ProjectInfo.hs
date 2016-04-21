{-# LANGUAGE TemplateHaskell #-}
-- | Project information to include in generated bindings, should be
-- kept in sync with haskell-gi.cabal
module Data.GI.CodeGen.ProjectInfo
    ( homepage
    , authors
    , license
    , licenseText
    , maintainers
    ) where

import Data.FileEmbed (embedStringFile)
import Data.Text (Text)

homepage :: Text
homepage = "https://github.com/haskell-gi/haskell-gi"

authors :: Text
authors = "Will Thompson, Iñaki García Etxebarria and Jonas Platte"

maintainers :: Text
maintainers = "Iñaki García Etxebarria (garetxe@gmail.com)"

license :: Text
license = "LGPL-2.1"

licenseText :: Text
licenseText = $(embedStringFile "LICENSE")
