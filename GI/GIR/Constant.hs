{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of constants in GIR files.
module GI.GIR.Constant
    ( Constant(..)
    , parseConstant
    ) where

import Data.Text (Text)

import GI.Type (Type)
import GI.GIR.Type (parseType)
import GI.GIR.Parser

-- | Info about a constant.
data Constant = Constant {
      constantType        :: Type,
      constantValue       :: Text,
      constantDeprecated  :: Maybe DeprecationInfo
    } deriving (Show)

-- | Parse a "constant" element from the GIR file.
parseConstant :: Parser (Name, Constant)
parseConstant = do
  name <- parseName
  deprecated <- parseDeprecation
  value <- getAttr "value"
  t <- parseType
  return (name, Constant t value deprecated)
