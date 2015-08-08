{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of constants in GIR files.
module GI.GIR.Constant
    ( Constant(..)
    , parseConstant
    ) where

import qualified Data.Map as M
import Data.Text (Text)
import Text.XML (Element(elementAttributes))

import GI.Type (Type)
import GI.GIR.BasicTypes (ParseContext, Name, nameInCurrentNS)
import GI.GIR.Deprecation (DeprecationInfo, parseDeprecation)
import GI.GIR.Type (parseType)

-- | Info about a constant.
data Constant = Constant {
      constantType        :: Type,
      constantValue       :: Text,
      constantDeprecated  :: Maybe DeprecationInfo
    } deriving (Show)

-- | Parse a "constant" element from the GIR file.
parseConstant :: ParseContext -> Element -> Maybe (Name, Constant)
parseConstant ctx element = do
  let attrs = elementAttributes element
  name <- M.lookup "name" attrs
  value <- M.lookup "value" attrs
  t <- parseType ctx element
  return (nameInCurrentNS ctx name,
          Constant t value (parseDeprecation element))
