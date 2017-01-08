-- | Parsing of constants in GIR files.
module Data.GI.GIR.Constant
    ( Constant(..)
    , parseConstant
    ) where

import Data.Text (Text)

import Data.GI.GIR.BasicTypes (Type)
import Data.GI.GIR.Type (parseType, parseCType)
import Data.GI.GIR.Parser

-- | Info about a constant.
data Constant = Constant {
      constantType        :: Type,
      constantValue       :: Text,
      constantCType       :: Text,
      constantDocumentation :: Documentation,
      constantDeprecated  :: Maybe DeprecationInfo
    } deriving (Show)

-- | Parse a "constant" element from the GIR file.
parseConstant :: Parser (Name, Constant)
parseConstant = do
  name <- parseName
  deprecated <- parseDeprecation
  value <- getAttr "value"
  t <- parseType
  ctype <- parseCType
  doc <- parseDocumentation
  return (name, Constant { constantType = t
                         , constantValue = value
                         , constantCType = ctype
                         , constantDocumentation = doc
                         , constantDeprecated = deprecated
                         })
