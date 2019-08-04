module Data.GI.GIR.Property
    ( Property(..)
    , PropertyFlag(..)
    , parseProperty
    ) where

import Data.Text (Text)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

import Data.GI.GIR.Arg (parseTransfer)
import Data.GI.GIR.BasicTypes (Transfer, Type)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (parseType)

data PropertyFlag = PropertyReadable
                  | PropertyWritable
                  | PropertyConstruct
                  | PropertyConstructOnly
                    deriving (Show,Eq)

data Property = Property {
        propName :: Text,
        propType :: Type,
        propFlags :: [PropertyFlag],
        propReadNullable :: Maybe Bool,
        propWriteNullable :: Maybe Bool,
        propTransfer :: Transfer,
        propDoc :: Documentation,
        propDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)

parseProperty :: Parser Property
parseProperty = do
  name <- getAttr "name"
  t <- parseType
  transfer <- parseTransfer
  deprecated <- parseDeprecation
  readable <- optionalAttr "readable" True parseBool
  writable <- optionalAttr "writable" False parseBool
  construct <- optionalAttr "construct" False parseBool
  constructOnly <- optionalAttr "construct-only" False parseBool
  maybeNullable <- optionalAttr "nullable" Nothing (\t -> Just <$> parseBool t)
  let flags = (if readable then [PropertyReadable] else [])
              <> (if writable then [PropertyWritable] else [])
              <> (if construct then [PropertyConstruct] else [])
              <> (if constructOnly then [PropertyConstructOnly] else [])
  doc <- parseDocumentation
  return $ Property {
                  propName = name
                , propType = t
                , propFlags = flags
                , propTransfer = transfer
                , propDeprecated = deprecated
                , propDoc = doc
                , propReadNullable = maybeNullable
                , propWriteNullable = maybeNullable
                }
