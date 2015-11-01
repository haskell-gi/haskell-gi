module GI.GIR.Arg
    ( Arg(..)
    , Direction(..)
    , Scope(..)
    , parseArg
    , parseTransfer
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)

import GI.GIR.BasicTypes (Transfer(..))
import GI.GIR.Parser
import GI.GIR.Type (parseType)
import GI.Type (Type)

data Direction = DirectionIn
               | DirectionOut
               | DirectionInout
                 deriving (Show, Eq, Ord)

data Scope = ScopeTypeInvalid
           | ScopeTypeCall
           | ScopeTypeAsync
           | ScopeTypeNotified
             deriving (Show, Eq, Ord)

data Arg = Arg {
        argName :: Text,
        argType :: Type,
        direction :: Direction,
        mayBeNull :: Bool,
        argScope :: Scope,
        argClosure :: Int,
        argDestroy :: Int,
        transfer :: Transfer
    } deriving (Show, Eq, Ord)

parseTransfer :: Parser Transfer
parseTransfer = getAttr "transfer-ownership" >>= \case
                "none" -> return TransferNothing
                "container" -> return TransferContainer
                "full" -> return TransferEverything
                t -> parseError $ "Unknown transfer type \"" <> t <> "\""

parseScope :: Text -> Parser Scope
parseScope "call" = return ScopeTypeCall
parseScope "async" = return ScopeTypeAsync
parseScope "notified" = return ScopeTypeNotified
parseScope s = parseError $ "Unknown scope type \"" <> s <> "\""

parseDirection :: Text -> Parser Direction
parseDirection "in" = return DirectionIn
parseDirection "out" = return DirectionOut
parseDirection "inout" = return DirectionInout
parseDirection d = parseError $ "Unknown direction \"" <> d <> "\""

parseArg :: Parser Arg
parseArg = do
  name <- getAttr "name"
  ownership <- parseTransfer
  scope <- optionalAttr "scope" ScopeTypeInvalid parseScope
  d <- optionalAttr "direction" DirectionIn parseDirection
  closure <- optionalAttr "closure" (-1) parseIntegral
  destroy <- optionalAttr "destroy" (-1) parseIntegral
  nullable <- optionalAttr "nullable" False parseBool
  t <- parseType
  return $ Arg { argName = name
               , argType = t
               , direction = d
               , mayBeNull = nullable
               , argScope = scope
               , argClosure = closure
               , argDestroy = destroy
               , transfer = ownership
               }
