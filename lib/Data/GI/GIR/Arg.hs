module Data.GI.GIR.Arg
    ( Arg(..)
    , Direction(..)
    , Scope(..)
    , parseArg
    , parseTransfer
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Data.GI.GIR.BasicTypes (Transfer(..), Type)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (parseType)

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
        argCName :: Text,  -- ^ "C" name for the argument. For a
                           -- escaped name valid in Haskell code, use
                           -- `GI.SymbolNaming.escapedArgName`.
        argType :: Type,
        direction :: Direction,
        mayBeNull :: Bool,
        argDoc :: Documentation,
        argScope :: Scope,
        argClosure :: Int,
        argDestroy :: Int,
        argCallerAllocates :: Bool,
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
  callerAllocates <- optionalAttr "caller-allocates" False parseBool
  t <- parseType
  doc <- parseDocumentation
  return $ Arg { argCName = name
               , argType = t
               , argDoc = doc
               , direction = d
               , mayBeNull = nullable
               , argScope = scope
               , argClosure = closure
               , argDestroy = destroy
               , argCallerAllocates = callerAllocates
               , transfer = ownership
               }
