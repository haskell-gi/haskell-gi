module Data.GI.GIR.Arg
    ( Arg(..)
    , Direction(..)
    , Scope(..)
    , parseArg
    , parseTransfer
    , parseTransferString
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Data.GI.GIR.BasicTypes (Transfer(..), Type)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (parseType, queryElementCType)

data Direction = DirectionIn
               | DirectionOut
               | DirectionInout
                 deriving (Show, Eq, Ord)

data Scope = ScopeTypeInvalid
           | ScopeTypeCall
           | ScopeTypeAsync
           | ScopeTypeNotified
           | ScopeTypeForever
             deriving (Show, Eq, Ord)

data Arg = Arg {
        argCName :: Text,  -- ^ "C" name for the argument. For a
                           -- escaped name valid in Haskell code, use
                           -- `GI.SymbolNaming.escapedArgName`.
        argType :: Type,
        argCType :: Maybe Text,
        direction :: Direction,
        mayBeNull :: Bool,
        argDoc :: Documentation,
        argScope :: Scope,
        argClosure :: Int,
        argDestroy :: Int,
        argCallerAllocates :: Bool,
        argCallbackUserData :: Bool,
        -- ^ Whether the argument is an "user-data" argument for a callback.
        transfer :: Transfer
    } deriving (Show, Eq, Ord)

parseTransferString :: Text -> Parser Transfer
parseTransferString transfer = case transfer of
                "none" -> return TransferNothing
                "container" -> return TransferContainer
                "full" -> return TransferEverything
                t -> parseError $ "Unknown transfer type \"" <> t <> "\""

parseTransfer :: Parser Transfer
parseTransfer = getAttr "transfer-ownership" >>= parseTransferString

parseScope :: Text -> Parser Scope
parseScope "call" = return ScopeTypeCall
parseScope "async" = return ScopeTypeAsync
parseScope "notified" = return ScopeTypeNotified
parseScope "forever" = return ScopeTypeForever
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
  allowNone <- optionalAttr "allow-none" False parseBool
  -- "allow-none" is deprecated, but still produced by Vala. Support
  -- it for in arguments.
  let mayBeNull = if d == DirectionIn
                  then nullable || allowNone
                  else nullable
  callerAllocates <- optionalAttr "caller-allocates" False parseBool
  -- There is no annotation for this one yet, see
  -- https://gitlab.gnome.org/GNOME/gobject-introspection/-/issues/450
  -- We will use some heuristics later for setting this field.
  let callbackUserData = False
  t <- parseType
  maybeCType <- queryElementCType
  doc <- parseDocumentation
  return $ Arg { argCName = name
               , argType = t
               , argCType = maybeCType
               , argDoc = doc
               , direction = d
               , mayBeNull = mayBeNull
               , argScope = scope
               , argClosure = closure
               , argDestroy = destroy
               , argCallerAllocates = callerAllocates
               , argCallbackUserData = callbackUserData
               , transfer = ownership
               }
