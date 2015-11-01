module GI.GIR.Callable
    ( Callable(..)
    , parseCallable
    ) where

import GI.Type (Type)
import GI.GIR.Arg (Arg(..), parseArg, parseTransfer)
import GI.GIR.BasicTypes (Transfer(..))
import GI.GIR.Parser
import GI.GIR.Type (parseType)

data Callable = Callable {
        returnType :: Type,
        returnMayBeNull :: Bool,
        returnTransfer :: Transfer,
        args :: [Arg],
        skipReturn :: Bool,
        callableDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)

parseArgs :: Parser [Arg]
parseArgs = do
  paramSets <- parseChildrenWithLocalName "parameters" parseArgSet
  case paramSets of
    [] -> return []
    (ps:[]) -> return ps
    _ -> parseError $ "Unexpected multiple \"parameters\" tag"
  where parseArgSet = parseChildrenWithLocalName "parameter" parseArg

parseOneReturn :: Parser (Type, Bool, Transfer, Bool)
parseOneReturn = do
  returnType <- parseType
  mayBeNull <- optionalAttr "allow-none" False parseBool
  transfer <- parseTransfer
  skip <- optionalAttr "skip" False parseBool
  return (returnType, mayBeNull, transfer, skip)

parseReturn :: Parser (Type, Bool, Transfer, Bool)
parseReturn = do
  returnSets <- parseChildrenWithLocalName "return-value" parseOneReturn
  case returnSets of
    (r:[]) -> return r
    [] -> parseError $ "No return information found"
    _ -> parseError $ "Multiple return values found"

parseCallable :: Parser Callable
parseCallable = do
  args <- parseArgs
  (returnType, mayBeNull, transfer, skip) <- parseReturn
  deprecated <- parseDeprecation
  return $ Callable {
                  returnType = returnType
                , returnMayBeNull = mayBeNull
                , returnTransfer = transfer
                , args = args
                , skipReturn = skip
                , callableDeprecated = deprecated
                }
