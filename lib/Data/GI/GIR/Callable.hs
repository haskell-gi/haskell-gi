module Data.GI.GIR.Callable
    ( Callable(..)
    , parseCallable
    ) where

import Data.GI.GIR.Arg (Arg(..), parseArg, parseTransfer)
import Data.GI.GIR.BasicTypes (Transfer(..), Type)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (parseOptionalType)

data Callable = Callable {
        returnType :: Maybe Type,
        returnMayBeNull :: Bool,
        returnTransfer :: Transfer,
        returnDocumentation :: Documentation,
        args :: [Arg],
        skipReturn :: Bool,
        callableThrows :: Bool,
        callableDeprecated :: Maybe DeprecationInfo,
        callableDocumentation :: Documentation,
        -- | Whether the symbol for this callable can be resolved in
        -- the dynamical library associated with the current
        -- introspection data. 'Nothing' means that we have not
        -- checked yet.
        callableResolvable :: Maybe Bool
    } deriving (Show, Eq)

parseArgs :: Parser [Arg]
parseArgs = do
  paramSets <- parseChildrenWithLocalName "parameters" parseArgSet
  case paramSets of
    [] -> return []
    (ps:[]) -> return ps
    _ -> parseError $ "Unexpected multiple \"parameters\" tag"
  where parseArgSet = parseChildrenWithLocalName "parameter" parseArg

parseOneReturn :: Parser (Maybe Type, Bool, Transfer, Bool, Documentation)
parseOneReturn = do
  returnType <- parseOptionalType
  allowNone <- optionalAttr "allow-none" False parseBool
  nullable <- optionalAttr "nullable" False parseBool
  transfer <- parseTransfer
  doc <- parseDocumentation
  skip <- optionalAttr "skip" False parseBool
  return (returnType, allowNone || nullable, transfer, skip, doc)

parseReturn :: Parser (Maybe Type, Bool, Transfer, Bool, Documentation)
parseReturn = do
  returnSets <- parseChildrenWithLocalName "return-value" parseOneReturn
  case returnSets of
    (r:[]) -> return r
    [] -> parseError $ "No return information found"
    _ -> parseError $ "Multiple return values found"

parseCallable :: Parser Callable
parseCallable = do
  args <- parseArgs
  (returnType, mayBeNull, transfer, skip, returnDoc) <- parseReturn
  deprecated <- parseDeprecation
  docs <- parseDocumentation
  throws <- optionalAttr "throws" False parseBool
  return $ Callable {
                  returnType = returnType
                , returnMayBeNull = mayBeNull
                , returnTransfer = transfer
                , returnDocumentation = returnDoc
                , args = args
                , skipReturn = skip
                , callableThrows = throws
                , callableDeprecated = deprecated
                , callableDocumentation = docs
                  -- Some symbols are present in the @.gir@ file, but
                  -- they are absent from the library
                  -- itself. Generating bindings for such symbols
                  -- could then lead to linker errors, so later on we
                  -- check whether the callables are actually
                  -- resolvable, and adjust the callable info
                  -- appropriately.
                , callableResolvable = Nothing
                }
