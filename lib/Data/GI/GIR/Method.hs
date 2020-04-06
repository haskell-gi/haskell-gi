module Data.GI.GIR.Method
    ( Method(..)
    , MethodType(..)
    , parseMethod
    ) where

import Data.Text (Text)

import Data.GI.GIR.Arg (Arg, parseArg)
import Data.GI.GIR.Callable (Callable(..), parseCallable)
import Data.GI.GIR.Parser

data MethodType = Constructor    -- ^ Constructs an instance of the parent type
                | MemberFunction -- ^ A function in the namespace
                | OrdinaryMethod -- ^ A function taking the parent
                                 -- instance as first argument.
                  deriving (Eq, Show)

data Method = Method {
      methodName        :: Name,
      -- | The symbol in the dynlib that this method refers to.
      methodSymbol      :: Text,
      methodType        :: MethodType,
      methodMovedTo     :: Maybe Text,
      methodCallable    :: Callable
    } deriving (Eq, Show)

parseInstanceArg :: Parser Arg
parseInstanceArg = do
  instanceInfo <- parseChildrenWithLocalName "parameters" parseInstPars
  case instanceInfo of
    [[inst]] -> return inst
    [] -> parseError $ "No instance-parameter found."
    _ -> parseError $ "Too many instance parameters."
  where parseInstPars :: Parser [Arg]
        parseInstPars = parseChildrenWithLocalName "instance-parameter" parseArg

parseMethod :: MethodType -> Parser Method
parseMethod mType = do
  name <- parseName
  shadows <- queryAttr "shadows"
  let exposedName = case shadows of
                      Just n -> name {name = n}
                      Nothing -> name
  callable <- if mType /= OrdinaryMethod
              then parseCallable
              else do
                c <- parseCallable
                instanceArg <- parseInstanceArg
                return $ c {args = instanceArg : args c}
  symbol <- getAttrWithNamespace CGIRNS "identifier"
  movedTo <- queryAttr "moved-to"
  return $ Method {
              methodName = exposedName
            , methodSymbol = symbol
            , methodType = mType
            , methodMovedTo = movedTo
            , methodCallable = callable
            }
