module GI.GIR.Method
    ( Method(..)
    , MethodType(..)
    , parseMethod
    ) where

import Data.Text (Text)

import GI.GIR.Callable (Callable(..), parseCallable)
import GI.GIR.Parser

data MethodType = Constructor    -- ^ Constructs an instance of the parent type
                | MemberFunction -- ^ A function in the namespace
                | OrdinaryMethod -- ^ A function taking the parent
                                 -- instance as first argument.
                  deriving (Eq, Show)

data Method = Method {
      methodName        :: Name,
      methodSymbol      :: Text,
      methodThrows      :: Bool,
      methodType        :: MethodType,
      methodCallable    :: Callable
    } deriving (Eq, Show)

parseMethod :: MethodType -> Parser Method
parseMethod mType = do
  name <- parseName
  shadows <- queryAttr "shadows"
  let exposedName = case shadows of
                      Just n -> name {name = n}
                      Nothing -> name
  callable <- parseCallable
  symbol <- getAttrWithNamespace CGIRNS "identifier"
  throws <- optionalAttr "throws" False parseBool
  return $ Method {
              methodName = exposedName
            , methodSymbol = symbol
            , methodThrows = throws
            , methodType = mType
            , methodCallable = callable
            }
