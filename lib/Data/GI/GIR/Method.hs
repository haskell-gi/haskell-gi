module Data.GI.GIR.Method
    ( Method(..)
    , MethodType(..)
    , parseMethod
    ) where

import Data.Text (Text)

import Data.GI.GIR.Callable (Callable(..), parseCallable)
import Data.GI.GIR.Parser

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
      methodMovedTo     :: Maybe Text,
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
  movedTo <- queryAttr "moved-to"
  return $ Method {
              methodName = exposedName
            , methodSymbol = symbol
            , methodThrows = throws
            , methodType = mType
            , methodMovedTo = movedTo
            , methodCallable = callable
            }
