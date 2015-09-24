{-# LANGUAGE OverloadedStrings #-}
module GI.GIR.Method
    ( Method(..)
    , MethodType(..)
    , parseMethod
    ) where

import Data.Text (Text)

import GI.GIR.Callable (Callable(..), parseCallable)
import GI.GIR.Parser

data MethodType = Constructor
                | OrdinaryMethod
                  deriving (Eq, Show)

data Method = Method {
      methodSymbol      :: Text,
      methodThrows      :: Bool,
      methodType        :: MethodType,
      methodCallable    :: Callable
    } deriving Show

parseMethod :: MethodType -> Parser (Name, Method)
parseMethod mType = do
  name <- parseName
  callable <- parseCallable
  symbol <- getAttrWithNamespace CGIRNS "identifier"
  throws <- optionalAttr "throws" False parseBool
  return $ (name,
            Method {
              methodSymbol = symbol
            , methodThrows = throws
            , methodType = mType
            , methodCallable = callable
            })
