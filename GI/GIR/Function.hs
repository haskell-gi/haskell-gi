{-# LANGUAGE OverloadedStrings #-}
module GI.GIR.Function
    ( Function(..)
    , parseFunction
    ) where

import Data.Text (Text)

import GI.GIR.Callable (Callable(..), parseCallable)
import GI.GIR.Parser

data Function = Function {
        fnSymbol :: Text,
        fnThrows :: Bool,
        fnCallable :: Callable
    } deriving Show

parseFunction :: Parser (Name, Function)
parseFunction = do
  name <- parseName
  callable <- parseCallable
  symbol <- getAttrWithNamespace CGIRNS "identifier"
  throws <- optionalAttr "throws" False parseBool
  return $ (name,
            Function {
              fnSymbol = symbol
            , fnCallable = callable
            , fnThrows = throws
            })
