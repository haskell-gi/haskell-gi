module GI.GIR.Function
    ( Function(..)
    , parseFunction
    ) where

import Data.Text (Text)

import GI.GIR.Callable (Callable(..), parseCallable)
import GI.GIR.Parser

data Function = Function {
      fnSymbol   :: Text
    , fnThrows   :: Bool
    , fnMovedTo  :: Maybe Text
    , fnCallable :: Callable
    } deriving Show

parseFunction :: Parser (Name, Function)
parseFunction = do
  name <- parseName
  shadows <- queryAttr "shadows"
  let exposedName = case shadows of
                      Just n -> name {name = n}
                      Nothing -> name
  callable <- parseCallable
  symbol <- getAttrWithNamespace CGIRNS "identifier"
  throws <- optionalAttr "throws" False parseBool
  movedTo <- queryAttr "moved-to"
  return $ (exposedName,
            Function {
              fnSymbol = symbol
            , fnCallable = callable
            , fnThrows = throws
            , fnMovedTo = movedTo
            })
