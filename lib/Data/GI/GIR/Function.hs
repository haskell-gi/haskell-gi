module Data.GI.GIR.Function
    ( Function(..)
    , parseFunction
    ) where

import Data.Text (Text)

import Data.GI.GIR.Callable (Callable(..), parseCallable)
import Data.GI.GIR.Parser

data Function = Function {
  -- | The symbol in the dynlib that this function refers to.
      fnSymbol   :: Text
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
  movedTo <- queryAttr "moved-to"
  return $ (exposedName,
            Function {
              fnSymbol = symbol
            , fnCallable = callable
            , fnMovedTo = movedTo
            })
