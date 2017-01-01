-- | A parser for gtk-doc formatted documentation, see
-- https://developer.gnome.org/gtk-doc-manual/ for the spec.
module Data.GI.GIR.GtkDoc
  ( parseGtkDoc
  , GtkDoc(..)
  , Token(..)
  ) where

import Prelude hiding (takeWhile)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*))
#endif
import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)
import qualified Data.Text as T
import Data.Text (Text)

-- | A parsed gtk-doc token.
data Token = Literal Text
           | FunctionRef Text
  deriving (Show, Eq)

-- | A parsed representation of gtk-doc formatted documentation.
newtype GtkDoc = GtkDoc [Token]
  deriving (Show, Eq)

-- | Parse the given gtk-doc formatted documentation.
--
-- === __Examples__
-- >>> parseGtkDoc ""
-- Right (GtkDoc [])
--
-- >>> parseGtkDoc "func()"
-- Right (GtkDoc [FunctionRef "func"])
--
-- >>> parseGtkDoc "literal"
-- Right (GtkDoc [Literal "literal"])
parseGtkDoc :: Text -> Either Text GtkDoc
parseGtkDoc raw = case parseOnly (parseTokens <* endOfInput) raw of
                    Left e -> Left (T.pack e)
                    Right tks -> Right (GtkDoc tks)

-- | Parser for tokens.
parseTokens :: Parser [Token]
parseTokens = many' parseToken

-- | Parse a single token.
--
-- === __Examples__
-- >>> parseOnly (parseToken <* endOfInput) "func()"
-- Right (FunctionRef "func")
parseToken :: Parser Token
parseToken = parseFunctionRef
             <|> parseLiteral

-- | Whether the given character is valid in a C identifier.
isCIdent :: Char -> Bool
isCIdent '_' = True
isCIdent c   = isDigit c || isAsciiUpper c || isAsciiLower c

-- | Something that could be a valid C identifier (loosely speaking,
-- we do not need to be too strict here).
parseCIdent :: Parser Text
parseCIdent = takeWhile1 isCIdent

-- | Parse a function ref, given by a valid C identifier followed by
-- '()', for instance 'gtk_widget_show()'.
parseFunctionRef :: Parser Token
parseFunctionRef = do
  ident <- parseCIdent
  _ <- string "()"
  return (FunctionRef ident)

-- | Parse a literal, i.e. anything without a known special meaning.
parseLiteral :: Parser Token
parseLiteral = (Literal <$> parseCIdent)
               <|> parseBoring

-- | Parse anything that cannot possibly be the beginning of an
-- interesting token.
parseBoring :: Parser Token
parseBoring = Literal <$> takeWhile1 (not . interesting)
  where interesting :: Char -> Bool
        interesting c = isCIdent c
