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
import Data.Monoid ((<>))
import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)
import qualified Data.Text as T
import Data.Text (Text)

-- | A parsed gtk-doc token.
data Token = Literal Text
           | FunctionRef Text
           | ParamRef Text
           | ConstantRef Text
           | SignalRef Text Text
           | PropertyRef Text Text
           | VMethodRef Text Text
           | StructFieldRef Text Text
           | TypeRef Text
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
--
-- >>> parseGtkDoc "This is a long literal"
-- Right (GtkDoc [Literal "This is a long literal"])
--
-- >>> parseGtkDoc "Call foo() for free cookies"
-- Right (GtkDoc [Literal "Call ",FunctionRef "foo",Literal " for free cookies"])
-- >>> parseGtkDoc "The signal ##%#GtkButton::activate is related to gtk_button_activate()."
-- Right (GtkDoc [Literal "The signal ##%",SignalRef "GtkButton" "activate",Literal " is related to ",FunctionRef "gtk_button_activate",Literal "."])
parseGtkDoc :: Text -> Either Text GtkDoc
parseGtkDoc raw = case parseOnly (parseTokens <* endOfInput) raw of
                    Left e -> Left (T.pack e)
                    Right tks -> Right (GtkDoc (coalesceLiterals tks))
  where coalesceLiterals :: [Token] -> [Token]
        coalesceLiterals tks = go Nothing tks

        -- | Accumulate consecutive literals into a single literal.
        go :: Maybe Text -> [Token] -> [Token]
        go Nothing  [] = []
        go (Just l) [] = [Literal l]
        go Nothing (Literal l : rest) = go (Just l) rest
        go (Just l) (Literal l' : rest) = go (Just (l <> l')) rest
        go Nothing (tk : rest) = tk : go Nothing rest
        go (Just l) (tk : rest) = Literal l : tk : go Nothing rest

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
             <|> parseSignal
             <|> parseProperty
             <|> parseVMethod
             <|> parseStructField
             <|> parseType
             <|> (Literal <$> string "#")
             <|> parseConstant
             <|> (Literal <$> string "%")
             <|> parseParam
             <|> (Literal <$> string "@")
             <|> parseLiteral

-- | Parse a signal name, of the form
-- > #Object::signal
--
-- === __Examples__
-- >>> parseOnly (parseSignal <* endOfInput) "#GtkButton::activate"
-- Right (SignalRef "GtkButton" "activate")
parseSignal :: Parser Token
parseSignal = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- string "::"
  signal <- signalOrPropName
  return (SignalRef obj signal)

-- | Parse a property name, of the form
-- > #Object:property
--
-- === __Examples__
-- >>> parseOnly (parseProperty <* endOfInput) "#GtkButton:always-show-image"
-- Right (PropertyRef "GtkButton" "always-show-image")
parseProperty :: Parser Token
parseProperty = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char ':'
  property <- signalOrPropName
  return (PropertyRef obj property)

-- | Parse a reference to a virtual method, of the form
-- > #Struct.method()
--
-- === __Examples__
-- >>> parseOnly (parseVMethod <* endOfInput) "#Foo.bar()"
-- Right (VMethodRef "Foo" "bar")
parseVMethod :: Parser Token
parseVMethod = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char '.'
  method <- parseCIdent
  _ <- string "()"
  return (VMethodRef obj method)

-- | Parse a reference to a struct field, of the form
-- > #Struct.field
--
-- === __Examples__
-- >>> parseOnly (parseStructField <* endOfInput) "#Foo.bar"
-- Right (StructFieldRef "Foo" "bar")
parseStructField :: Parser Token
parseStructField = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char '.'
  field <- parseCIdent
  return (StructFieldRef obj field)

-- | Parse a reference to a C type, of the form
-- > #Type
--
-- === __Examples__
-- >>> parseOnly (parseType <* endOfInput) "#Foo"
-- Right (TypeRef "Foo")
parseType :: Parser Token
parseType = do
  _ <- char '#'
  obj <- parseCIdent
  return (TypeRef obj)

-- | Parse a constant, of the form
-- > %CONSTANT_NAME
--
-- === __Examples__
-- >>> parseOnly (parseConstant <* endOfInput) "%TEST_CONSTANT"
-- Right (ConstantRef "TEST_CONSTANT")
parseConstant :: Parser Token
parseConstant = do
  _ <- char '%'
  c <- parseCIdent
  return (ConstantRef c)

-- | Parse a reference to a parameter, of the form
-- > @param_name
--
-- === __Examples__
-- >>> parseOnly (parseParam <* endOfInput) "@test_param"
-- Right (ParamRef "test_param")
parseParam :: Parser Token
parseParam = do
  _ <- char '@'
  param <- parseCIdent
  return (ParamRef param)

-- | Whether the given character is valid in a C identifier.
isCIdent :: Char -> Bool
isCIdent '_' = True
isCIdent c   = isDigit c || isAsciiUpper c || isAsciiLower c

-- | Name of a signal or property name. Similar to a C identifier, but
-- hyphens are allowed too.
signalOrPropName :: Parser Text
signalOrPropName = takeWhile1 isSignalOrPropIdent
  where isSignalOrPropIdent :: Char -> Bool
        isSignalOrPropIdent '-' = True
        isSignalOrPropIdent c = isCIdent c

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
        interesting '#' = True
        interesting '@' = True
        interesting '%' = True
        interesting c = isCIdent c
