-- | A parser for gtk-doc formatted documentation, see
-- https://developer.gnome.org/gtk-doc-manual/ for the spec.
module Data.GI.CodeGen.GtkDoc
  ( parseGtkDoc
  , GtkDoc(..)
  , Token(..)
  , Language(..)
  , Link(..)
  , ListItem(..)
  , CRef(..)
  ) where

import Prelude hiding (takeWhile)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*))
#endif
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)
import qualified Data.Text as T
import Data.Text (Text)

-- | A parsed gtk-doc token.
data Token = Literal Text
           | Comment Text
           | Verbatim Text
           | CodeBlock (Maybe Language) Text
           | ExternalLink Link
           | Image Link
           | List [ListItem]
           | SectionHeader Int GtkDoc -- ^ A section header of the given depth.
           | SymbolRef CRef
  deriving (Show, Eq)

-- | A link to a resource, either offline or a section of the documentation.
data Link = Link { linkName :: Text
                 , linkAddress :: Text }
  deriving (Show, Eq)

-- | An item in a list, given by a list of lines (not including ending
-- newlines). The list is always non-empty, so we represent it by the
-- first line and then a possibly empty list with the rest of the lines.
data ListItem = ListItem GtkDoc [GtkDoc]
  deriving (Show, Eq)

-- | The language for an embedded code block.
newtype Language = Language Text
  deriving (Show, Eq)

-- | A reference to some symbol in the API.
data CRef = FunctionRef Text
          | ParamRef Text
          | ConstantRef Text
          | SignalRef Text Text
          | LocalSignalRef Text
          | PropertyRef Text Text
          | VMethodRef Text Text
          | StructFieldRef Text Text
          | TypeRef Text
  deriving (Show, Eq, Ord)

-- | A parsed representation of gtk-doc formatted documentation.
newtype GtkDoc = GtkDoc [Token]
  deriving (Show, Eq)

-- | Parse the given gtk-doc formatted documentation.
--
-- === __Examples__
-- >>> parseGtkDoc ""
-- GtkDoc []
--
-- >>> parseGtkDoc "func()"
-- GtkDoc [SymbolRef (FunctionRef "func")]
--
-- >>> parseGtkDoc "literal"
-- GtkDoc [Literal "literal"]
--
-- >>> parseGtkDoc "This is a long literal"
-- GtkDoc [Literal "This is a long literal"]
--
-- >>> parseGtkDoc "Call foo() for free cookies"
-- GtkDoc [Literal "Call ",SymbolRef (FunctionRef "foo"),Literal " for free cookies"]
--
-- >>> parseGtkDoc "The signal ::activate is related to gtk_button_activate()."
-- GtkDoc [Literal "The signal ",SymbolRef (LocalSignalRef "activate"),Literal " is related to ",SymbolRef (FunctionRef "gtk_button_activate"),Literal "."]
--
-- >>> parseGtkDoc "The signal ##%#GtkButton::activate is related to gtk_button_activate()."
-- GtkDoc [Literal "The signal ##%",SymbolRef (SignalRef "GtkButton" "activate"),Literal " is related to ",SymbolRef (FunctionRef "gtk_button_activate"),Literal "."]
--
-- >>> parseGtkDoc "# A section\n\n## and a subsection ##\n"
-- GtkDoc [SectionHeader 1 (GtkDoc [Literal "A section"]),Literal "\n",SectionHeader 2 (GtkDoc [Literal "and a subsection "])]
--
-- >>> parseGtkDoc "Compact list:\n- First item\n- Second item"
-- GtkDoc [Literal "Compact list:\n",List [ListItem (GtkDoc [Literal "First item"]) [],ListItem (GtkDoc [Literal "Second item"]) []]]
--
-- >>> parseGtkDoc "Spaced list:\n\n- First item\n\n- Second item"
-- GtkDoc [Literal "Spaced list:\n",List [ListItem (GtkDoc [Literal "First item"]) [],ListItem (GtkDoc [Literal "Second item"]) []]]
--
-- >>> parseGtkDoc "List with urls:\n- [test](http://test)\n- ![](image.png)"
-- GtkDoc [Literal "List with urls:\n",List [ListItem (GtkDoc [ExternalLink (Link {linkName = "test", linkAddress = "http://test"})]) [],ListItem (GtkDoc [Image (Link {linkName = "", linkAddress = "image.png"})]) []]]
parseGtkDoc :: Text -> GtkDoc
parseGtkDoc raw =
  case parseOnly (parseTokens <* endOfInput) raw of
    Left e ->
      error $ "gtk-doc parsing failed with error \"" <> e
      <> "\" on the input \"" <> T.unpack raw <> "\""
    Right tks -> GtkDoc . coalesceLiterals
                 . restoreSHPreNewlines . restoreListPreNewline $ tks

-- | `parseSectionHeader` eats the newline before the section header,
-- but `parseInitialSectionHeader` does not, since it only matches at
-- the beginning of the text. This restores the newlines eaten by
-- `parseSectionHeader`, so a `SectionHeader` returned by the parser
-- can always be assumed /not/ to have an implicit starting newline.
restoreSHPreNewlines :: [Token] -> [Token]
restoreSHPreNewlines [] = []
restoreSHPreNewlines (i : rest) = i : restoreNewlines rest
  where restoreNewlines :: [Token] -> [Token]
        restoreNewlines [] = []
        restoreNewlines (s@(SectionHeader _ _) : rest) =
          Literal "\n" : s : restoreNewlines rest
        restoreNewlines (x : rest) = x : restoreNewlines rest

-- | `parseList` eats the newline before the list, restore it.
restoreListPreNewline :: [Token] -> [Token]
restoreListPreNewline [] = []
restoreListPreNewline (l@(List _) : rest) =
  Literal "\n" : l : restoreListPreNewline rest
restoreListPreNewline (x : rest) = x : restoreListPreNewline rest

-- | Accumulate consecutive literals into a single literal.
coalesceLiterals :: [Token] -> [Token]
coalesceLiterals tks = go Nothing tks
  where
    go :: Maybe Text -> [Token] -> [Token]
    go Nothing  [] = []
    go (Just l) [] = [Literal l]
    go Nothing (Literal l : rest) = go (Just l) rest
    go (Just l) (Literal l' : rest) = go (Just (l <> l')) rest
    go Nothing (tk : rest) = tk : go Nothing rest
    go (Just l) (tk : rest) = Literal l : tk : go Nothing rest

-- | Parser for tokens.
parseTokens :: Parser [Token]
parseTokens = headerAndTokens <|> justTokens
  where -- In case the input starts by a section header.
        headerAndTokens :: Parser [Token]
        headerAndTokens = do
          header <- parseInitialSectionHeader
          tokens <- justTokens
          return (header : tokens)

        justTokens :: Parser [Token]
        justTokens = many' parseToken

-- | Parse a single token.
--
-- === __Examples__
-- >>> parseOnly (parseToken <* endOfInput) "func()"
-- Right (SymbolRef (FunctionRef "func"))
parseToken :: Parser Token
parseToken = -- Note that the parsers overlap, so this is not as
             -- efficient as it could be (if we had combined parsers
             -- and then branched, so that there is no
             -- backtracking). But speed is not an issue here, so for
             -- clarity we keep the parsers distinct. The exception
             -- is parseFunctionRef, since it does not complicate the
             -- parser much, and it is the main source of
             -- backtracking.
                 parseFunctionRef
             <|> parseSignal
             <|> parseLocalSignal
             <|> parseProperty
             <|> parseVMethod
             <|> parseStructField
             <|> parseType
             <|> parseConstant
             <|> parseParam
             <|> parseEscaped
             <|> parseVerbatim
             <|> parseCodeBlock
             <|> parseUrl
             <|> parseImage
             <|> parseSectionHeader
             <|> parseList
             <|> parseComment
             <|> parseBoringLiteral

-- | Parse a signal name, of the form
-- > #Object::signal
--
-- === __Examples__
-- >>> parseOnly (parseSignal <* endOfInput) "#GtkButton::activate"
-- Right (SymbolRef (SignalRef "GtkButton" "activate"))
parseSignal :: Parser Token
parseSignal = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- string "::"
  signal <- signalOrPropName
  return (SymbolRef (SignalRef obj signal))

-- | Parse a reference to a signal defined in the current module, of the form
-- > ::signal
--
-- === __Examples__
-- >>> parseOnly (parseLocalSignal <* endOfInput) "::activate"
-- Right (SymbolRef (LocalSignalRef "activate"))
parseLocalSignal :: Parser Token
parseLocalSignal = do
  _ <- string "::"
  signal <- signalOrPropName
  return (SymbolRef (LocalSignalRef signal))

-- | Parse a property name, of the form
-- > #Object:property
--
-- === __Examples__
-- >>> parseOnly (parseProperty <* endOfInput) "#GtkButton:always-show-image"
-- Right (SymbolRef (PropertyRef "GtkButton" "always-show-image"))
parseProperty :: Parser Token
parseProperty = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char ':'
  property <- signalOrPropName
  return (SymbolRef (PropertyRef obj property))

-- | Parse an xml comment, of the form
-- > <!-- comment -->
-- Note that this function keeps spaces.
--
-- === __Examples__
-- >>> parseOnly (parseComment <* endOfInput) "<!-- comment -->"
-- Right (Comment " comment ")
parseComment :: Parser Token
parseComment = do
  comment <- string "<!--" *> manyTill anyChar (string "-->")
  return (Comment $ T.pack comment)

-- | Parse a reference to a virtual method, of the form
-- > #Struct.method()
--
-- === __Examples__
-- >>> parseOnly (parseVMethod <* endOfInput) "#Foo.bar()"
-- Right (SymbolRef (VMethodRef "Foo" "bar"))
parseVMethod :: Parser Token
parseVMethod = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char '.'
  method <- parseCIdent
  _ <- string "()"
  return (SymbolRef (VMethodRef obj method))

-- | Parse a reference to a struct field, of the form
-- > #Struct.field
--
-- === __Examples__
-- >>> parseOnly (parseStructField <* endOfInput) "#Foo.bar"
-- Right (SymbolRef (StructFieldRef "Foo" "bar"))
parseStructField :: Parser Token
parseStructField = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char '.'
  field <- parseCIdent
  return (SymbolRef (StructFieldRef obj field))

-- | Parse a reference to a C type, of the form
-- > #Type
--
-- === __Examples__
-- >>> parseOnly (parseType <* endOfInput) "#Foo"
-- Right (SymbolRef (TypeRef "Foo"))
parseType :: Parser Token
parseType = do
  _ <- char '#'
  obj <- parseCIdent
  return (SymbolRef (TypeRef obj))

-- | Parse a constant, of the form
-- > %CONSTANT_NAME
--
-- === __Examples__
-- >>> parseOnly (parseConstant <* endOfInput) "%TEST_CONSTANT"
-- Right (SymbolRef (ConstantRef "TEST_CONSTANT"))
parseConstant :: Parser Token
parseConstant = do
  _ <- char '%'
  c <- parseCIdent
  return (SymbolRef (ConstantRef c))

-- | Parse a reference to a parameter, of the form
-- > @param_name
--
-- === __Examples__
-- >>> parseOnly (parseParam <* endOfInput) "@test_param"
-- Right (SymbolRef (ParamRef "test_param"))
parseParam :: Parser Token
parseParam = do
  _ <- char '@'
  param <- parseCIdent
  return (SymbolRef (ParamRef param))

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
-- '()', for instance 'gtk_widget_show()'. If the identifier is not
-- followed by "()", return it as a literal instead.
--
-- === __Examples__
-- >>> parseOnly (parseFunctionRef <* endOfInput) "test_func()"
-- Right (SymbolRef (FunctionRef "test_func"))
--
-- >>> parseOnly (parseFunctionRef <* endOfInput) "not_a_func"
-- Right (Literal "not_a_func")
parseFunctionRef :: Parser Token
parseFunctionRef = do
  ident <- parseCIdent
  option (Literal ident) (string "()" >>
                          return (SymbolRef (FunctionRef ident)))

-- | Parse a escaped special character, i.e. one preceded by '\'.
parseEscaped :: Parser Token
parseEscaped = do
  _ <- char '\\'
  c <- satisfy (`elem` ("#@%\\`" :: [Char]))
  return $ Literal (T.singleton c)

-- | Parse a literal, i.e. anything without a known special
-- meaning. Note that this parser always consumes the first character,
-- regardless of what it is.
parseBoringLiteral :: Parser Token
parseBoringLiteral = do
  c <- anyChar
  boring <- takeWhile (not . special)
  return $ Literal (T.cons c boring)

-- | List of special characters from the point of view of the parser
-- (in the sense that they may be the beginning of something with a
-- special interpretation).
special :: Char -> Bool
special '#' = True
special '@' = True
special '%' = True
special '\\' = True
special '`' = True
special '|' = True
special '[' = True
special '!' = True
special '\n' = True
special ':' = True
special c = isCIdent c

-- | Parse a verbatim string, of the form
-- > `verbatim text`
--
-- === __Examples__
-- >>> parseOnly (parseVerbatim <* endOfInput) "`Example quote!`"
-- Right (Verbatim "Example quote!")
parseVerbatim :: Parser Token
parseVerbatim = do
  _ <- char '`'
  v <- takeWhile1 (/= '`')
  _ <- char '`'
  return $ Verbatim v

-- | Parse a URL in Markdown syntax, of the form
-- > [name](url)
--
-- === __Examples__
-- >>> parseOnly (parseUrl <* endOfInput) "[haskell](http://haskell.org)"
-- Right (ExternalLink (Link {linkName = "haskell", linkAddress = "http://haskell.org"}))
parseUrl :: Parser Token
parseUrl = do
  _ <- char '['
  name <- takeWhile1 (/= ']')
  _ <- string "]("
  address <- takeWhile1 (/= ')')
  _ <- char ')'
  return $ ExternalLink $ Link {linkName = name, linkAddress = address}

-- | Parse an image reference, of the form
-- > ![label](url)
--
-- === __Examples__
-- >>> parseOnly (parseImage <* endOfInput) "![](diagram.png)"
-- Right (Image (Link {linkName = "", linkAddress = "diagram.png"}))
parseImage :: Parser Token
parseImage = do
  _ <- string "!["
  name <- takeWhile (/= ']')
  _ <- string "]("
  address <- takeWhile1 (/= ')')
  _ <- char ')'
  return $ Image $ Link {linkName = name, linkAddress = address}

-- | Parse a code block embedded in the documentation.
parseCodeBlock :: Parser Token
parseCodeBlock = do
  _ <- string "|["
  lang <- (Just <$> parseLanguage) <|> return Nothing
  code <- T.pack <$> manyTill anyChar (string "]|")
  return $ CodeBlock lang code

-- | Parse the language of a code block, specified as a comment.
parseLanguage :: Parser Language
parseLanguage = do
  _ <- string "<!--"
  skipSpace
  _ <- string "language=\""
  lang <- takeWhile1 (/= '"')
  _ <- char '"'
  skipSpace
  _ <- string "-->"
  return $ Language lang

-- | Parse a section header, given by a number of hash symbols, and
-- then ordinary text. Note that this parser "eats" the newline before
-- and after the section header.
parseSectionHeader :: Parser Token
parseSectionHeader = char '\n' >> parseInitialSectionHeader

-- | Parse a section header at the beginning of the text. I.e. this is
-- the same as `parseSectionHeader`, but we do not expect a newline as
-- a first character.
--
-- === __Examples__
-- >>> parseOnly (parseInitialSectionHeader <* endOfInput) "### Hello! ###\n"
-- Right (SectionHeader 3 (GtkDoc [Literal "Hello! "]))
--
-- >>> parseOnly (parseInitialSectionHeader <* endOfInput) "# Hello!\n"
-- Right (SectionHeader 1 (GtkDoc [Literal "Hello!"]))
parseInitialSectionHeader :: Parser Token
parseInitialSectionHeader = do
  hashes <- takeWhile1 (== '#')
  _ <- many1 space
  heading <- takeWhile1 (notInClass "#\n")
  _ <- (string hashes >> char '\n') <|> (char '\n')
  return $ SectionHeader (T.length hashes) (parseGtkDoc heading)

-- | Parse a list header. Note that the newline before the start of
-- the list is "eaten" by this parser, but is restored later by
-- `parseGtkDoc`.
--
-- === __Examples__
-- >>> parseOnly (parseList <* endOfInput) "\n- First item\n- Second item"
-- Right (List [ListItem (GtkDoc [Literal "First item"]) [],ListItem (GtkDoc [Literal "Second item"]) []])
--
-- >>> parseOnly (parseList <* endOfInput) "\n\n- Two line\n  item\n\n- Second item,\n  also two lines"
-- Right (List [ListItem (GtkDoc [Literal "Two line"]) [GtkDoc [Literal "item"]],ListItem (GtkDoc [Literal "Second item,"]) [GtkDoc [Literal "also two lines"]]])
parseList :: Parser Token
parseList = do
  items <- many1 parseListItem
  return $ List items
  where parseListItem :: Parser ListItem
        parseListItem = do
          _ <- char '\n'
          _ <- string "\n- " <|> string "- "
          first <- takeWhile1 (/= '\n')
          rest <- many' parseLine
          return $ ListItem (parseGtkDoc first) (map parseGtkDoc rest)

        parseLine :: Parser Text
        parseLine = string "\n  " >> takeWhile1 (/= '\n')
