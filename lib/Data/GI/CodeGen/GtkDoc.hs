-- | A parser for gtk-doc formatted documentation, see
-- https://developer.gnome.org/gtk-doc-manual/ for the spec.
module Data.GI.CodeGen.GtkDoc
  ( parseGtkDoc
  , GtkDoc(..)
  , Token(..)
  , Language(..)
  , Link(..)
  , CRef(..)
  , DocSymbolName(..)
  , docName
  , resolveDocSymbol
  ) where

import Prelude hiding (takeWhile)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*))
#endif
import Control.Applicative ((<|>))
import Control.Monad (forM, guard, when)
import Data.Either (isRight)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif

import Data.GI.CodeGen.Util (terror)
import Data.GI.GIR.BasicTypes (Name(Name))

import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAlpha, isAscii, isDigit)
import qualified Data.Text as T
import Data.Text (Text)

-- | A parsed gtk-doc token.
data Token = Literal Text
           | Comment Text
           | Verbatim Text
           | CodeBlock (Maybe Language) Text
           | ExternalLink Link
           | Image Link
           | UnnumberedList [GtkDoc]
           -- ^ An unnumbered list of items.
           | NumberedList [(Text, GtkDoc)]
           -- ^ A list of numbered list items. The first element in
           -- the pair is the index.
           | SectionHeader Int GtkDoc -- ^ A section header of the given depth.
           | SymbolRef CRef
  deriving (Show, Eq)

-- | A link to a resource, either offline or a section of the documentation.
data Link = Link { linkName :: Text
                 , linkAddress :: Text }
  deriving (Show, Eq)

-- | The language for an embedded code block.
newtype Language = Language Text
  deriving (Show, Eq)

-- | A reference to some symbol in the API.
data CRef = FunctionRef DocSymbolName
          | OldFunctionRef Text
          | MethodRef DocSymbolName Text
          | ParamRef Text
          | ConstantRef Text
          | SignalRef DocSymbolName Text
          | OldSignalRef Text Text
          | LocalSignalRef Text
          | PropertyRef DocSymbolName Text
          | OldPropertyRef Text Text
          | VMethodRef Text Text
          | VFuncRef DocSymbolName Text
          | StructFieldRef Text Text
          | EnumMemberRef DocSymbolName Text
          | CTypeRef Text
          | TypeRef DocSymbolName
          deriving (Show, Eq, Ord)

-- | Reference to a name (of a class, for instance) in the
-- documentation. It can be either relative to the module where the
-- documentation is, of in some other namespace.
data DocSymbolName = RelativeName Text
                     -- ^ The symbol without a namespace specified
                   | AbsoluteName Text Text
                     -- ^ Namespace and symbol
  deriving (Show, Eq, Ord)

-- | A parsed gtk-doc with fully resolved references.
newtype GtkDoc = GtkDoc [Token]
  deriving (Show, Eq)

-- | Parse the given gtk-doc formatted documentation.
--
-- === __Examples__
-- >>> parseGtkDoc ""
-- GtkDoc []
--
-- >>> parseGtkDoc "func()"
-- GtkDoc [SymbolRef (OldFunctionRef "func")]
--
-- >>> parseGtkDoc "literal"
-- GtkDoc [Literal "literal"]
--
-- >>> parseGtkDoc "This is a long literal"
-- GtkDoc [Literal "This is a long literal"]
--
-- >>> parseGtkDoc "Call foo() for free cookies"
-- GtkDoc [Literal "Call ",SymbolRef (OldFunctionRef "foo"),Literal " for free cookies"]
--
-- >>> parseGtkDoc "The signal ::activate is related to gtk_button_activate()."
-- GtkDoc [Literal "The signal ",SymbolRef (LocalSignalRef "activate"),Literal " is related to ",SymbolRef (OldFunctionRef "gtk_button_activate"),Literal "."]
--
-- >>> parseGtkDoc "The signal ##%#GtkButton::activate is related to gtk_button_activate()."
-- GtkDoc [Literal "The signal ##%",SymbolRef (OldSignalRef "GtkButton" "activate"),Literal " is related to ",SymbolRef (OldFunctionRef "gtk_button_activate"),Literal "."]
--
-- >>> parseGtkDoc "# A section\n\n## and a subsection ##\n"
-- GtkDoc [SectionHeader 1 (GtkDoc [Literal "A section"]),Literal "\n",SectionHeader 2 (GtkDoc [Literal "and a subsection "])]
--
-- >>> parseGtkDoc "Compact list:\n- First item\n- Second item"
-- GtkDoc [Literal "Compact list:\n",UnnumberedList [GtkDoc [Literal "First item"],GtkDoc [Literal "Second item"]]]
--
-- >>> parseGtkDoc "Spaced list:\n\n- First item\n\n- Second item"
-- GtkDoc [Literal "Spaced list:\n\n",UnnumberedList [GtkDoc [Literal "First item"],GtkDoc [Literal "Second item"]]]
--
-- >>> parseGtkDoc "List with urls:\n- [test](http://test)\n- ![](image.png)"
-- GtkDoc [Literal "List with urls:\n",UnnumberedList [GtkDoc [ExternalLink (Link {linkName = "test", linkAddress = "http://test"})],GtkDoc [Image (Link {linkName = "", linkAddress = "image.png"})]]]
parseGtkDoc :: Text -> GtkDoc
parseGtkDoc doc = rawParseGtkDoc (T.cons startOfString doc)

-- | Like `parseGtkDoc`, but it does not annotate beginning of lines.
rawParseGtkDoc :: Text -> GtkDoc
rawParseGtkDoc raw =
  case parseOnly (parseTokens <* endOfInput) raw of
    Left e ->
      terror $ "gtk-doc parsing failed with error \"" <> T.pack e
      <> "\" on the input \"" <>
      T.replace (T.singleton startOfString) "" raw <> "\""
    Right tks -> GtkDoc . coalesceLiterals . removeSOS $ tks

-- | A character indicating the start of the string, to simplify the
-- GtkDoc parser (part of the syntax is sensitive to the start of
-- lines, which we can represent as any character after '\n' or SOS).
startOfString :: Char
startOfString = '\x98' -- Unicode Start Of String (SOS)

-- | Remove the SOS marker from the input. Since this only appears at
-- the beginning of the text, we only need to worry about replacing it
-- in the first token, and only if it's a literal.
removeSOS :: [Token] -> [Token]
removeSOS [] = []
removeSOS (Literal l : rest) =
  if l == T.singleton startOfString
  then rest
  else Literal (T.replace (T.singleton startOfString) "" l) : rest
removeSOS (other : rest) = other : rest

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
          return (header <> tokens)

        justTokens :: Parser [Token]
        justTokens = concat <$> many' parseToken

-- | Parse a single token. This can sometimes return more than a
-- single token, when parsing a logical token produces multiple output
-- tokens (for example when keeping the initial structure requires
-- adding together literals and other tokens).
--
-- === __Examples__
-- >>> parseOnly (parseToken <* endOfInput) "func()"
-- Right [SymbolRef (OldFunctionRef "func")]
parseToken :: Parser [Token]
parseToken = -- Note that the parsers overlap, so this is not as
             -- efficient as it could be (if we had combined parsers
             -- and then branched, so that there is no
             -- backtracking). But speed is not an issue here, so for
             -- clarity we keep the parsers distinct. The exception
             -- is parseFunctionRef, since it does not complicate the
             -- parser much, and it is the main source of
             -- backtracking.
                 parseFunctionRef
             <|> parseMethod
             <|> parseConstructor
             <|> parseSignal
             <|> parseId
             <|> parseLocalSignal
             <|> parseProperty
             <|> parseVMethod
             <|> parseStructField
             <|> parseClass
             <|> parseCType
             <|> parseConstant
             <|> parseEnumMember
             <|> parseParam
             <|> parseEscaped
             <|> parseCodeBlock
             <|> parseVerbatim
             <|> parseUrl
             <|> parseImage
             <|> parseSectionHeader
             <|> parseUnnumberedList
             <|> parseNumberedList
             <|> parseComment
             <|> parseBoringLiteral

-- | Whether the given character is valid in a C identifier.
isCIdent :: Char -> Bool
isCIdent '_' = True
isCIdent c   = isAscii c && isAlphaNum c

-- | Something that could be a valid C identifier (loosely speaking,
-- we do not need to be too strict here).
parseCIdent :: Parser Text
parseCIdent = takeWhile1 isCIdent

-- | Parse a function ref
parseFunctionRef :: Parser [Token]
parseFunctionRef = parseOldFunctionRef <|> parseNewFunctionRef

-- | Parse an unresolved reference to a C symbol in new gtk-doc notation.
parseId :: Parser [Token]
parseId = do
  _ <- string "[id@"
  ident <- parseCIdent
  _ <- char ']'
  return [SymbolRef (OldFunctionRef ident)]

-- | Parse a function ref, given by a valid C identifier followed by
-- '()', for instance 'gtk_widget_show()'. If the identifier is not
-- followed by "()", return it as a literal instead.
--
-- === __Examples__
-- >>> parseOnly (parseFunctionRef <* endOfInput) "test_func()"
-- Right [SymbolRef (OldFunctionRef "test_func")]
--
-- >>> parseOnly (parseFunctionRef <* endOfInput) "not_a_func"
-- Right [Literal "not_a_func"]
parseOldFunctionRef :: Parser [Token]
parseOldFunctionRef = do
  ident <- parseCIdent
  option [Literal ident] (string "()" >>
                          return [SymbolRef (OldFunctionRef ident)])

-- | Parse a function name in new style, of the form
-- > [func@Namespace.c_func_name]
--
-- === __Examples__
-- >>> parseOnly (parseFunctionRef <* endOfInput) "[func@Gtk.init]"
-- Right [SymbolRef (FunctionRef (AbsoluteName "Gtk" "init"))]
parseNewFunctionRef :: Parser [Token]
parseNewFunctionRef = do
  _ <- string "[func@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- takeWhile1 isCIdent
  _ <- char ']'
  return [SymbolRef $ FunctionRef (AbsoluteName ns n)]

-- | Parse a method name, of the form
-- > [method@Namespace.Object.c_func_name]
--
-- === __Examples__
-- >>> parseOnly (parseMethod <* endOfInput) "[method@Gtk.Button.set_child]"
-- Right [SymbolRef (MethodRef (AbsoluteName "Gtk" "Button") "set_child")]
--
-- >>> parseOnly (parseMethod <* endOfInput) "[func@Gtk.Settings.get_for_display]"
-- Right [SymbolRef (MethodRef (AbsoluteName "Gtk" "Settings") "get_for_display")]
parseMethod :: Parser [Token]
parseMethod = do
  _ <- string "[method@" <|> string "[func@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- takeWhile1 isCIdent
  _ <- char '.'
  method <- takeWhile1 isCIdent
  _ <- char ']'
  return [SymbolRef $ MethodRef (AbsoluteName ns n) method]

-- | Parse a reference to a constructor, of the form
-- > [ctor@Namespace.Object.c_func_name]
--
-- === __Examples__
-- >>> parseOnly (parseConstructor <* endOfInput) "[ctor@Gtk.Builder.new_from_file]"
-- Right [SymbolRef (MethodRef (AbsoluteName "Gtk" "Builder") "new_from_file")]
parseConstructor :: Parser [Token]
parseConstructor = do
  _ <- string "[ctor@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- takeWhile1 isCIdent
  _ <- char '.'
  method <- takeWhile1 isCIdent
  _ <- char ']'
  return [SymbolRef $ MethodRef (AbsoluteName ns n) method]

-- | Parse a reference to a type, of the form
-- > [class@Namespace.Name]
-- an interface of the form
-- > [iface@Namespace.Name]
-- or an enumeration type, of the form
-- > [enum@Namespace.Name]
--
-- === __Examples__
-- >>> parseOnly (parseClass <* endOfInput) "[class@Gtk.Dialog]"
-- Right [SymbolRef (TypeRef (AbsoluteName "Gtk" "Dialog"))]
--
-- >>> parseOnly (parseClass <* endOfInput) "[iface@Gtk.Editable]"
-- Right [SymbolRef (TypeRef (AbsoluteName "Gtk" "Editable"))]
--
-- >>> parseOnly (parseClass <* endOfInput) "[enum@Gtk.SizeRequestMode]"
-- Right [SymbolRef (TypeRef (AbsoluteName "Gtk" "SizeRequestMode"))]
--
-- >>> parseOnly (parseClass <* endOfInput) "[struct@GLib.Variant]"
-- Right [SymbolRef (TypeRef (AbsoluteName "GLib" "Variant"))]
parseClass :: Parser [Token]
parseClass = do
  _ <- string "[class@" <|> string "[iface@" <|>
       string "[enum@" <|> string "[struct@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- takeWhile1 isCIdent
  _ <- char ']'
  return [SymbolRef $ TypeRef (AbsoluteName ns n)]

-- | Parse a reference to a member of the enum, of the form
-- > [enum@Gtk.FontRendering.AUTOMATIC]
--
-- === __Examples__
-- >>> parseOnly (parseEnumMember <* endOfInput) "[enum@Gtk.FontRendering.AUTOMATIC]"
-- Right [SymbolRef (EnumMemberRef (AbsoluteName "Gtk" "FontRendering") "automatic")]
parseEnumMember :: Parser [Token]
parseEnumMember = do
  _ <- string "[enum@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- takeWhile1 isCIdent
  _ <- char '.'
  member <- takeWhile1 isCIdent
  _ <- char ']'
  -- Sometimes the references are written in uppercase while the name
  -- of the member in the introspection data is written in lowercase,
  -- so normalise everything to lowercase. (See the similar annotation
  -- in CtoHaskellMap.hs.)
  return [SymbolRef $ EnumMemberRef (AbsoluteName ns n) (T.toLower member)]

parseSignal :: Parser [Token]
parseSignal = parseOldSignal <|> parseNewSignal

-- | Parse an old style signal name, of the form
-- > #Object::signal
--
-- === __Examples__
-- >>> parseOnly (parseOldSignal <* endOfInput) "#GtkButton::activate"
-- Right [SymbolRef (OldSignalRef "GtkButton" "activate")]
parseOldSignal :: Parser [Token]
parseOldSignal = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- string "::"
  signal <- signalOrPropName
  return [SymbolRef (OldSignalRef obj signal)]

-- | Parse a new style signal ref, of the form
-- > [signal@Namespace.Object::signal-name]
--
-- === __Examples__
-- >>> parseOnly (parseNewSignal <* endOfInput) "[signal@Gtk.AboutDialog::activate-link]"
-- Right [SymbolRef (SignalRef (AbsoluteName "Gtk" "AboutDialog") "activate-link")]
parseNewSignal :: Parser [Token]
parseNewSignal = do
  _ <- string "[signal@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- parseCIdent
  _ <- string "::"
  signal <- takeWhile1 (\c -> (isAscii c && isAlpha c) || c == '-')
  _ <- char ']'
  return [SymbolRef (SignalRef (AbsoluteName ns n) signal)]

-- | Parse a reference to a signal defined in the current module, of the form
-- > ::signal
--
-- === __Examples__
-- >>> parseOnly (parseLocalSignal <* endOfInput) "::activate"
-- Right [SymbolRef (LocalSignalRef "activate")]
parseLocalSignal :: Parser [Token]
parseLocalSignal = do
  _ <- string "::"
  signal <- signalOrPropName
  return [SymbolRef (LocalSignalRef signal)]

-- | Parse a property name in the old style, of the form
-- > #Object:property
--
-- === __Examples__
-- >>> parseOnly (parseOldProperty <* endOfInput) "#GtkButton:always-show-image"
-- Right [SymbolRef (OldPropertyRef "GtkButton" "always-show-image")]
parseOldProperty :: Parser [Token]
parseOldProperty = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char ':'
  property <- signalOrPropName
  return [SymbolRef (OldPropertyRef obj property)]

-- | Parse a property name in the new style:
-- > [property@Namespace.Object:property-name]
--
-- === __Examples__
-- >>> parseOnly (parseNewProperty <* endOfInput) "[property@Gtk.ProgressBar:show-text]"
-- Right [SymbolRef (PropertyRef (AbsoluteName "Gtk" "ProgressBar") "show-text")]
-- >>> parseOnly (parseNewProperty <* endOfInput) "[property@Gtk.Editable:width-chars]"
-- Right [SymbolRef (PropertyRef (AbsoluteName "Gtk" "Editable") "width-chars")]
parseNewProperty :: Parser [Token]
parseNewProperty = do
  _ <- string "[property@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- parseCIdent
  _ <- char ':'
  property <- takeWhile1 (\c -> (isAscii c && isAlpha c) || c == '-')
  _ <- char ']'
  return [SymbolRef (PropertyRef (AbsoluteName ns n) property)]

-- | Parse a property
parseProperty :: Parser [Token]
parseProperty = parseOldProperty <|> parseNewProperty

-- | Parse an xml comment, of the form
-- > <!-- comment -->
-- Note that this function keeps spaces.
--
-- === __Examples__
-- >>> parseOnly (parseComment <* endOfInput) "<!-- comment -->"
-- Right [Comment " comment "]
parseComment :: Parser [Token]
parseComment = do
  comment <- string "<!--" *> manyTill anyChar (string "-->")
  return [Comment $ T.pack comment]

-- | Parse an old style reference to a virtual method, of the form
-- > #Struct.method()
--
-- === __Examples__
-- >>> parseOnly (parseOldVMethod <* endOfInput) "#Foo.bar()"
-- Right [SymbolRef (VMethodRef "Foo" "bar")]
parseOldVMethod :: Parser [Token]
parseOldVMethod = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char '.'
  method <- parseCIdent
  _ <- string "()"
  return [SymbolRef (VMethodRef obj method)]

-- | Parse a new style reference to a virtual function, of the form
-- > [vfunc@Namespace.Object.vfunc_name]
--
-- >>> parseOnly (parseVFunc <* endOfInput) "[vfunc@Gtk.Widget.get_request_mode]"
-- Right [SymbolRef (VFuncRef (AbsoluteName "Gtk" "Widget") "get_request_mode")]
parseVFunc :: Parser [Token]
parseVFunc = do
  _ <- string "[vfunc@"
  ns <- takeWhile1 (\c -> isAscii c && isAlpha c)
  _ <- char '.'
  n <- parseCIdent
  _ <- char '.'
  vfunc <- parseCIdent
  _ <- char ']'
  return [SymbolRef (VFuncRef (AbsoluteName ns n) vfunc)]

-- | Parse a reference to a virtual method
parseVMethod :: Parser [Token]
parseVMethod = parseOldVMethod <|> parseVFunc

-- | Parse a reference to a struct field, of the form
-- > #Struct.field
--
-- === __Examples__
-- >>> parseOnly (parseStructField <* endOfInput) "#Foo.bar"
-- Right [SymbolRef (StructFieldRef "Foo" "bar")]
parseStructField :: Parser [Token]
parseStructField = do
  _ <- char '#'
  obj <- parseCIdent
  _ <- char '.'
  field <- parseCIdent
  return [SymbolRef (StructFieldRef obj field)]

-- | Parse a reference to a C type, of the form
-- > #Type
--
-- === __Examples__
-- >>> parseOnly (parseCType <* endOfInput) "#Foo"
-- Right [SymbolRef (CTypeRef "Foo")]
parseCType :: Parser [Token]
parseCType = do
  _ <- char '#'
  obj <- parseCIdent
  return [SymbolRef (CTypeRef obj)]

-- | Parse a constant, of the form
-- > %CONSTANT_NAME
--
-- === __Examples__
-- >>> parseOnly (parseConstant <* endOfInput) "%TEST_CONSTANT"
-- Right [SymbolRef (ConstantRef "TEST_CONSTANT")]
parseConstant :: Parser [Token]
parseConstant = do
  _ <- char '%'
  c <- parseCIdent
  return [SymbolRef (ConstantRef c)]

-- | Parse a reference to a parameter, of the form
-- > @param_name
--
-- === __Examples__
-- >>> parseOnly (parseParam <* endOfInput) "@test_param"
-- Right [SymbolRef (ParamRef "test_param")]
parseParam :: Parser [Token]
parseParam = do
  _ <- char '@'
  param <- parseCIdent
  return [SymbolRef (ParamRef param)]

-- | Name of a signal or property name. Similar to a C identifier, but
-- hyphens are allowed too.
signalOrPropName :: Parser Text
signalOrPropName = takeWhile1 isSignalOrPropIdent
  where isSignalOrPropIdent :: Char -> Bool
        isSignalOrPropIdent '-' = True
        isSignalOrPropIdent c = isCIdent c

-- | Parse a escaped special character, i.e. one preceded by '\'.
parseEscaped :: Parser [Token]
parseEscaped = do
  _ <- char '\\'
  c <- satisfy (`elem` ("#@%\\`" :: [Char]))
  return [Literal (T.singleton c)]

-- | Parse a literal, i.e. anything without a known special
-- meaning. Note that this parser always consumes the first character,
-- regardless of what it is.
parseBoringLiteral :: Parser [Token]
parseBoringLiteral = do
  c <- anyChar
  boring <- takeWhile (not . special)
  return [Literal (T.cons c boring)]

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
special '-' = True
special c = isCIdent c

-- | Parse a verbatim string, of the form
-- > `verbatim text`
--
-- === __Examples__
-- >>> parseOnly (parseVerbatim <* endOfInput) "`Example quote!`"
-- Right [Verbatim "Example quote!"]
parseVerbatim :: Parser [Token]
parseVerbatim = do
  _ <- char '`'
  v <- takeWhile1 (/= '`')
  _ <- char '`'
  return [Verbatim v]

-- | Parse a URL in Markdown syntax, of the form
-- > [name](url)
--
-- === __Examples__
-- >>> parseOnly (parseUrl <* endOfInput) "[haskell](http://haskell.org)"
-- Right [ExternalLink (Link {linkName = "haskell", linkAddress = "http://haskell.org"})]
parseUrl :: Parser [Token]
parseUrl = do
  _ <- char '['
  name <- takeWhile1 (/= ']')
  _ <- string "]("
  address <- takeWhile1 (/= ')')
  _ <- char ')'
  return [ExternalLink $ Link {linkName = name, linkAddress = address}]

-- | Parse an image reference, of the form
-- > ![label](url)
--
-- === __Examples__
-- >>> parseOnly (parseImage <* endOfInput) "![](diagram.png)"
-- Right [Image (Link {linkName = "", linkAddress = "diagram.png"})]
parseImage :: Parser [Token]
parseImage = do
  _ <- string "!["
  name <- takeWhile (/= ']')
  _ <- string "]("
  address <- takeWhile1 (/= ')')
  _ <- char ')'
  return [Image $ Link {linkName = name, linkAddress = address}]

-- | Parse a code block embedded in the documentation.
parseCodeBlock :: Parser [Token]
parseCodeBlock = parseOldStyleCodeBlock <|> parseNewStyleCodeBlock

-- | Parse a new style code block, of the form
-- > ```c
-- > some c code
-- > ```
--
-- === __Examples__
-- >>> parseOnly (parseNewStyleCodeBlock <* endOfInput) "```c\nThis is C code\n```"
-- Right [CodeBlock (Just (Language "c")) "This is C code"]
--
-- >>> parseOnly (parseNewStyleCodeBlock <* endOfInput) "```\nThis is langless\n```"
-- Right [CodeBlock Nothing "This is langless"]
--
-- >>> parseOnly (parseNewStyleCodeBlock <* endOfInput) "   ```py\n   This has space in front\n   ```"
-- Right [CodeBlock (Just (Language "py")) "   This has space in front"]
--
-- >>> parseOnly (parseNewStyleCodeBlock <* endOfInput) "   ```c\n   new_type_id = g_type_register_dynamic (parent_type_id,\n                                          \"TypeName\",\n                                          new_type_plugin,\n                                          type_flags);\n   ```"
-- Right [CodeBlock (Just (Language "c")) "   new_type_id = g_type_register_dynamic (parent_type_id,\n                                          \"TypeName\",\n                                          new_type_plugin,\n                                          type_flags);"]
parseNewStyleCodeBlock :: Parser [Token]
parseNewStyleCodeBlock = do
  _ <- takeWhile isHorizontalSpace
  _ <- string "```"
  lang <- T.strip <$> takeWhile (/= '\n')
  _ <- char '\n'
  let maybeLang = if T.null lang then Nothing
                  else Just lang
  code <- T.pack <$> manyTill anyChar (string "\n" >>
                                       takeWhile isHorizontalSpace >>
                                       string "```")
  return [CodeBlock (Language <$> maybeLang) code]

-- | Parse an old style code block, of the form
-- > |[<!-- language="C" --> code ]|
--
-- === __Examples__
-- >>> parseOnly (parseOldStyleCodeBlock <* endOfInput) "|[this is code]|"
-- Right [CodeBlock Nothing "this is code"]
--
-- >>> parseOnly (parseOldStyleCodeBlock <* endOfInput) "|[<!-- language=\"C\"-->this is C code]|"
-- Right [CodeBlock (Just (Language "C")) "this is C code"]
parseOldStyleCodeBlock :: Parser [Token]
parseOldStyleCodeBlock = do
  _ <- string "|["
  lang <- (Just <$> parseLanguage) <|> return Nothing
  code <- T.pack <$> manyTill anyChar (string "]|")
  return [CodeBlock lang code]

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

-- | Parse at least one newline (or Start of String (SOS)), and keep
-- going while we see newlines. Return either the empty list (for the
-- case that we see a single SOS), or a singleton list with the
-- Literal representing the seen newlines, and removing the SOS.
parseInitialNewlines :: Parser [Token]
parseInitialNewlines = do
  initial <- char '\n' <|> char startOfString
  let initialString = if initial == '\n'
                      then "\n"
                      else ""
  others <- T.pack <$> many' (char '\n')
  let joint = initialString <> others
  if T.null joint
    then return []
    else return [Literal joint]

-- | Parse a section header, given by a number of hash symbols, and
-- then ordinary text. Note that this parser "eats" the newline before
-- and after the section header.
parseSectionHeader :: Parser [Token]
parseSectionHeader = do
  initialNewlines <- parseInitialNewlines
  sectionHeader <- parseInitialSectionHeader
  return $ initialNewlines <> sectionHeader

-- | Parse a section header at the beginning of the text. I.e. this is
-- the same as `parseSectionHeader`, but we do not expect a newline as
-- a first character.
--
-- === __Examples__
-- >>> parseOnly (parseInitialSectionHeader <* endOfInput) "### Hello! ###\n"
-- Right [SectionHeader 3 (GtkDoc [Literal "Hello! "])]
--
-- >>> parseOnly (parseInitialSectionHeader <* endOfInput) "# Hello!\n"
-- Right [SectionHeader 1 (GtkDoc [Literal "Hello!"])]
parseInitialSectionHeader :: Parser [Token]
parseInitialSectionHeader = do
  hashes <- takeWhile1 (== '#')
  _ <- many1 space
  heading <- takeWhile1 (notInClass "#\n")
  _ <- (string hashes >> char '\n') <|> (char '\n')
  return [SectionHeader (T.length hashes) (parseGtkDoc heading)]

{- | Parse an unnumbered list.

=== __Examples__
>>> :{
parseOnly (parseUnnumberedList <* endOfInput) $ T.stripEnd $ T.unlines [
T.cons startOfString
"- First item",
"- Second item"
]
:}
Right [UnnumberedList [GtkDoc [Literal "First item"],GtkDoc [Literal "Second item"]]]

>>> :{
parseOnly (parseUnnumberedList <* endOfInput) $ T.stripEnd $ T.unlines [
"",
"",
"- Two line",
"  item",
"",
"- Second item,",
"  with three lines",
"  of text."
]
:}
Right [Literal "\n\n",UnnumberedList [GtkDoc [Literal "Two line\nitem"],GtkDoc [Literal "Second item,\nwith three lines\nof text."]]]
-}
parseUnnumberedList :: Parser [Token]
parseUnnumberedList = do
  (initialNewlines, entries) <- parseList (string "- ") T.length
  return $ initialNewlines <> [UnnumberedList (map snd entries)]

{- | Parse a numbered list header.

=== __Examples__
>>> :{
parseOnly (parseNumberedList <* endOfInput) $ T.stripEnd $ T.unlines [
T.cons startOfString
"1. First item,",
"   written in two lines",
"",
"2. Second item,",
"   also in two lines"
]
:}
Right [NumberedList [("1",GtkDoc [Literal "First item,\nwritten in two lines"]),("2",GtkDoc [Literal "Second item,\nalso in two lines"])]]

>>> :{
parseOnly (parseNumberedList <* endOfInput) $ T.stripEnd $ T.unlines [
T.cons startOfString
"1. First item,",
"   written in two lines",
"2. Second item,",
"   now in three lines,",
"   written compactly"
]
:}
Right [NumberedList [("1",GtkDoc [Literal "First item,\nwritten in two lines"]),("2",GtkDoc [Literal "Second item,\nnow in three lines,\nwritten compactly"])]]

>>> :{
parseOnly (parseNumberedList <* endOfInput) $ T.stripEnd $ T.unlines [
T.cons startOfString
"9. This is a list entry with two lines,",
"   with the second line in its own line.",
"10. If the label width changes,",
"    the indentation of the second line should also be adjusted.",
"",
"11. You can optionally include an empty line between entries",
"    without stopping the list.",
"",
"    This also applies within list entries, this is still part of",
"    entry 11.",
"12. But you don't have to."
]
:}
Right [NumberedList [("9",GtkDoc [Literal "This is a list entry with two lines,\nwith the second line in its own line."]),("10",GtkDoc [Literal "If the label width changes,\nthe indentation of the second line should also be adjusted."]),("11",GtkDoc [Literal "You can optionally include an empty line between entries\nwithout stopping the list.\n\nThis also applies within list entries, this is still part of\nentry 11."]),("12",GtkDoc [Literal "But you don't have to."])]]

>>> :{
parseGtkDoc $ T.stripEnd $ T.unlines [
"1. A list with a single element",
"",
"And this is text not in the list, so we use parseGtkDoc."
]
:}
GtkDoc [NumberedList [("1",GtkDoc [Literal "A list with a single element"])],Literal "\n\nAnd this is text not in the list, so we use parseGtkDoc."]

>>> :{
parseOnly (parseNumberedList <* endOfInput) $ T.stripEnd $ T.unlines [
T.cons startOfString
"1. An example of a list in lenient mode,",
"where we don't require indenting this second line.",
"",
"2. In this mode entries can be optionally separated by an empty line.",
"3. But they don't need to"
]
:}
Right [NumberedList [("1",GtkDoc [Literal "An example of a list in lenient mode,\nwhere we don't require indenting this second line."]),("2",GtkDoc [Literal "In this mode entries can be optionally separated by an empty line."]),("3",GtkDoc [Literal "But they don't need to"])]]
-}
parseNumberedList :: Parser [Token]
parseNumberedList = do
  (initialNewlines, list) <- parseList (do idx <- takeWhile1 isDigit
                                           _ <- string ". "
                                           return idx)
                                       (\label -> T.length label + 2)
  return $ initialNewlines <> [NumberedList list]

{- | The indent parsing mode. In strict mode we require that all the
   text in the lines is indented relative to the label, as in the
   following example:

        1. The first line,
           and the second line

           In this mode we allow empty lines in the entry.
        2. This is the second entry.

   In lenient mode we drop this restriction, so the following is valid:
        1. The first line,
        and the second line
        In this mode we _do not_ allow empty lines in the entry.
        2. This is the second entry.
-}
data IndentParsingMode = Lenient | Strict
  deriving (Eq)

{- | Parse an unnumbered or numbered list. See 'parseNumberedList' and
   'parseUnnumberedList' for examples.
-}
parseList :: Parser Text -> (Text -> Int) ->
                    Parser ([Token], [(Text, GtkDoc)])
parseList labelParser indent =
  doParseList Lenient <|> doParseList Strict
 where
   doParseList :: IndentParsingMode ->
                  Parser ([Token], [(Text, GtkDoc)])
   doParseList mode = do
     -- Consume the initial newlines before parseListItem does, so we can
     -- restore the initial newlines after. We impose that there is at
     -- least a newline (or Start of String symbol) before the start of
     -- the list.
     initialNewlines <- parseInitialNewlines
     (initialSpace, first) <-
       parseListItem (takeWhile isHorizontalSpace) (pure ())
     -- We allow either one or zero empty lines between entries.
     let newlineParser = (string "\n\n" <|> string "\n") >> pure ()
     rest <- map snd <$>
             many' (parseListItem (string initialSpace) newlineParser)
     -- Validate the resulting entries, and assemble them into GtkDoc.
     validated <- forM (first : rest) $ \(label, (firstLine, otherLines)) -> do
       validate label otherLines
       return (label,
               parseGtkDoc $ T.strip $ T.unlines $ firstLine : otherLines)

     return (initialNewlines, validated)

    where
      parseListItem :: Parser Text -> Parser () ->
                         Parser (Text, (Text, (Text, [Text])))
      parseListItem parseInitialSpace startingNewlines = do
        startingNewlines
        initialSpace <- parseInitialSpace
        label <- labelParser
        first <- takeWhile (/= '\n')
        let padding = case mode of
              Strict -> initialSpace <> T.replicate (indent label) " "
              Lenient -> initialSpace
            paddingParser = string padding

        rest <- many' (parseLine paddingParser)

        return (initialSpace, (label, (first, rest)))

      parseLine :: Parser Text -> Parser Text
      parseLine paddingParser = do
        emptyLines <- case mode of
          -- We do not allow empty lines in entries in the lenient
          -- indent parser, while the strict indent one allows one
          -- at most.
          Strict -> option "" emptyLine
          Lenient -> pure ""
        _ <- char '\n' >> paddingParser
        contents <- takeWhile1 (/= '\n')
        when (startsWith labelParser contents) $
          fail "Line starting with a label"
        return $ emptyLines <> contents

      emptyLine = do
        _ <- char '\n'
        maybeNext <- peekChar
        guard $ maybeNext == Nothing || maybeNext == Just '\n'
        return ("\n" :: Text)

      startsWith :: Parser a -> Text -> Bool
      startsWith p l = isRight $ parseOnly p l

      validate :: Text -> [Text] -> Parser ()
      validate _ [] = pure ()
      validate label lines = case mode of
        Strict -> pure ()
        Lenient -> do
          let extraIndent = string $ T.replicate (indent label) " "

          -- If every line has extra padding we are most likely in
          -- the wrong mode too.
          when (all (startsWith extraIndent) lines) $
            fail "All lines have extra indent"

-- | Turn an ordinary `Name` into a `DocSymbolName`
docName :: Name -> DocSymbolName
docName (Name ns n) = AbsoluteName ns n

-- | Return a `Name` from a potentially relative `DocSymbolName`,
-- using the provided default namespace if the name is relative.
resolveDocSymbol :: DocSymbolName -> Text -> Name
resolveDocSymbol (AbsoluteName ns n) _ = Name ns n
resolveDocSymbol (RelativeName n) defaultNS = Name defaultNS n
