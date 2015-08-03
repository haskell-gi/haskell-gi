{-# LANGUAGE OverloadedStrings, RecordWildCards, PatternGuards, NamedFieldPuns #-}

module GI.API
    ( API(..)
    , Name(..)
    , Constant(..)
    , Arg(..)
    , Callable(..)
    , Function(..)
    , Signal(..)
    , Property(..)
    , Field(..)
    , Struct(..)
    , Callback(..)
    , Interface(..)
    , Object(..)
    , Enumeration(..)
    , Flags (..)
    , Union (..)

    , Scope(..) -- from GI.Internal.ArgInfo, for convenience

    , deprecatedPragma
    , DeprecationInfo(..)

    , loadAPI

    , loadGIRInfo
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative ((<|>))

import Data.Int
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import Foreign.Storable (sizeOf)
import Foreign.C (CInt, CUInt, CLong, CULong)

import Text.XML hiding (Name)

import GI.Internal.ArgInfo
import GI.Internal.FieldInfo
import GI.Internal.FunctionInfo
import GI.Internal.PropertyInfo
import GI.Internal.TypeInfo
import GI.Repository (readGiRepository)
import GI.Type

data Name = Name { namespace :: String, name :: String }
    deriving (Eq, Ord, Show)

-- (Namespace, name)
type Alias = (Text, Text)

data ParseContext = ParseContext {
      currentNamespace :: Text,
      knownAliases     :: M.Map Alias Type
    }

data DeprecationInfo = DeprecationInfo {
      deprecatedSinceVersion :: Maybe Text,
      deprecationMessage     :: Maybe Text
    } deriving (Show, Eq)

deprecatedPragma :: String -> Maybe DeprecationInfo -> String
deprecatedPragma _    Nothing     = ""
deprecatedPragma name (Just info) = "{-# DEPRECATED " ++ name ++ reason ++ note ++ "#-}"
        where reason = case deprecationMessage info of
                         Nothing -> " <no reason given for deprecation> "
                         Just msg -> " \"" ++ T.unpack msg ++ "\" "
              note = case deprecatedSinceVersion info of
                       Nothing -> ""
                       Just v -> "(since version " ++ T.unpack v ++ " ) "

{-
getName :: InfoClass info => info -> Name
getName i = Name namespace name
    where namespace = infoNamespace i
          name = infoName i

withName :: InfoClass info => (info -> a) -> (info -> (Name, a))
withName f x = (getName x, f x)
-}

-- * Some helpers for making traversals easier.

-- | Turn a node into an element (it is indeed an element node).
nodeToElement :: Node -> Maybe Element
nodeToElement (NodeElement e) = Just e
nodeToElement _               = Nothing

-- | Find all children of the given element which are XML Elements
-- themselves.
subelements :: Element -> [Element]
subelements = mapMaybe nodeToElement . elementNodes

-- | The local name of an element.
localName :: Element -> Text
localName = nameLocalName . elementName

-- | Restrict to those with the given local name.
childElemsWithLocalName :: Text -> Element -> [Element]
childElemsWithLocalName n =
    filter localNameMatch . subelements
    where localNameMatch = (== n) . localName

-- | Find the first child element with the given name.
firstChildWithLocalName :: Text -> Element -> Maybe Element
firstChildWithLocalName n = listToMaybe . childElemsWithLocalName n

-- | Get the content of a given element, if it exists.
getElementContent :: Element -> Maybe Text
getElementContent = listToMaybe . mapMaybe getContent . elementNodes
    where getContent :: Node -> Maybe Text
          getContent (NodeContent t) = Just t
          getContent _ = Nothing

data TypeElement = TypeElement Element | ArrayElement Element

-- | Find the children giving the type of the given element.
findTypeElements :: Element -> [TypeElement]
findTypeElements = mapMaybe toTypeElement . subelements
    where toTypeElement :: Element -> Maybe TypeElement
          toTypeElement elem
              | "type" <- localName elem = Just (TypeElement elem)
              | "array" <- localName elem = Just (ArrayElement elem)
          toTypeElement _ = Nothing

-- | Map the given type name to a BasicType (in GI.Type), if possible.
nameToBasicType :: Text -> Maybe BasicType
nameToBasicType "none"     = Just TVoid
-- XXX Should we try to distinguish TPtr from TVoid?
nameToBasicType "gpointer" = Just TVoid
nameToBasicType "gboolean" = Just TBoolean
nameToBasicType "gint8"    = Just TInt8
nameToBasicType "guint8"   = Just TUInt8
nameToBasicType "gint16"   = Just TInt16
nameToBasicType "guint16"  = Just TUInt16
nameToBasicType "gint32"   = Just TInt32
nameToBasicType "guint32"  = Just TUInt32
nameToBasicType "gint64"   = Just TInt64
nameToBasicType "guint64"  = Just TUInt64
nameToBasicType "gfloat"   = Just TFloat
nameToBasicType "gdouble"  = Just TDouble
nameToBasicType "gunichar" = Just TUniChar
nameToBasicType "gtype"    = Just TGType
nameToBasicType "utf8"     = Just TUTF8
nameToBasicType "filename" = Just TFileName
nameToBasicType "gint"     = case sizeOf (0 :: CInt) of
                                 4 -> Just TInt32
                                 8 -> Just TInt64
                                 n -> error $ "Unexpected length: " ++ show n
nameToBasicType "guint"    = case sizeOf (0 :: CUInt) of
                                 4 -> Just TUInt32
                                 8 -> Just TUInt64
                                 n -> error $ "Unexpected length: " ++ show n
nameToBasicType "glong"    = case sizeOf (0 :: CLong) of
                                 4 -> Just TInt32
                                 8 -> Just TInt64
                                 n -> error $ "Unexpected length: " ++ show n
nameToBasicType "gulong"   = case sizeOf (0 :: CULong) of
                                 4 -> Just TUInt32
                                 8 -> Just TUInt64
                                 n -> error $ "Unexpected length: " ++ show n
nameToBasicType _          = Nothing

-- | Parse a signed integer.
parseInteger :: Text -> Maybe Int
parseInteger str = case TR.signed TR.decimal str of
                     Right (n, r) | T.null r -> Just n
                     _ -> Nothing

-- | A boolean value given by a numerical constant.
parseBool :: Text -> Maybe Bool
parseBool "0" = Just False
parseBool "1" = Just True
parseBool _   = Nothing

-- | The different array types.
parseArrayType :: ParseContext -> Element -> Maybe Type
parseArrayType ctx elem =
    case M.lookup "name" (elementAttributes elem) of
      Just "GLib.Array" -> TGArray <$> parseType ctx elem
      Just "GLib.PtrArray" -> TPtrArray <$> parseType ctx elem
      Just "GLib.ByteArray" -> Just TByteArray
      Just _ -> Nothing
      Nothing -> parseCArrayType ctx elem

-- | A C array
parseCArrayType :: ParseContext -> Element -> Maybe Type
parseCArrayType ctx element = do
  let attrs = elementAttributes element
      length = fromMaybe (-1) (M.lookup "length" attrs >>= parseInteger)
      zeroTerminated = fromMaybe True (M.lookup "zero-terminated" attrs >>= parseBool)
      fixedSize = fromMaybe (-1) (M.lookup "fixed-size" attrs >>= parseInteger)
  elementType <- parseType ctx element
  return $ TCArray zeroTerminated fixedSize length elementType

-- | A hash table.
parseHashTable :: ParseContext -> Element -> Maybe Type
parseHashTable ctx elem =
    case findTypeElements elem of
      [key, value] -> TGHash <$> parseTypeElement ctx key <*> parseTypeElement ctx value
      _ -> Nothing

-- | A type which is not a BasicType or array.
parseFundamentalType :: Text -> ParseContext -> Element -> Maybe Type
parseFundamentalType "GLib.List" ctx elem = TGList <$> parseType ctx elem
parseFundamentalType "GLib.SList" ctx elem = TGSList <$> parseType ctx elem
parseFundamentalType "GLib.HashTable" ctx elem = parseHashTable ctx elem
parseFundamentalType "GLib.Error" _ _ = Just TError
parseFundamentalType "GLib.Variant" _ _ = Just TVariant
parseFundamentalType "GObject.ParamSpec" _ _ = Just TParamSpec
parseFundamentalType iface ctx _ = parseInterface iface ctx

-- | An interface type (basically, everything that is not of a known type).
parseInterface :: Text -> ParseContext -> Maybe Type
parseInterface iface ParseContext{..} =
    case T.split (== '.') iface of
      [ns, n] -> Just $ checkAliases ns n
      [n]     -> Just $ checkAliases currentNamespace n
      _       -> Nothing
    where checkAliases :: Text -> Text -> Type
          checkAliases ns name =
              case M.lookup (ns, name) knownAliases of
                Just t -> t
                Nothing -> TInterface (T.unpack ns) (T.unpack name)

-- | Parse the type of a node (which will be described by a child node
-- named "type" or "array").
parseType :: ParseContext -> Element -> Maybe Type
parseType ctx element =
    case findTypeElements element of
      [e] -> parseTypeElement ctx e
      _ -> Nothing -- If there is not precisely one type element it is
                   -- not clear what to do.

-- | Parse a single type element (the "type" or "array" element itself).
parseTypeElement :: ParseContext -> TypeElement -> Maybe Type
parseTypeElement ctx (TypeElement e) = do
  typeName <- M.lookup "name" (elementAttributes e)
  (TBasicType <$> nameToBasicType typeName) <|> parseFundamentalType typeName ctx e
parseTypeElement ctx (ArrayElement e) = parseArrayType ctx e

parseDeprecation :: Element -> Maybe DeprecationInfo
parseDeprecation element =
    case M.lookup "deprecated" attrs of
      Just _ -> let version = M.lookup "deprecated-version" attrs
                    msg = firstChildWithLocalName "doc-deprecated" element >>=
                          getElementContent
                in Just (DeprecationInfo version msg)
      Nothing -> Nothing
    where attrs = elementAttributes element

-- | Build a name in the current namespace.
nameInCurrentNS :: ParseContext -> Text -> Name
nameInCurrentNS ctx n = Name (T.unpack (currentNamespace ctx)) (T.unpack n)

data Constant = Constant {
      constantType        :: Type,
      constantValue       :: Text,
      constantDeprecated  :: Maybe DeprecationInfo
    } deriving (Show)

parseConstant :: ParseContext -> Element -> Maybe (Name, API)
parseConstant ctx element = do
  let attrs = elementAttributes element
  name <- M.lookup "name" attrs
  value <- M.lookup "value" attrs
  t <- parseType ctx element
  return (nameInCurrentNS ctx name,
          APIConst (Constant t value (parseDeprecation element)))

data Enumeration = Enumeration {
    enumValues :: [(String, Int64)],
    errorDomain :: Maybe String,
    enumTypeInit :: Maybe String,
    enumStorageType :: TypeTag,
    enumDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toEnumeration :: EnumInfo -> Enumeration
toEnumeration ei = Enumeration
    (map viToV $ enumInfoValues ei)
    (enumInfoErrorDomain ei)
    (registeredTypeInfoTypeInit ei)
    (enumInfoStorageType ei)
    (infoDeprecated ei)
    where viToV vi = (infoName vi, valueInfoValue vi)
-}

data Flags = Flags Enumeration
    deriving Show

{-
toFlags :: EnumInfo -> Flags
toFlags ei = Flags $ toEnumeration ei
-}

data Arg = Arg {
    argName :: String,
    argType :: Type,
    direction :: Direction,
    mayBeNull :: Bool,
    argScope :: Scope,
    argClosure :: Int,
    argDestroy :: Int,
    transfer :: Transfer }
    deriving (Show, Eq, Ord)

{-
toArg :: ArgInfo -> Arg
toArg ai = Arg
    (infoName ai)
    (typeFromTypeInfo . argInfoType $ ai)
    (argInfoDirection ai)
    (argInfoMayBeNull ai)
    (argInfoScope ai)
    (argInfoClosure ai)
    (argInfoDestroy ai)
    (argInfoOwnershipTransfer ai)
-}

data Callable = Callable {
    returnType :: Type,
    returnMayBeNull :: Bool,
    returnTransfer :: Transfer,
    returnAttributes :: [(String, String)],
    args :: [Arg],
    skipReturn :: Bool,
    callableDeprecated :: Maybe DeprecationInfo }
    deriving (Show, Eq)

{-
toCallable :: CallableInfo -> Callable
toCallable ci = Callable
    (typeFromTypeInfo $ callableInfoReturnType ci)
    (callableInfoMayReturnNull ci)
    (callableInfoCallerOwns ci)
    (callableInfoReturnAttributes ci)
    (map toArg $ callableInfoArgs ci)
    (callableInfoSkipReturn ci)
    (infoDeprecated ci)
-}

data Function = Function {
    fnSymbol :: String,
    fnCallable :: Callable,
    fnFlags :: [FunctionInfoFlag] }
    deriving Show

{-
toFunction :: FunctionInfo -> Function
toFunction fi = Function
    (functionInfoSymbol fi)
    (toCallable ci)
    (functionInfoFlags fi)
    where ci = fromBaseInfo (baseInfo fi) :: CallableInfo
-}

data Signal = Signal {
    sigName :: String,
    sigCallable :: Callable,
    sigDeprecated :: Maybe DeprecationInfo }
    deriving (Show, Eq)

{-
toSignal :: SignalInfo -> Signal
toSignal si = Signal
    (infoName si)
    (toCallable $ callableInfo si)
    (infoDeprecated si)
-}

data Property = Property {
    propName :: String,
    propType :: Type,
    propFlags :: [ParamFlag],
    propTransfer :: Transfer,
    propDeprecated :: Maybe DeprecationInfo }
    deriving (Show, Eq)

{-
toProperty :: PropertyInfo -> Property
toProperty pi = Property
    (infoName pi)
    (typeFromTypeInfo $ propertyInfoType pi)
    (propertyInfoFlags pi)
    (propertyInfoTransfer pi)
    (infoDeprecated pi)
-}

data Field = Field {
    fieldName :: String,
    fieldType :: Type,
    fieldCallback :: Maybe Callback,
    fieldOffset :: Int,
    fieldFlags :: [FieldInfoFlag],
    fieldDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toField :: FieldInfo -> Field
toField fi = Field
    (infoName fi)
    (typeFromTypeInfo $ fieldInfoType fi)
     -- Fields with embedded "anonymous" callback interfaces.
    (case typeFromTypeInfo (fieldInfoType fi) of
       TInterface _ n ->
         if n /= infoName fi
         then Nothing
         else let iface = baseInfo . typeInfoInterface . fieldInfoType $ fi
              in case infoType iface of
                   InfoTypeCallback -> Just . toCallback . fromBaseInfo $ iface
                   _                -> Nothing
       _ -> Nothing)
    (fieldInfoOffset fi)
    (fieldInfoFlags fi)
    (infoDeprecated fi)
-}

data Struct = Struct {
    structIsBoxed :: Bool,
    structTypeInit :: Maybe String,
    structSize :: Int,
    structIsForeign :: Bool,
    isGTypeStruct :: Bool,
    structFields :: [Field],
    structMethods :: [(Name, Function)],
    structDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toStruct :: StructInfo -> Struct
toStruct si = Struct
    (isJust (registeredTypeInfoTypeInit si) &&
        gtypeIsBoxed (registeredTypeInfoGType si))
    (registeredTypeInfoTypeInit si)
    (structInfoSize si)
    (structInfoIsForeign si)
    (structInfoIsGTypeStruct si)
    (map toField $ structInfoFields si)
    (map (withName toFunction) (structInfoMethods si))
    (infoDeprecated si)
-}

-- XXX: Capture alignment and method info.

data Union = Union {
    unionIsBoxed :: Bool,
    unionSize :: Int,
    unionTypeInit :: Maybe String,
    unionFields :: [Field],
    unionMethods :: [(Name, Function)],
    unionDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toUnion :: UnionInfo -> Union
toUnion ui = Union
    (isJust (registeredTypeInfoTypeInit ui) &&
        gtypeIsBoxed (registeredTypeInfoGType ui))
    (unionInfoSize ui)
    (registeredTypeInfoTypeInit ui)
    (map toField $ unionInfoFields ui)
    (map (withName toFunction) (unionInfoMethods ui))
    (infoDeprecated ui)
-}

-- XXX
data Callback = Callback Callable
    deriving Show

{-
toCallback = Callback . toCallable
-}

data Interface = Interface {
    ifConstants :: [(Name, Constant)],
    ifProperties :: [Property],
    ifSignals :: [Signal],
    ifPrerequisites :: [Name],
    ifTypeInit :: Maybe String,
    ifMethods :: [(Name, Function)],
    ifDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toInterface :: InterfaceInfo -> Interface
toInterface ii = Interface
    (map (withName toConstant) (interfaceInfoConstants ii))
    (map toProperty (interfaceInfoProperties ii))
    (map toSignal (interfaceInfoSignals ii))
    (map (fst . toAPI) (interfaceInfoPrerequisites ii))
    (registeredTypeInfoTypeInit ii)
    (map (withName toFunction) (interfaceInfoMethods ii))
    (infoDeprecated ii)
-}

data Object = Object {
    objFields :: [Field],
    objMethods :: [(Name, Function)],
    objProperties :: [Property],
    objSignals :: [Signal],
    objInterfaces :: [Name],
    objConstants :: [Constant],
    objParent :: Maybe Name,
    objTypeInit :: String,
    objTypeName :: String,
    objRefFunction :: Maybe String,
    objUnrefFunction :: Maybe String,
    objDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toObject :: ObjectInfo -> Object
toObject oi = Object
    (map toField $ objectInfoFields oi)
    (map (withName toFunction) (objectInfoMethods oi))
    (map toProperty $ objectInfoProperties oi)
    (map toSignal (objectInfoSignals oi))
    (map getName $ objectInfoInterfaces oi)
    (map toConstant $ objectInfoConstants oi)
    (getName <$> objectInfoParent oi)
    (objectInfoTypeInit oi)
    (objectInfoTypeName oi)
    (objectInfoRefFunction oi)
    (objectInfoUnrefFunction oi)
    (infoDeprecated oi)
-}

-- XXX: Work out what to do with boxed types.
data Boxed = Boxed
    deriving Show

{-
toBoxed :: BaseInfo -> Boxed
toBoxed _ = Boxed
-}

data API
    = APIConst Constant
    | APIFunction Function
    | APICallback Callback
    | APIEnum Enumeration
    | APIFlags Flags
    | APIInterface Interface
    | APIObject Object
    | APIStruct Struct
    | APIUnion Union
    | APIBoxed Boxed
    deriving Show

{-
toAPI :: BaseInfo -> (Name, API)
toAPI bi = (getName bi, toAPI' (infoType bi) bi)
    where

    toAPI' InfoTypeConstant = convert APIConst toConstant
    toAPI' InfoTypeEnum = convert APIEnum toEnumeration
    toAPI' InfoTypeFlags = convert APIFlags toFlags
    toAPI' InfoTypeFunction = convert APIFunction toFunction
    toAPI' InfoTypeCallback = convert APICallback toCallback
    toAPI' InfoTypeStruct = convert APIStruct toStruct
    toAPI' InfoTypeUnion = convert APIUnion toUnion
    toAPI' InfoTypeObject = convert APIObject toObject
    toAPI' InfoTypeInterface = convert APIInterface toInterface
    toAPI' InfoTypeBoxed = convert APIBoxed toBoxed
    toAPI' it = error $ "not expecting a " ++ show it

    convert fa fb bi = fa $ fb $ fromBaseInfo bi
-}

data GIRNamespace = GIRNamespace {
      girNSName      :: Text,
      girNSVersion   :: Text,
      girNSAPIs      :: [(Name, API)]
    } deriving (Show)

maybeAddAPI :: GIRNamespace -> Maybe (Name, API) -> GIRNamespace
maybeAddAPI ns Nothing = ns
maybeAddAPI ns@GIRNamespace{girNSAPIs} (Just k) = ns {girNSAPIs = k : girNSAPIs}

parseNamespaceElement :: ParseContext -> GIRNamespace -> Element -> GIRNamespace
parseNamespaceElement ctx ns@GIRNamespace{..} element =
    case nameLocalName (elementName element) of
      "alias" -> ns     -- Processed separately
      "constant" -> maybeAddAPI ns (parseConstant ctx element)
      _ -> ns

parseNamespace :: Element -> M.Map Alias Type -> Maybe GIRNamespace
parseNamespace element aliases = do
  let attrs = elementAttributes element
  name <- M.lookup "name" attrs
  version <- M.lookup "version" attrs
  let ns = GIRNamespace {
             girNSName         = name,
             girNSVersion      = version,
             girNSAPIs         = []
           }
      ctx = ParseContext name aliases
  return $ L.foldl' (parseNamespaceElement ctx) ns (subelements element)

parseInclude :: Element -> Maybe (Text, Text)
parseInclude element = do
  name <- M.lookup "name" attrs
  version <- M.lookup "version" attrs
  return (name, version)
      where attrs = elementAttributes element

parsePackage :: Element -> Maybe Text
parsePackage element = M.lookup "name" (elementAttributes element)

parseRootElement :: M.Map Alias Type -> GIRInfoParse -> Element -> GIRInfoParse
parseRootElement aliases info@GIRInfoParse{..} element =
    case nameLocalName (elementName element) of
      "include" -> info {girIPIncludes = parseInclude element : girIPIncludes}
      "package" -> info {girIPPackage = parsePackage element : girIPPackage}
      "namespace" -> info {girIPNamespaces = parseNamespace element aliases : girIPNamespaces}
      _ -> info

data GIRInfoParse = GIRInfoParse {
    girIPPackage    :: [Maybe Text],
    girIPIncludes   :: [Maybe (Text, Text)],
    girIPNamespaces :: [Maybe GIRNamespace]
} deriving (Show)

emptyGIRInfoParse :: GIRInfoParse
emptyGIRInfoParse = GIRInfoParse {
                      girIPPackage = [],
                      girIPIncludes = [],
                      girIPNamespaces = []
                    }

parseGIRDocument :: M.Map Alias Type -> Document -> GIRInfoParse
parseGIRDocument aliases doc = L.foldl' (parseRootElement aliases) emptyGIRInfoParse (subelements (documentRoot doc))

-- | Find all aliases in a given namespace. We assume that namespaces
-- are the first elements in the namespace, in order to avoid scanning
-- all the entries in the namespace.
namespaceListAliases :: Element -> M.Map Alias Type
namespaceListAliases ns = M.fromList $ maybe [] (go (subelements ns)) maybeNSName
    where maybeNSName = M.lookup "name" (elementAttributes ns)
          go :: [Element] -> Text -> [(Alias, Type)]
          go [] _ = []
          go (e:es) nsName
             | localName e /= "alias" = []
             | otherwise = case parseAlias nsName e of
                             Just n -> n : go es nsName
                             Nothing -> go es nsName

-- | Parse an alias
parseAlias :: Text -> Element -> Maybe (Alias, Type)
parseAlias nsName element = do
  name <- M.lookup "name" (elementAttributes element)
  t <- parseType ctx element
  return ((nsName, name), t)
  where ctx = ParseContext {currentNamespace = nsName,
                            knownAliases = M.empty}

-- | Find all aliases in a given document.
documentListAliases :: Document -> M.Map Alias Type
documentListAliases doc = M.unions (map namespaceListAliases namespaces)
    where namespaces = childElemsWithLocalName "namespace" (documentRoot doc)

-- | Parse the list of includes in a given document.
documentListIncludes :: Document -> S.Set (Text, Text)
documentListIncludes doc = S.fromList (mapMaybe parseInclude includes)
    where includes = childElemsWithLocalName "include" (documentRoot doc)

-- | Load a set of dependencies, recursively.
loadDependencies :: Bool                              -- Verbose
                 -> S.Set (Text, Text)                -- Requested
                 -> M.Map (Text, Text) Document       -- Loaded so far
                 -> IO (M.Map (Text, Text) Document)  -- New loaded set
loadDependencies verbose requested loaded
        | S.null requested = return loaded
        | otherwise = do
  let (name, version) = S.elemAt 0 requested
  doc <- readGiRepository verbose name (Just version)
  let newLoaded = M.insert (name, version) doc loaded
      newRequested = S.union requested (documentListIncludes doc)
      notYetLoaded = S.filter (/= (name, version)) newRequested
  loadDependencies verbose notYetLoaded newLoaded

-- | Load a given GIR file and recursively its dependencies
loadGIRFile :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> IO (Document,                    -- ^ loaded document
                   M.Map (Text, Text) Document) -- ^ dependencies
loadGIRFile verbose name version = do
  doc <- readGiRepository verbose name version
  deps <- loadDependencies verbose (documentListIncludes doc) M.empty
  return (doc, deps)

-- | Load and parse a GIR file, including its dependencies.
loadGIRInfo :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> IO (GIRInfoParse,                    -- ^ parsed document
                   M.Map (Text, Text) GIRInfoParse) -- ^ parsed deps
loadGIRInfo verbose name version =  do
  (doc, deps) <- loadGIRFile verbose name version
  let aliases = M.unions (map documentListAliases (doc : M.elems deps))
  return ( parseGIRDocument aliases doc
         , M.map (parseGIRDocument aliases) deps)

loadAPI :: Bool -> String -> Maybe String -> IO [(Name, API)]
loadAPI = error $ "This git branch is a work in progress, use the main git branch instead!"
