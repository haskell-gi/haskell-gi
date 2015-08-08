{-# LANGUAGE OverloadedStrings, RecordWildCards, PatternGuards, NamedFieldPuns #-}

module GI.API
    ( API(..)
    , GIRInfo(..)
    , loadGIRInfo

    -- Reexported from GI.GIR.BasicTypes.
    , Name(..)

    -- Reexported from GI.Internal.ArgInfo.
    , Scope(..)

    -- Reexported from GI.GIR.Deprecation.
    , deprecatedPragma
    , DeprecationInfo

    -- Reexported from the corresponding GI.GIR modules
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
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import Text.XML hiding (Name)

import GI.Internal.ArgInfo
import GI.Internal.FieldInfo
import GI.Internal.FunctionInfo
import GI.Internal.PropertyInfo
import GI.GIR.Alias (documentListAliases)
import GI.GIR.BasicTypes (ParseContext(..), Alias, Name(..))
import GI.GIR.Constant (Constant(..), parseConstant)
import GI.GIR.Deprecation (DeprecationInfo, deprecatedPragma)
import GI.GIR.Enum (Enumeration(..), parseEnum)
import GI.GIR.Repository (readGiRepository)
import GI.GIR.XMLUtils (subelements, childElemsWithLocalName)
import GI.Type (Type)

data GIRInfo = GIRInfo {
      girPCPackages      :: [Text],
      girNSName          :: Text,
      girNSVersion       :: Text,
      girAPIs            :: [(Name, API)]
    } deriving Show

data GIRNamespace = GIRNamespace {
      nsName      :: Text,
      nsVersion   :: Text,
      nsAPIs      :: [(Name, API)]
    } deriving (Show)

data GIRInfoParse = GIRInfoParse {
    girIPPackage    :: [Maybe Text],
    girIPIncludes   :: [Maybe (Text, Text)],
    girIPNamespaces :: [Maybe GIRNamespace]
} deriving (Show)

data Flags = Flags Enumeration
    deriving Show

{-
toFlags :: EnumInfo -> Flags
toFlags ei = Flags $ toEnumeration ei
-}

parseFlags :: ParseContext -> Element -> Maybe (Name, API)
parseFlags _ _ = Nothing

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

parseFunction :: ParseContext -> Element -> Maybe (Name, API)
parseFunction _ _ = Nothing

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

parseStruct :: ParseContext -> Element -> Maybe (Name, API)
parseStruct _ _ = Nothing

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

parseUnion :: ParseContext -> Element -> Maybe (Name, API)
parseUnion _ _ = Nothing

-- XXX
data Callback = Callback Callable
    deriving Show

{-
toCallback = Callback . toCallable
-}

parseCallback :: ParseContext -> Element -> Maybe (Name, API)
parseCallback _ _ = Nothing

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

parseInterface :: ParseContext -> Element -> Maybe (Name, API)
parseInterface _ _ = Nothing

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

parseObject :: ParseContext -> Element -> Maybe (Name, API)
parseObject _ _ = Nothing

-- XXX: Work out what to do with boxed types.
data Boxed = Boxed
    deriving Show

{-
toBoxed :: BaseInfo -> Boxed
toBoxed _ = Boxed
-}

parseBoxed :: ParseContext -> Element -> Maybe (Name, API)
parseBoxed _ _ = Nothing

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

maybeAddAPI :: GIRNamespace -> (a -> API) -> Maybe (Name, a) -> GIRNamespace
maybeAddAPI ns _ Nothing = ns
maybeAddAPI ns@GIRNamespace{nsAPIs} wrap (Just (n, v)) =
    ns {nsAPIs = (n, wrap v) : nsAPIs}

parseNamespaceElement :: ParseContext -> GIRNamespace -> Element -> GIRNamespace
parseNamespaceElement ctx ns@GIRNamespace{..} element =
    case nameLocalName (elementName element) of
      "alias" -> ns     -- Processed separately
      "constant" -> maybeAddAPI ns APIConst (parseConstant ctx element)
      "enumeration" -> maybeAddAPI ns APIEnum (parseEnum ctx element)
      "bitfield" -> maybeAddAPI ns id (parseFlags ctx element)
      "function" -> maybeAddAPI ns id (parseFunction ctx element)
      "callback" -> maybeAddAPI ns id (parseCallback ctx element)
      "record" -> maybeAddAPI ns id (parseStruct ctx element)
      "union" -> maybeAddAPI ns id (parseUnion ctx element)
      "class" -> maybeAddAPI ns id (parseObject ctx element)
      "interface" -> maybeAddAPI ns id (parseInterface ctx element)
      "boxed" -> maybeAddAPI ns id (parseBoxed ctx element)
      n -> error . T.unpack $ "Unknown GIR element \"" <> n <> "\" when processing namespace \"" <> nsName <> "\", aborting."

parseNamespace :: Element -> M.Map Alias Type -> Maybe GIRNamespace
parseNamespace element aliases = do
  let attrs = elementAttributes element
  name <- M.lookup "name" attrs
  version <- M.lookup "version" attrs
  let ns = GIRNamespace {
             nsName         = name,
             nsVersion      = version,
             nsAPIs         = []
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

emptyGIRInfoParse :: GIRInfoParse
emptyGIRInfoParse = GIRInfoParse {
                      girIPPackage = [],
                      girIPIncludes = [],
                      girIPNamespaces = []
                    }

parseGIRDocument :: M.Map Alias Type -> Document -> GIRInfoParse
parseGIRDocument aliases doc = L.foldl' (parseRootElement aliases) emptyGIRInfoParse (subelements (documentRoot doc))

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

-- | Turn a GIRInfoParse into a proper GIRInfo, doing some sanity
-- checking along the way.
toGIRInfo :: GIRInfoParse -> Either Text GIRInfo
toGIRInfo info =
    case catMaybes (girIPNamespaces info) of
      [ns] -> Right $ GIRInfo {
                girPCPackages = catMaybes (girIPPackage info)
              , girNSName = nsName ns
              , girNSVersion = nsVersion ns
              , girAPIs = nsAPIs ns
              }
      [] -> Left "Found no valid namespaces."
      _ -> Left "Found multiple namespaces."

-- | Load and parse a GIR file, including its dependencies.
loadGIRInfo :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> IO (GIRInfo,     -- ^ parsed document
                   [GIRInfo])   -- ^ parsed deps
loadGIRInfo verbose name version =  do
  (doc, deps) <- loadGIRFile verbose name version
  let aliases = M.unions (map documentListAliases (doc : M.elems deps))
      parsedDoc = toGIRInfo (parseGIRDocument aliases doc)
      parsedDeps = map (toGIRInfo . parseGIRDocument aliases) (M.elems deps)
  case combineErrors parsedDoc parsedDeps of
    Left err -> error . T.unpack $ "Error when parsing \"" <> name <> "\": " <> err
    Right (docGIR, depsGIR) ->
        if girNSName docGIR == name
        then return (docGIR, depsGIR)
        else error . T.unpack $ "Got unexpected namespace \""
                 <> girNSName docGIR <> "\" when parsing \"" <> name <> "\"."
  where combineErrors :: Either Text GIRInfo -> [Either Text GIRInfo]
                      -> Either Text (GIRInfo, [GIRInfo])
        combineErrors parsedDoc parsedDeps = do
          doc <- parsedDoc
          deps <- sequence parsedDeps
          return (doc, deps)
