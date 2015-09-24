{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module GI.API
    ( API(..)
    , GIRInfo(..)
    , loadGIRInfo

    -- Reexported from GI.GIR.BasicTypes
    , Name(..)
    , Transfer(..)

    -- Reexported from GI.GIR.Arg
    , Direction(..)
    , Scope(..)

    -- Reexported from GI.GIR.Deprecation
    , deprecatedPragma
    , DeprecationInfo

    -- Reexported from GI.GIR.Property
    , PropertyFlag(..)

    -- Reexported from GI.GIR.Method
    , MethodType(..)

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
    , Method(..)
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

import GI.GIR.Alias (documentListAliases)
import GI.GIR.Arg (Arg(..), Direction(..), Scope(..))
import GI.GIR.BasicTypes (Alias, Name(..), Transfer(..))
import GI.GIR.Callable (Callable(..))
import GI.GIR.Callback (Callback(..), parseCallback)
import GI.GIR.Constant (Constant(..), parseConstant)
import GI.GIR.Deprecation (DeprecationInfo, deprecatedPragma)
import GI.GIR.Enum (Enumeration(..), parseEnum)
import GI.GIR.Field (Field(..))
import GI.GIR.Flags (Flags(..), parseFlags)
import GI.GIR.Function (Function(..), parseFunction)
import GI.GIR.Interface (Interface(..), parseInterface)
import GI.GIR.Method (Method(..), MethodType(..))
import GI.GIR.Object (Object(..), parseObject)
import GI.GIR.Parser (Parser, runParser)
import GI.GIR.Property (Property(..), PropertyFlag(..))
import GI.GIR.Repository (readGiRepository)
import GI.GIR.Signal (Signal(..))
import GI.GIR.Struct (Struct(..), parseStruct)
import GI.GIR.Union (Union(..), parseUnion)
import GI.GIR.XMLUtils (subelements, childElemsWithLocalName, lookupAttr)
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
    deriving Show

parseAPI :: Text -> M.Map Alias Type -> Element -> (a -> API)
         -> Parser (Name, a) -> (Name, API)
parseAPI ns aliases element wrapper parser =
    case runParser ns aliases element parser of
      Left err -> error $ "Parse error: " ++ T.unpack err
      Right (n, a) -> (n, wrapper a)

parseNSElement :: M.Map Alias Type -> GIRNamespace -> Element -> GIRNamespace
parseNSElement aliases ns@GIRNamespace{..} element
    | lookupAttr "introspectable" element == Just "0" = ns
    | otherwise =
        case nameLocalName (elementName element) of
          "alias" -> ns     -- Processed separately
          "constant" -> parse APIConst parseConstant
          "enumeration" -> parse APIEnum parseEnum
          "bitfield" -> parse APIFlags parseFlags
          "function" -> parse APIFunction parseFunction
          "callback" -> parse APICallback parseCallback
          "record" -> parse APIStruct parseStruct
          "union" -> parse APIUnion parseUnion
          "class" -> parse APIObject parseObject
          "interface" -> parse APIInterface parseInterface
          "boxed" -> ns -- Unsupported
          n -> error . T.unpack $ "Unknown GIR element \"" <> n <> "\" when processing namespace \"" <> nsName <> "\", aborting."
    where parse = \w p -> ns { nsAPIs = parseAPI nsName aliases element w p : nsAPIs }

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
  return $ L.foldl' (parseNSElement aliases) ns (subelements element)

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
                 -> [FilePath]                        -- extra path to search
                 -> IO (M.Map (Text, Text) Document)  -- New loaded set
loadDependencies verbose requested loaded extraPaths
        | S.null requested = return loaded
        | otherwise = do
  let (name, version) = S.elemAt 0 requested
  doc <- readGiRepository verbose name (Just version) extraPaths
  let newLoaded = M.insert (name, version) doc loaded
      newRequested = S.union requested (documentListIncludes doc)
      notYetLoaded = S.filter (/= (name, version)) newRequested
  loadDependencies verbose notYetLoaded newLoaded extraPaths

-- | Load a given GIR file and recursively its dependencies
loadGIRFile :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> [FilePath]       -- ^ extra paths to search
            -> IO (Document,                    -- ^ loaded document
                   M.Map (Text, Text) Document) -- ^ dependencies
loadGIRFile verbose name version extraPaths = do
  doc <- readGiRepository verbose name version extraPaths
  deps <- loadDependencies verbose (documentListIncludes doc) M.empty extraPaths
  return (doc, deps)

-- | Turn a GIRInfoParse into a proper GIRInfo, doing some sanity
-- checking along the way.
toGIRInfo :: GIRInfoParse -> Either Text GIRInfo
toGIRInfo info =
    case catMaybes (girIPNamespaces info) of
      [ns] -> Right GIRInfo {
                girPCPackages = catMaybes (girIPPackage info)
              , girNSName = nsName ns
              , girNSVersion = nsVersion ns
              , girAPIs = nsAPIs ns
              }
      [] -> Left "Found no valid namespace."
      _  -> Left "Found multiple namespaces."

-- | Load and parse a GIR file, including its dependencies.
loadGIRInfo :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> [FilePath]       -- ^ extra paths to search
            -> IO (GIRInfo,     -- ^ parsed document
                   [GIRInfo])   -- ^ parsed deps
loadGIRInfo verbose name version extraPaths =  do
  (doc, deps) <- loadGIRFile verbose name version extraPaths
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
