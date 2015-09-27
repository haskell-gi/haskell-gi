{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module GI.API
    ( API(..)
    , GIRInfo(..)
    , loadGIRInfo
    , loadRawGIRInfo

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

import Control.Monad ((>=>), forM, forM_)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import Foreign.Ptr (Ptr)
import Foreign (peek)
import Foreign.C.Types (CUInt)

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
import GI.GIR.XMLUtils (subelements, childElemsWithLocalName, lookupAttr,
                        lookupAttrWithNamespace, GIRXMLNamespace(..))

import GI.Utils.BasicConversions (unpackStorableArrayWithLength)
import GI.Utils.BasicTypes (GType(..), CGType, gtypeName)
import GI.Utils.Utils (allocMem, freeMem)
import GI.LibGIRepository (girRequire, girStructSizeAndOffsets,
                           girUnionSizeAndOffsets, girLoadGType)
import GI.GType (gtypeIsBoxed)
import GI.Type (Type)

data GIRInfo = GIRInfo {
      girPCPackages      :: [Text],
      girNSName          :: Text,
      girNSVersion       :: Text,
      girAPIs            :: [(Name, API)],
      girCTypes          :: M.Map Text Name
    } deriving Show

data GIRNamespace = GIRNamespace {
      nsName      :: Text,
      nsVersion   :: Text,
      nsAPIs      :: [(Name, API)],
      nsCTypes    :: [(Text, Name)]
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
    where parse :: (a -> API) -> Parser (Name, a) -> GIRNamespace
          parse wrapper parser =
              let (n, api) = parseAPI nsName aliases element wrapper parser
                  maybeCType = lookupAttrWithNamespace CGIRNS "type" element
              in ns { nsAPIs = (n, api) : nsAPIs,
                      nsCTypes = case maybeCType of
                                   Just ctype -> (ctype, n) : nsCTypes
                                   Nothing -> nsCTypes
                    }

parseNamespace :: Element -> M.Map Alias Type -> Maybe GIRNamespace
parseNamespace element aliases = do
  let attrs = elementAttributes element
  name <- M.lookup "name" attrs
  version <- M.lookup "version" attrs
  let ns = GIRNamespace {
             nsName         = name,
             nsVersion      = version,
             nsAPIs         = [],
             nsCTypes       = []
           }
  return (L.foldl' (parseNSElement aliases) ns (subelements element))

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
                girPCPackages = (reverse . catMaybes . girIPPackage) info
              , girNSName = nsName ns
              , girNSVersion = nsVersion ns
              , girAPIs = reverse (nsAPIs ns)
              , girCTypes = M.fromList (nsCTypes ns)
              }
      [] -> Left "Found no valid namespace."
      _  -> Left "Found multiple namespaces."

-- | Bare minimum loading and parsing of a single repository, without
-- loading or parsing its dependencies, resolving aliases, or fixing
-- up structs or interfaces.
loadRawGIRInfo :: Bool          -- ^ verbose
               -> Text          -- ^ name
               -> Maybe Text    -- ^ version
               -> [FilePath]    -- ^ extra paths to search
               -> IO GIRInfo    -- ^ bare parsed document
loadRawGIRInfo verbose name version extraPaths = do
  doc <- readGiRepository verbose name version extraPaths
  case toGIRInfo (parseGIRDocument M.empty doc) of
    Left err -> error . T.unpack $ "Error when raw parsing \"" <> name <> "\": " <> err
    Right docGIR -> return docGIR

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
    Right (docGIR, depsGIR) -> do
      if girNSName docGIR == name
      then do
        forM_ (docGIR : depsGIR) $ \info ->
            girRequire (girNSName info) (girNSVersion info)
        (fixedDoc, fixedDeps) <- fixupGIRInfos docGIR depsGIR
        return (fixedDoc, fixedDeps)
      else error . T.unpack $ "Got unexpected namespace \""
               <> girNSName docGIR <> "\" when parsing \"" <> name <> "\"."
  where combineErrors :: Either Text GIRInfo -> [Either Text GIRInfo]
                      -> Either Text (GIRInfo, [GIRInfo])
        combineErrors parsedDoc parsedDeps = do
          doc <- parsedDoc
          deps <- sequence parsedDeps
          return (doc, deps)

foreign import ccall "g_type_interface_prerequisites" g_type_interface_prerequisites :: CGType -> Ptr CUInt -> IO (Ptr CGType)

-- | List the prerequisites for a 'GType' corresponding to an interface.
gtypeInterfaceListPrereqs :: GType -> IO [Text]
gtypeInterfaceListPrereqs (GType cgtype) = do
  nprereqsPtr <- allocMem :: IO (Ptr CUInt)
  ps <- g_type_interface_prerequisites cgtype nprereqsPtr
  nprereqs <- peek nprereqsPtr
  psCGTypes <- unpackStorableArrayWithLength nprereqs ps
  freeMem ps
  freeMem nprereqsPtr
  mapM (fmap T.pack . gtypeName . GType) psCGTypes

-- | The list of prerequisites in GIR files is not always
-- accurate. Instead of relying on this, we instantiate the 'GType'
-- associated to the interface, and listing the interfaces from there.
fixupInterface :: M.Map Text Name -> (Name, API) -> IO (Name, API)
fixupInterface csymbolMap (n@(Name ns _), APIInterface iface) = do
  prereqs <- case ifTypeInit iface of
               Nothing -> return []
               Just ti -> do
                 gtype <- girLoadGType (T.pack ns) ti
                 prereqGTypes <- gtypeInterfaceListPrereqs gtype
                 forM prereqGTypes $ \p -> do
                   case M.lookup p csymbolMap of
                     Just pn -> return pn
                     Nothing -> error $ "Could not find prerequisite type " ++ show p ++ " for interface " ++ show n
  return (n, APIInterface (iface {ifPrerequisites = prereqs}))
fixupInterface _ (n, api) = return (n, api)

-- | There is not enough info in the GIR files to determine whether a
-- struct is boxed. We find out by instantiating the 'GType'
-- corresponding to the struct (if known) and checking whether it
-- descends from the boxed GType. Similarly, the size of the struct
-- and offset of the fields is hard to compute from the GIR data, we
-- simply reuse the machinery in libgirepository.
fixupStruct :: M.Map Text Name -> (Name, API) -> IO (Name, API)
fixupStruct _ (n, APIStruct s) = do
  fixed <- (fixupStructIsBoxed n >=> fixupStructSizeAndOffsets n) s
  return (n, APIStruct fixed)
fixupStruct _ api = return api

-- | Find out whether the struct is boxed.
fixupStructIsBoxed :: Name -> Struct -> IO Struct
-- The type for "GVariant" is marked as "intern", we wrap
-- this one natively.
fixupStructIsBoxed (Name "GLib" "Variant") s =
    return (s {structIsBoxed = False})
fixupStructIsBoxed (Name ns _) s = do
  isBoxed <- case structTypeInit s of
               Nothing -> return False
               Just ti -> do
                 gtype <- girLoadGType (T.pack ns) ti
                 return (gtypeIsBoxed gtype)
  return (s {structIsBoxed = isBoxed})

-- | Fix the size and alignment of fields. This is much easier to do
-- by using libgirepository than reading the GIR file directly.
fixupStructSizeAndOffsets :: Name -> Struct -> IO Struct
fixupStructSizeAndOffsets (Name ns n) s = do
  (size, offsetMap) <- girStructSizeAndOffsets (T.pack ns) (T.pack n)
  return (s { structSize = size
            , structFields = map (fixupField offsetMap) (structFields s)})

-- | Same thing for unions.
fixupUnion :: M.Map Text Name -> (Name, API) -> IO (Name, API)
fixupUnion _ (n, APIUnion u) = do
  fixed <- (fixupUnionSizeAndOffsets n) u
  return (n, APIUnion fixed)
fixupUnion _ api = return api

-- | Like 'fixupStructSizeAndOffset' above.
fixupUnionSizeAndOffsets :: Name -> Union -> IO Union
fixupUnionSizeAndOffsets (Name ns n) u = do
  (size, offsetMap) <- girUnionSizeAndOffsets (T.pack ns) (T.pack n)
  return (u { unionSize = size
            , unionFields = map (fixupField offsetMap) (unionFields u)})

-- | Fixup the offsets of fields using the given offset map.
fixupField :: M.Map Text Int -> Field -> Field
fixupField offsetMap f =
    f {fieldOffset = case M.lookup (fieldName f) offsetMap of
                       Nothing -> error $ "Could not find field "
                                  ++ show (fieldName f)
                       Just o -> o }

-- | Fixup parsed GIRInfos: some of the required information is not
-- found in the GIR files themselves, but can be obtained by
-- instantiating the required GTypes from the installed libraries.
fixupGIRInfos :: GIRInfo -> [GIRInfo] -> IO (GIRInfo, [GIRInfo])
fixupGIRInfos doc deps = (fixup fixupInterface >=>
                          fixup fixupStruct >=>
                          fixup fixupUnion) (doc, deps)
  where fixup :: (M.Map Text Name -> (Name, API) -> IO (Name, API))
                 -> (GIRInfo, [GIRInfo]) -> IO (GIRInfo, [GIRInfo])
        fixup fixer (doc, deps) = do
          fixedDoc <- fixAPIs fixer doc
          fixedDeps <- mapM (fixAPIs fixer) deps
          return (fixedDoc, fixedDeps)

        fixAPIs :: (M.Map Text Name -> (Name, API) -> IO (Name, API)) -> GIRInfo -> IO GIRInfo
        fixAPIs fixer info = do
          fixedAPIs <- mapM (fixer ctypes) (girAPIs info)
          return $ info {girAPIs = fixedAPIs}

        ctypes :: M.Map Text Name
        ctypes = M.unions (map girCTypes (doc:deps))
