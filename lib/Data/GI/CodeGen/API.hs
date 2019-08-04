{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.GI.CodeGen.API
    ( API(..)
    , GIRInfo(..)
    , loadGIRInfo
    , loadRawGIRInfo

    , GIRRule(..)
    , GIRPath
    , GIRNodeSpec(..)
    , GIRNameTag(..)

    -- Reexported from Data.GI.GIR.BasicTypes
    , Name(..)
    , Transfer(..)

    -- Reexported from Data.GI.GIR.Allocation
    , AllocationInfo(..)
    , AllocationOp(..)
    , unknownAllocationInfo

    -- Reexported from Data.GI.GIR.Arg
    , Direction(..)
    , Scope(..)

    -- Reexported from Data.GI.GIR.Deprecation
    , DeprecationInfo

    -- Reexported from Data.GI.GIR.Enumeration
    , EnumerationMember(..)

    -- Reexported from Data.GI.GIR.Property
    , PropertyFlag(..)

    -- Reexported from Data.GI.GIR.Method
    , MethodType(..)

    -- Reexported from the corresponding Data.GI.GIR modules
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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad ((>=>), foldM, forM, forM_)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe, catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import Foreign.Ptr (Ptr)
import Foreign (peek)
import Foreign.C.Types (CUInt)

import Text.XML hiding (Name)
import qualified Text.XML as XML

import Text.Regex.TDFA ((=~))

import Data.GI.GIR.Alias (documentListAliases)
import Data.GI.GIR.Allocation (AllocationInfo(..), AllocationOp(..), unknownAllocationInfo)
import Data.GI.GIR.Arg (Arg(..), Direction(..), Scope(..))
import Data.GI.GIR.BasicTypes (Alias, Name(..), Transfer(..))
import Data.GI.GIR.Callable (Callable(..))
import Data.GI.GIR.Callback (Callback(..), parseCallback)
import Data.GI.GIR.Constant (Constant(..), parseConstant)
import Data.GI.GIR.Deprecation (DeprecationInfo)
import Data.GI.GIR.Enum (Enumeration(..), EnumerationMember(..), parseEnum)
import Data.GI.GIR.Field (Field(..))
import Data.GI.GIR.Flags (Flags(..), parseFlags)
import Data.GI.GIR.Function (Function(..), parseFunction)
import Data.GI.GIR.Interface (Interface(..), parseInterface)
import Data.GI.GIR.Method (Method(..), MethodType(..))
import Data.GI.GIR.Object (Object(..), parseObject)
import Data.GI.GIR.Parser (Parser, runParser)
import Data.GI.GIR.Property (Property(..), PropertyFlag(..))
import Data.GI.GIR.Repository (readGiRepository)
import Data.GI.GIR.Signal (Signal(..))
import Data.GI.GIR.Struct (Struct(..), parseStruct)
import Data.GI.GIR.Union (Union(..), parseUnion)
import Data.GI.GIR.XMLUtils (subelements, childElemsWithLocalName, lookupAttr,
                        lookupAttrWithNamespace, GIRXMLNamespace(..),
                        xmlLocalName)

import Data.GI.Base.BasicConversions (unpackStorableArrayWithLength)
import Data.GI.Base.BasicTypes (GType(..), CGType, gtypeName)
import Data.GI.Base.Utils (allocMem, freeMem)
import Data.GI.CodeGen.LibGIRepository (girRequire, FieldInfo(..),
                                        girStructFieldInfo, girUnionFieldInfo,
                                        girLoadGType)
import Data.GI.CodeGen.GType (gtypeIsBoxed)
import Data.GI.CodeGen.Type (Type)

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

-- | Path to a node in the GIR file, starting from the document root
-- of the GIR file. This is a very simplified version of something
-- like XPath.
type GIRPath = [GIRNodeSpec]

-- | Node selector for a path in the GIR file.
data GIRNodeSpec = GIRNamed GIRNameTag  -- ^ Node with the given "name" attr.
                 | GIRType Text         -- ^ Node of the given type.
                 | GIRTypedName Text GIRNameTag -- ^ Combination of the above.
                   deriving (Show)

-- | A name tag, which is either a name or a regular expression.
data GIRNameTag = GIRPlainName Text
                | GIRRegex Text
                  deriving (Show)

-- | A rule for modifying the GIR file.
data GIRRule = GIRSetAttr (GIRPath, XML.Name) Text -- ^ (Path to element,
                                                   -- attrName), newValue.
             | GIRAddNode GIRPath XML.Name -- ^ Add a child node at
                                           -- the given selector.
             | GIRDeleteNode GIRPath -- ^ Delete any nodes matching
                                     -- the given selector.
             deriving (Show)

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
loadDependencies :: Bool                              -- ^ Verbose
                 -> S.Set (Text, Text)                -- ^ Requested
                 -> M.Map (Text, Text) Document       -- ^ Loaded so far
                 -> [FilePath]                        -- ^ extra path to search
                 -> [GIRRule]                         -- ^ fixups
                 -> IO (M.Map (Text, Text) Document)  -- ^ New loaded set
loadDependencies verbose requested loaded extraPaths rules
        | S.null requested = return loaded
        | otherwise = do
  let (name, version) = S.elemAt 0 requested
  doc <- fixupGIRDocument rules <$>
         readGiRepository verbose name (Just version) extraPaths
  let newLoaded = M.insert (name, version) doc loaded
      loadedSet = S.fromList (M.keys newLoaded)
      newRequested = S.union requested (documentListIncludes doc)
      notYetLoaded = S.difference newRequested loadedSet
  loadDependencies verbose notYetLoaded newLoaded extraPaths rules

-- | Load a given GIR file and recursively its dependencies
loadGIRFile :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> [FilePath]       -- ^ extra paths to search
            -> [GIRRule]        -- ^ fixups
            -> IO (Document,
                   M.Map (Text, Text) Document) -- ^ (loaded doc, dependencies)
loadGIRFile verbose name version extraPaths rules = do
  doc <- fixupGIRDocument rules <$>
         readGiRepository verbose name version extraPaths
  deps <- loadDependencies verbose (documentListIncludes doc) M.empty
          extraPaths rules
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
            -> [GIRRule]        -- ^ fixups
            -> IO (GIRInfo,
                   [GIRInfo])   -- ^ (parsed doc, parsed deps)
loadGIRInfo verbose name version extraPaths rules =  do
  (doc, deps) <- loadGIRFile verbose name version extraPaths rules
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
                 gtype <- girLoadGType ns ti
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
                 gtype <- girLoadGType ns ti
                 return (gtypeIsBoxed gtype)
  return (s {structIsBoxed = isBoxed})

-- | Fix the size and alignment of fields. This is much easier to do
-- by using libgirepository than reading the GIR file directly.
fixupStructSizeAndOffsets :: Name -> Struct -> IO Struct
fixupStructSizeAndOffsets (Name ns n) s = do
  (size, infoMap) <- girStructFieldInfo ns n
  return (s { structSize = size
            , structFields = map (fixupField infoMap) (structFields s)})

-- | Same thing for unions.
fixupUnion :: M.Map Text Name -> (Name, API) -> IO (Name, API)
fixupUnion _ (n, APIUnion u) = do
  fixed <- (fixupUnionSizeAndOffsets n) u
  return (n, APIUnion fixed)
fixupUnion _ api = return api

-- | Like 'fixupStructSizeAndOffset' above.
fixupUnionSizeAndOffsets :: Name -> Union -> IO Union
fixupUnionSizeAndOffsets (Name ns n) u = do
  (size, infoMap) <- girUnionFieldInfo ns n
  return (u { unionSize = size
            , unionFields = map (fixupField infoMap) (unionFields u)})

-- | Fixup the offsets of fields using the given offset map.
fixupField :: M.Map Text FieldInfo -> Field -> Field
fixupField offsetMap f =
    f {fieldOffset = case M.lookup (fieldName f) offsetMap of
                       Nothing -> error $ "Could not find field "
                                  ++ show (fieldName f)
                       Just o -> fieldInfoOffset o }

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

-- | Given a XML document containing GIR data, apply the given overrides.
fixupGIRDocument :: [GIRRule] -> XML.Document -> XML.Document
fixupGIRDocument rules doc =
    doc {XML.documentRoot = fixupGIR rules (XML.documentRoot doc)}

-- | Looks for the given path in the given subelements of the given
-- element. If the path is empty apply the corresponding rule,
-- otherwise return the element ummodified.
fixupGIR :: [GIRRule] -> XML.Element -> XML.Element
fixupGIR rules elem =
    elem {XML.elementNodes =
          mapMaybe (\e -> foldM applyGIRRule e rules) (XML.elementNodes elem)}
    where applyGIRRule :: XML.Node -> GIRRule -> Maybe XML.Node
          applyGIRRule n (GIRSetAttr (path, attr) newVal) =
            Just $ girSetAttr (path, attr) newVal n
          applyGIRRule n (GIRAddNode path new) =
            Just $ girAddNode path new n
          applyGIRRule n (GIRDeleteNode path) =
            girDeleteNodes path n

-- | Set an attribute for the child element specified by the given
-- path.
girSetAttr :: (GIRPath, XML.Name) -> Text -> XML.Node -> XML.Node
girSetAttr (spec:rest, attr) newVal n@(XML.NodeElement elem) =
    if specMatch spec n
    then case rest of
           -- Matched the full path, apply
           [] -> XML.NodeElement (elem {XML.elementAttributes =
                                        M.insert attr newVal
                                        (XML.elementAttributes elem)})
           -- Still some selectors to apply
           _ -> XML.NodeElement (elem {XML.elementNodes =
                                       map (girSetAttr (rest, attr) newVal)
                                       (XML.elementNodes elem)})
    else n
girSetAttr _ _ n = n

-- | Add the given subnode to any nodes matching the given path
girAddNode :: GIRPath -> XML.Name -> XML.Node -> XML.Node
girAddNode (spec:rest) newNode n@(XML.NodeElement element) =
  if specMatch spec n
  then case rest of
    -- Matched the full path, add the new child node.
    [] -> let newElement = XML.Element { elementName = newNode
                                       , elementAttributes = M.empty
                                       , elementNodes = [] }
              -- We only insert if not present, see #171. For
              -- convenience when writing the override files, we
              -- ignore the namespace when comparing.
              nodeElementName (XML.NodeElement e) =
                (Just . nameLocalName . elementName) e
              nodeElementName _ = Nothing
              nodeNames = mapMaybe nodeElementName (XML.elementNodes element)
          in if nameLocalName newNode `elem` nodeNames
             then n
             else XML.NodeElement (element {XML.elementNodes =
                                    XML.elementNodes element <>
                                     [XML.NodeElement newElement]})
    -- Still some selectors to apply.
    _ -> XML.NodeElement (element {XML.elementNodes =
                                map (girAddNode rest newNode)
                                 (XML.elementNodes element)})
  else n
girAddNode _ _ n = n

-- | Delete any nodes matching the given path.
girDeleteNodes :: GIRPath -> XML.Node -> Maybe XML.Node
girDeleteNodes (spec:rest) n@(XML.NodeElement elem) =
  if specMatch spec n
  then case rest of
         -- Matched the full path, discard the node
         [] -> Nothing
         -- More selectors to apply
         _ -> Just $ XML.NodeElement (elem {XML.elementNodes =
                                            mapMaybe (girDeleteNodes rest)
                                            (XML.elementNodes elem)})
  else Just n
girDeleteNodes _ n = Just n

-- | Lookup the given attribute and if present see if it matches the
-- given regex.
lookupAndMatch :: GIRNameTag -> M.Map XML.Name Text -> XML.Name -> Bool
lookupAndMatch tag attrs attr =
    case M.lookup attr attrs of
      Just s -> case tag of
                  GIRPlainName pn -> s == pn
                  GIRRegex r -> T.unpack s =~ T.unpack r
      Nothing -> False

-- | See if a given node specification applies to the given node.
specMatch :: GIRNodeSpec -> XML.Node -> Bool
specMatch (GIRType t) (XML.NodeElement elem) =
    XML.nameLocalName (XML.elementName elem) == t
specMatch (GIRNamed name) (XML.NodeElement elem) =
    lookupAndMatch name  (XML.elementAttributes elem) (xmlLocalName "name")
specMatch (GIRTypedName t name) (XML.NodeElement elem) =
    XML.nameLocalName (XML.elementName elem) == t &&
    lookupAndMatch name (XML.elementAttributes elem) (xmlLocalName "name")
specMatch _ _ = False
