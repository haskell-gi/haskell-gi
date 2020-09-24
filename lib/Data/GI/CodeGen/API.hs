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

import Control.Monad ((>=>), foldM, forM, when)
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
import Data.GI.CodeGen.LibGIRepository (girRequire, Typelib, FieldInfo(..),
                                        girStructFieldInfo, girUnionFieldInfo,
                                        girLoadGType, girIsSymbolResolvable)
import Data.GI.CodeGen.GType (gtypeIsBoxed)
import Data.GI.CodeGen.Type (Type)
import Data.GI.CodeGen.Util (printWarning, terror, tshow)

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
             | GIRDeleteAttr GIRPath XML.Name
             -- ^ Delete the given attribute
             | GIRAddNode GIRPath XML.Name -- ^ Add a child node at
                                           -- the given selector.
             | GIRDeleteNode GIRPath -- ^ Delete any nodes matching
                                     -- the given selector.
             deriving (Show)

-- | An element in the exposed API
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
          "docsection" -> ns -- Ignored for now, see https://github.com/haskell-gi/haskell-gi/issues/318
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
  doc <- overrideGIRDocument rules <$>
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
            -> [GIRRule]        -- ^ overrides
            -> IO (Document,
                   M.Map (Text, Text) Document) -- ^ (loaded doc, dependencies)
loadGIRFile verbose name version extraPaths rules = do
  doc <- overrideGIRDocument rules <$>
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

-- | Fixup parsed GIRInfos: some of the required information is not
-- found in the GIR files themselves, or does not accurately reflect
-- the content in the dynamic library itself, but this can be
-- corrected by checking the typelib.
fixupGIRInfos :: Bool -> M.Map Text Typelib -> GIRInfo -> [GIRInfo]
              -> IO (GIRInfo, [GIRInfo])
fixupGIRInfos verbose typelibMap doc deps =
  (fixup (fixupInterface typelibMap ctypes) >=>
    fixup (fixupStruct typelibMap) >=>
    fixup fixupUnion >=>
    fixup (fixupMissingSymbols verbose typelibMap)
  ) (doc, deps)
  where fixup :: ((Name, API) -> IO (Name, API))
                 -> (GIRInfo, [GIRInfo]) -> IO (GIRInfo, [GIRInfo])
        fixup fixer (doc, deps) = do
          fixedDoc <- fixAPIs fixer doc
          fixedDeps <- mapM (fixAPIs fixer) deps
          return (fixedDoc, fixedDeps)

        fixAPIs :: ((Name, API) -> IO (Name, API))
                -> GIRInfo -> IO GIRInfo
        fixAPIs fixer info = do
          fixedAPIs <- mapM fixer (girAPIs info)
          return $ info {girAPIs = fixedAPIs}

        ctypes :: M.Map Text Name
        ctypes = M.unions (map girCTypes (doc:deps))

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
fixupInterface :: M.Map Text Typelib -> M.Map Text Name -> (Name, API)
               -> IO (Name, API)
fixupInterface typelibMap csymbolMap (n@(Name ns _), APIInterface iface) = do
  prereqs <- case ifTypeInit iface of
               Nothing -> return []
               Just ti -> do
                 gtype <- case M.lookup ns typelibMap of
                            Just typelib -> girLoadGType typelib ti
                            Nothing -> error $ "fi: Typelib for " ++ show ns ++ " not loaded."
                 prereqGTypes <- gtypeInterfaceListPrereqs gtype
                 forM prereqGTypes $ \p -> do
                   case M.lookup p csymbolMap of
                     Just pn -> return pn
                     Nothing -> error $ "Could not find prerequisite type " ++ show p ++ " for interface " ++ show n
  return (n, APIInterface (iface {ifPrerequisites = prereqs}))
fixupInterface _ _ (n, api) = return (n, api)

-- | There is not enough info in the GIR files to determine whether a
-- struct is boxed. We find out by instantiating the 'GType'
-- corresponding to the struct (if known) and checking whether it
-- descends from the boxed GType. Similarly, the size of the struct
-- and offset of the fields is hard to compute from the GIR data, we
-- simply reuse the machinery in libgirepository.
fixupStruct :: M.Map Text Typelib -> (Name, API)
            -> IO (Name, API)
fixupStruct typelibMap (n, APIStruct s) = do
  fixed <- (fixupStructIsBoxed typelibMap n >=> fixupStructSizeAndOffsets n) s
  return (n, APIStruct fixed)
fixupStruct _ api = return api

-- | Find out whether the struct is boxed.
fixupStructIsBoxed :: M.Map Text Typelib -> Name -> Struct -> IO Struct
-- The type for "GVariant" is marked as "intern", we wrap
-- this one natively.
fixupStructIsBoxed _ (Name "GLib" "Variant") s =
    return (s {structIsBoxed = False})
fixupStructIsBoxed typelibMap (Name ns _) s = do
  isBoxed <- case structTypeInit s of
               Nothing -> return False
               Just ti -> do
                 gtype <- case M.lookup ns typelibMap of
                   Just typelib -> girLoadGType typelib ti
                   Nothing -> error $ "fsib: Typelib for " ++ show ns ++ " not loaded."

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
fixupUnion :: (Name, API) -> IO (Name, API)
fixupUnion (n, APIUnion u) = do
  fixed <- (fixupUnionSizeAndOffsets n) u
  return (n, APIUnion fixed)
fixupUnion api = return api

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

-- | Some of the symbols listed in the introspection data are not
-- present in the dynamic library itself. Generating bindings for
-- these will sometimes lead to linker errors, so here we check that
-- every symbol listed in the bindings is actually present.
fixupMissingSymbols :: Bool -> M.Map Text Typelib -> (Name, API)
                    -> IO (Name, API)
fixupMissingSymbols verbose typelibMap (n, APIStruct s) = do
  fixedMethods <- fixupMethodMissingSymbols (resolveTypelib n typelibMap)
                                            (structMethods s) verbose
  return (n, APIStruct (s {structMethods = fixedMethods}))
fixupMissingSymbols verbose typelibMap (n, APIUnion u) = do
  fixedMethods <- fixupMethodMissingSymbols (resolveTypelib n typelibMap)
                                            (unionMethods u) verbose
  return (n, APIUnion (u {unionMethods = fixedMethods}))
fixupMissingSymbols verbose typelibMap (n, APIObject o) = do
  fixedMethods <- fixupMethodMissingSymbols (resolveTypelib n typelibMap)
                                            (objMethods o) verbose
  return (n, APIObject (o {objMethods = fixedMethods}))
fixupMissingSymbols verbose typelibMap (n, APIInterface i) = do
  fixedMethods <- fixupMethodMissingSymbols (resolveTypelib n typelibMap)
                                            (ifMethods i) verbose
  return (n, APIInterface (i {ifMethods = fixedMethods}))
fixupMissingSymbols verbose typelibMap (n, APIFunction f) =
  fixupFunctionSymbols typelibMap (n, f) verbose
fixupMissingSymbols _ _ (n, api) = return (n, api)

-- | Resolve the typelib owning the given name, erroring out if the
-- typelib is not known.
resolveTypelib :: Name -> M.Map Text Typelib -> Typelib
resolveTypelib n typelibMap = case M.lookup (namespace n) typelibMap of
  Nothing -> terror $ "Could not find typelib for “" <> namespace n <> "”."
  Just typelib -> typelib

-- | Mark whether the methods can be resolved in the given typelib.
fixupMethodMissingSymbols :: Typelib -> [Method] -> Bool -> IO [Method]
fixupMethodMissingSymbols typelib methods verbose = mapM check methods
  where check :: Method -> IO Method
        check method@Method{methodCallable = callable} = do
          resolvable <- girIsSymbolResolvable typelib (methodSymbol method)
          when (verbose && not resolvable) $
            printWarning $ "Could not resolve the callable “"
                           <> methodSymbol method
                           <> "” in the “" <> tshow typelib
                           <> "” typelib, ignoring."
          let callable' = callable{callableResolvable = Just resolvable}
          return $ method{methodCallable = callable'}

-- | Check that the symbol the function refers to is actually present
-- in the dynamic library.
fixupFunctionSymbols :: M.Map Text Typelib -> (Name, Function) -> Bool
                            -> IO (Name, API)
fixupFunctionSymbols typelibMap (n, f) verbose = do
  let typelib = resolveTypelib n typelibMap
  resolvable <- girIsSymbolResolvable typelib (fnSymbol f)
  when (verbose && not resolvable) $
    printWarning $ "Could not resolve the function “" <> fnSymbol f
                    <> "” in the “" <> tshow typelib <> "” typelib, ignoring."
  let callable' = (fnCallable f){callableResolvable = Just resolvable}
  return (n, APIFunction (f {fnCallable = callable'}))

-- | Load and parse a GIR file, including its dependencies.
loadGIRInfo :: Bool             -- ^ verbose
            -> Text             -- ^ name
            -> Maybe Text       -- ^ version
            -> [FilePath]       -- ^ extra paths to search
            -> [GIRRule]        -- ^ fixups
            -> IO (GIRInfo, [GIRInfo])
            -- ^ (parsed doc,  parsed deps)
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
        typelibMap <- M.fromList <$> (forM (docGIR : depsGIR) $ \info -> do
             typelib <- girRequire (girNSName info) (girNSVersion info)
             return (girNSName info, typelib))
        (fixedDoc, fixedDeps) <- fixupGIRInfos verbose typelibMap docGIR depsGIR
        return (fixedDoc, fixedDeps)
      else error . T.unpack $ "Got unexpected namespace \""
               <> girNSName docGIR <> "\" when parsing \"" <> name <> "\"."
  where combineErrors :: Either Text GIRInfo -> [Either Text GIRInfo]
                      -> Either Text (GIRInfo, [GIRInfo])
        combineErrors parsedDoc parsedDeps = do
          doc <- parsedDoc
          deps <- sequence parsedDeps
          return (doc, deps)

-- | Given a XML document containing GIR data, apply the given overrides.
overrideGIRDocument :: [GIRRule] -> XML.Document -> XML.Document
overrideGIRDocument rules doc =
    doc {XML.documentRoot = overrideGIR rules (XML.documentRoot doc)}

-- | Looks for the given path in the given subelements of the given
-- element. If the path is empty apply the corresponding rule,
-- otherwise return the element ummodified.
overrideGIR :: [GIRRule] -> XML.Element -> XML.Element
overrideGIR rules elem =
    elem {XML.elementNodes =
          mapMaybe (\e -> foldM applyGIRRule e rules) (XML.elementNodes elem)}
    where applyGIRRule :: XML.Node -> GIRRule -> Maybe XML.Node
          applyGIRRule n (GIRSetAttr (path, attr) newVal) =
            Just $ girSetAttr (path, attr) newVal n
          applyGIRRule n (GIRDeleteAttr path attr) =
            Just $ girDeleteAttr path attr n
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

-- | Delete an attribute for the child element specified by the given
-- path, if the attribute exists.
girDeleteAttr :: GIRPath -> XML.Name -> XML.Node -> XML.Node
girDeleteAttr (spec:rest) attr n@(XML.NodeElement elem) =
    if specMatch spec n
    then case rest of
           -- Matched the full path, apply
           [] -> XML.NodeElement (elem {XML.elementAttributes =
                                        M.delete attr
                                        (XML.elementAttributes elem)})
           -- Still some selectors to apply
           _ -> XML.NodeElement (elem {XML.elementNodes =
                                       map (girDeleteAttr rest attr)
                                       (XML.elementNodes elem)})
    else n
girDeleteAttr _ _ n = n

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
