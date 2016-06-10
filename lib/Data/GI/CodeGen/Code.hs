module Data.GI.CodeGen.Code
    ( Code(..)
    , ModuleInfo(..)
    , ModuleFlag(..)
    , BaseCodeGen
    , CodeGen
    , ExcCodeGen
    , CGError(..)
    , genCode
    , evalCodeGen

    , writeModuleTree
    , listModuleTree
    , codeToText
    , transitiveModuleDeps
    , minBaseVersion
    , BaseVersion(..)
    , showBaseVersion

    , ModuleName
    , registerNSDependency
    , qualified
    , getDeps
    , recurseWithAPIs

    , handleCGExc
    , describeCGError
    , notImplementedError
    , badIntroError
    , missingInfoError

    , indent
    , bline
    , line
    , blank
    , group
    , hsBoot
    , submodule
    , setLanguagePragmas
    , setGHCOptions
    , setModuleFlags
    , setModuleMinBase
    , addModuleDocumentation

    , exportToplevel
    , exportModule
    , exportDecl
    , exportMethod
    , exportProperty
    , exportSignal

    , findAPI
    , getAPI
    , findAPIByName
    , getAPIs

    , config
    , currentModule
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(..))
#endif
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Sequence (Seq, ViewL ((:<)), (><), (|>), (<|))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, takeDirectory)

import Data.GI.CodeGen.API (API, Name(..))
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.Type (Type(..))
import Data.GI.CodeGen.Util (tshow, terror, padTo)
import Data.GI.CodeGen.ProjectInfo (authors, license, maintainers)
import Data.GI.GIR.Documentation (Documentation(..))

data Code
    = NoCode              -- ^ No code
    | Line Text           -- ^ A single line, indented to current indentation
    | Indent Code         -- ^ Indented region
    | Sequence (Seq Code) -- ^ The basic sequence of code
    | Group Code          -- ^ A grouped set of lines
    deriving (Eq, Show)

instance Monoid Code where
    mempty = NoCode

    NoCode `mappend` NoCode = NoCode
    x `mappend` NoCode = x
    NoCode `mappend` x = x
    (Sequence a) `mappend` (Sequence b) = Sequence (a >< b)
    (Sequence a) `mappend` b = Sequence (a |> b)
    a `mappend` (Sequence b) = Sequence (a <| b)
    a `mappend` b = Sequence (a <| b <| S.empty)

type Deps = Set.Set Text
type ModuleName = [Text]

-- | Subsection of the haddock documentation where the export should
-- be located.
type HaddockSection = Text

-- | Symbol to export.
type SymbolName = Text

-- | Possible exports for a given module. Every export type
-- constructor has two parameters: the section of the haddocks where
-- it should appear, and the symbol name to export in the export list
-- of the module.
data Export = Export {
      exportType    :: ExportType       -- ^ Which kind of export.
    , exportSymbol  :: SymbolName       -- ^ Actual symbol to export.
    } deriving (Show, Eq, Ord)

-- | Possible types of exports.
data ExportType = ExportTypeDecl -- ^ A type declaration.
                | ExportToplevel -- ^ An export in no specific section.
                | ExportMethod HaddockSection -- ^ A method for a struct/union, etc.
                | ExportProperty HaddockSection -- ^ A property for an object/interface.
                | ExportSignal HaddockSection  -- ^ A signal for an object/interface.
                | ExportModule   -- ^ Reexport of a whole module.
                  deriving (Show, Eq, Ord)

-- | Information on a generated module.
data ModuleInfo = ModuleInfo {
      moduleName :: ModuleName -- ^ Full module name: ["GI", "Gtk", "Label"].
    , moduleCode :: Code       -- ^ Generated code for the module.
    , bootCode   :: Code       -- ^ Interface going into the .hs-boot file.
    , submodules :: M.Map Text ModuleInfo -- ^ Indexed by the relative
                                          -- module name.
    , moduleDeps :: Deps -- ^ Set of dependencies for this module.
    , moduleExports :: Seq Export -- ^ Exports for the module.
    , qualifiedImports :: Set.Set ModuleName -- ^ Qualified (source) imports
    , modulePragmas :: Set.Set Text -- ^ Set of language pragmas for the module.
    , moduleGHCOpts :: Set.Set Text -- ^ GHC options for compiling the module.
    , moduleFlags   :: Set.Set ModuleFlag -- ^ Flags for the module.
    , moduleDoc     :: Maybe Text -- ^ Documentation for the module.
    , moduleMinBase :: BaseVersion -- ^ Minimal version of base the
                                   -- module will work on.
    }

-- | Flags for module code generation.
data ModuleFlag = ImplicitPrelude  -- ^ Use the standard prelude,
                                   -- instead of the haskell-gi-base short one.
                  deriving (Show, Eq, Ord)

-- | Minimal version of base supported by a given module.
data BaseVersion = Base47  -- ^ 4.7.0
                 | Base48  -- ^ 4.8.0
                   deriving (Show, Eq, Ord)

-- | A `Text` representation of the given base version bound.
showBaseVersion :: BaseVersion -> Text
showBaseVersion Base47 = "4.7"
showBaseVersion Base48 = "4.8"

-- | Generate the empty module.
emptyModule :: ModuleName -> ModuleInfo
emptyModule m = ModuleInfo { moduleName = m
                           , moduleCode = NoCode
                           , bootCode = NoCode
                           , submodules = M.empty
                           , moduleDeps = Set.empty
                           , moduleExports = S.empty
                           , qualifiedImports = Set.empty
                           , modulePragmas = Set.empty
                           , moduleGHCOpts = Set.empty
                           , moduleFlags = Set.empty
                           , moduleDoc = Nothing
                           , moduleMinBase = Base47
                           }

-- | Information for the code generator.
data CodeGenConfig = CodeGenConfig {
      hConfig     :: Config        -- ^ Ambient config.
    , loadedAPIs :: M.Map Name API -- ^ APIs available to the generator.
    }

data CGError = CGErrorNotImplemented Text
             | CGErrorBadIntrospectionInfo Text
             | CGErrorMissingInfo Text
               deriving (Show)

type BaseCodeGen excType a =
    ReaderT CodeGenConfig (StateT ModuleInfo (ExceptT excType IO)) a

-- | The code generator monad, for generators that cannot throw
-- errors. The fact that they cannot throw errors is encoded in the
-- forall, which disallows any operation on the error, except
-- discarding it or passing it along without inspecting. This last
-- operation is useful in order to allow embedding `CodeGen`
-- computations inside `ExcCodeGen` computations, while disallowing
-- the opposite embedding without explicit error handling.
type CodeGen a = forall e. BaseCodeGen e a

-- | Code generators that can throw errors.
type ExcCodeGen a = BaseCodeGen CGError a

-- | Run a `CodeGen` with given `Config` and initial `ModuleInfo`,
-- returning either the resulting exception, or the result and final
-- state of the codegen.
runCodeGen :: BaseCodeGen e a -> CodeGenConfig -> ModuleInfo ->
              IO (Either e (a, ModuleInfo))
runCodeGen cg cfg state = runExceptT (runStateT (runReaderT cg cfg) state)

-- | This is useful when we plan run a subgenerator, and `mconcat` the
-- result to the original structure later.
cleanInfo :: ModuleInfo -> ModuleInfo
cleanInfo info = info { moduleCode = NoCode, submodules = M.empty,
                        bootCode = NoCode, moduleExports = S.empty,
                        qualifiedImports = Set.empty,
                        moduleDoc = Nothing, moduleMinBase = Base47 }

-- | Run the given code generator using the state and config of an
-- ambient CodeGen, but without adding the generated code to
-- `moduleCode`, instead returning it explicitly.
recurseCG :: BaseCodeGen e a -> BaseCodeGen e (a, Code)
recurseCG cg = do
  cfg <- ask
  oldInfo <- get
  -- Start the subgenerator with no code and no submodules.
  let info = cleanInfo oldInfo
  liftIO (runCodeGen cg cfg info) >>= \case
     Left e -> throwError e
     Right (r, new) -> put (mergeInfoState oldInfo new) >>
                       return (r, moduleCode new)

-- | Like `recurse`, giving explicitly the set of loaded APIs for the
-- subgenerator.
recurseWithAPIs :: M.Map Name API -> CodeGen () -> CodeGen ()
recurseWithAPIs apis cg = do
  cfg <- ask
  oldInfo <- get
  -- Start the subgenerator with no code and no submodules.
  let info = cleanInfo oldInfo
      cfg' = cfg {loadedAPIs = apis}
  liftIO (runCodeGen cg cfg' info) >>= \case
     Left e -> throwError e
     Right (_, new) -> put (mergeInfo oldInfo new)

-- | Merge everything but the generated code for the two given `ModuleInfo`.
mergeInfoState :: ModuleInfo -> ModuleInfo -> ModuleInfo
mergeInfoState oldState newState =
    let newDeps = Set.union (moduleDeps oldState) (moduleDeps newState)
        newSubmodules = M.unionWith mergeInfo (submodules oldState) (submodules newState)
        newExports = moduleExports oldState <> moduleExports newState
        newImports = qualifiedImports oldState <> qualifiedImports newState
        newPragmas = Set.union (modulePragmas oldState) (modulePragmas newState)
        newGHCOpts = Set.union (moduleGHCOpts oldState) (moduleGHCOpts newState)
        newFlags = Set.union (moduleFlags oldState) (moduleFlags newState)
        newBoot = bootCode oldState <> bootCode newState
        newDoc = moduleDoc oldState <> moduleDoc newState
        newMinBase = max (moduleMinBase oldState) (moduleMinBase newState)
    in oldState {moduleDeps = newDeps, submodules = newSubmodules,
                 moduleExports = newExports, qualifiedImports = newImports,
                 modulePragmas = newPragmas,
                 moduleGHCOpts = newGHCOpts, moduleFlags = newFlags,
                 bootCode = newBoot, moduleDoc = newDoc,
                 moduleMinBase = newMinBase }

-- | Merge the infos, including code too.
mergeInfo :: ModuleInfo -> ModuleInfo -> ModuleInfo
mergeInfo oldInfo newInfo =
    let info = mergeInfoState oldInfo newInfo
    in info { moduleCode = moduleCode oldInfo <> moduleCode newInfo }

-- | Add the given submodule to the list of submodules of the current
-- module.
addSubmodule :: Text -> ModuleInfo -> ModuleInfo -> ModuleInfo
addSubmodule modName submodule current = current { submodules = M.insertWith mergeInfo modName submodule (submodules current)}

-- | Run the given CodeGen in order to generate a single submodule of the
-- current module. Note that we do not generate the submodule if the
-- code generator generated no code and the module does not have
-- submodules.
submodule' :: Text -> BaseCodeGen e () -> BaseCodeGen e ()
submodule' modName cg = do
  cfg <- ask
  oldInfo <- get
  let info = emptyModule (moduleName oldInfo ++ [modName])
  liftIO (runCodeGen cg cfg info) >>= \case
         Left e -> throwError e
         Right (_, smInfo) -> if moduleCode smInfo == NoCode &&
                                 M.null (submodules smInfo)
                              then return ()
                              else modify' (addSubmodule modName smInfo)

-- | Run the given CodeGen in order to generate a submodule (specified
-- an an ordered list) of the current module.
submodule :: [Text] -> BaseCodeGen e () -> BaseCodeGen e ()
submodule [] cg = cg
submodule (m:ms) cg = submodule' m (submodule ms cg)

-- | Try running the given `action`, and if it fails run `fallback`
-- instead.
handleCGExc :: (CGError -> CodeGen a) -> ExcCodeGen a -> CodeGen a
handleCGExc fallback
 action = do
    cfg <- ask
    oldInfo <- get
    let info = cleanInfo oldInfo
    liftIO (runCodeGen action cfg info) >>= \case
        Left e -> fallback e
        Right (r, newInfo) -> do
            put (mergeInfo oldInfo newInfo)
            return r

-- | Return the currently loaded set of dependencies.
getDeps :: CodeGen Deps
getDeps = moduleDeps <$> get

-- | Return the ambient configuration for the code generator.
config :: CodeGen Config
config = hConfig <$> ask

-- | Return the name of the current module.
currentModule :: CodeGen Text
currentModule = do
  s <- get
  return (T.intercalate "." (moduleName s))

-- | Return the list of APIs available to the generator.
getAPIs :: CodeGen (M.Map Name API)
getAPIs = loadedAPIs <$> ask

-- | Due to the `forall` in the definition of `CodeGen`, if we want to
-- run the monad transformer stack until we get an `IO` action, our
-- only option is ignoring the possible error code from
-- `runExceptT`. This is perfectly safe, since there is no way to
-- construct a computation in the `CodeGen` monad that throws an
-- exception, due to the higher rank type.
unwrapCodeGen :: CodeGen a -> CodeGenConfig -> ModuleInfo ->
                 IO (a, ModuleInfo)
unwrapCodeGen cg cfg info =
    runCodeGen cg cfg info >>= \case
        Left _ -> error "unwrapCodeGen:: The impossible happened!"
        Right (r, newInfo) -> return (r, newInfo)

-- | Like `evalCodeGen`, but discard the resulting output value.
genCode :: Config -> M.Map Name API -> ModuleName -> CodeGen () ->
           IO ModuleInfo
genCode cfg apis mName cg = snd <$> evalCodeGen cfg apis mName cg

-- | Run a code generator, and return the information for the
-- generated module together with the return value of the generator.
evalCodeGen :: Config -> M.Map Name API -> ModuleName -> CodeGen a ->
               IO (a, ModuleInfo)
evalCodeGen cfg apis mName cg = do
  let initialInfo = emptyModule mName
      cfg' = CodeGenConfig {hConfig = cfg, loadedAPIs = apis}
  unwrapCodeGen cg cfg' initialInfo

-- | Mark the given dependency as used by the module.
registerNSDependency :: Text -> CodeGen ()
registerNSDependency name = do
    deps <- getDeps
    unless (Set.member name deps) $ do
        let newDeps = Set.insert name deps
        modify' $ \s -> s {moduleDeps = newDeps}

-- | Return the transitive set of dependencies, i.e. the union of
-- those of the module and (transitively) its submodules.
transitiveModuleDeps :: ModuleInfo -> Deps
transitiveModuleDeps minfo =
    Set.unions (moduleDeps minfo
               : map transitiveModuleDeps (M.elems $ submodules minfo))

-- | Given a module name and a symbol in the module (including a
-- proper namespace), return a qualified name for the symbol.
qualified :: ModuleName -> Name -> CodeGen Text
qualified mn (Name ns s) = do
  cfg <- config
  -- Make sure the module is listed as a dependency.
  when (modName cfg /= Just ns) $
    registerNSDependency ns
  minfo <- get
  if mn == moduleName minfo
  then return s
  else do
    qm <- qualifiedImport mn
    return (qm <> "." <> s)

-- | Import the given module name qualified (as a source import if the
-- namespace is the same as the current one), and return the name
-- under which the module was imported.
qualifiedImport :: ModuleName -> CodeGen Text
qualifiedImport mn = do
  modify' $ \s -> s {qualifiedImports = Set.insert mn (qualifiedImports s)}
  return (qualifiedModuleName mn)

-- | Construct a simplified version of the module name, suitable for a
-- qualified import.
qualifiedModuleName :: ModuleName -> Text
qualifiedModuleName ["GI", ns, "Objects", o] = ns <> "." <> o
qualifiedModuleName ["GI", ns, "Interfaces", i] = ns <> "." <> i
qualifiedModuleName ["GI", ns, "Structs", s] = ns <> "." <> s
qualifiedModuleName ["GI", ns, "Unions", u] = ns <> "." <> u
qualifiedModuleName ("GI" : rest) = dotModuleName rest
qualifiedModuleName mn = dotModuleName mn

-- | Return the minimal base version supported by the module and all
-- its submodules.
minBaseVersion :: ModuleInfo -> BaseVersion
minBaseVersion minfo =
    maximum (moduleMinBase minfo
            : map minBaseVersion (M.elems $ submodules minfo))

-- | Give a friendly textual description of the error for presenting
-- to the user.
describeCGError :: CGError -> Text
describeCGError (CGErrorNotImplemented e) = "Not implemented: " <> tshow e
describeCGError (CGErrorBadIntrospectionInfo e) = "Bad introspection data: " <> tshow e
describeCGError (CGErrorMissingInfo e) = "Missing info: " <> tshow e

notImplementedError :: Text -> ExcCodeGen a
notImplementedError s = throwError $ CGErrorNotImplemented s

badIntroError :: Text -> ExcCodeGen a
badIntroError s = throwError $ CGErrorBadIntrospectionInfo s

missingInfoError :: Text -> ExcCodeGen a
missingInfoError s = throwError $ CGErrorMissingInfo s

findAPI :: Type -> CodeGen (Maybe API)
findAPI TError = Just <$> findAPIByName (Name "GLib" "Error")
findAPI (TInterface ns n) = Just <$> findAPIByName (Name ns n)
findAPI _ = return Nothing

-- | Find the API associated with a given type. If the API cannot be
-- found this raises an `error`.
getAPI :: Type -> CodeGen API
getAPI t = findAPI t >>= \case
           Just a -> return a
           Nothing -> terror ("Could not resolve type \"" <> tshow t <> "\".")

findAPIByName :: Name -> CodeGen API
findAPIByName n@(Name ns _) = do
    apis <- getAPIs
    case M.lookup n apis of
        Just api -> return api
        Nothing ->
            terror $ "couldn't find API description for " <> ns <> "." <> name n

-- | Add some code to the current generator.
tellCode :: Code -> CodeGen ()
tellCode c = modify' (\s -> s {moduleCode = moduleCode s <> c})

-- | Print out a (newline-terminated) line.
line :: Text -> CodeGen ()
line = tellCode . Line

-- | Print out the given line both to the normal module, and to the
-- HsBoot file.
bline :: Text -> CodeGen ()
bline l = hsBoot (line l) >> line l

-- | A blank line
blank :: CodeGen ()
blank = line ""

-- | Increase the indent level for code generation.
indent :: BaseCodeGen e a -> BaseCodeGen e a
indent cg = do
  (x, code) <- recurseCG cg
  tellCode (Indent code)
  return x

-- | Group a set of related code.
group :: BaseCodeGen e a -> BaseCodeGen e a
group cg = do
  (x, code) <- recurseCG cg
  tellCode (Group code)
  blank
  return x

-- | Write the given code into the .hs-boot file for the current module.
hsBoot :: BaseCodeGen e a -> BaseCodeGen e a
hsBoot cg = do
  (x, code) <- recurseCG cg
  modify' (\s -> s{bootCode = bootCode s <> code})
  return x

-- | Add a export to the current module.
export :: Export -> CodeGen ()
export e =
    modify' $ \s -> s{moduleExports = moduleExports s |> e}

-- | Reexport a whole module.
exportModule :: SymbolName -> CodeGen ()
exportModule m = export (Export ExportModule m)

-- | Export a toplevel (i.e. belonging to no section) symbol.
exportToplevel :: SymbolName -> CodeGen ()
exportToplevel t = export (Export ExportToplevel t)

-- | Add a type declaration-related export.
exportDecl :: SymbolName -> CodeGen ()
exportDecl d = export (Export ExportTypeDecl d)

-- | Add a method export under the given section.
exportMethod :: HaddockSection -> SymbolName -> CodeGen ()
exportMethod s n = export (Export (ExportMethod s) n)

-- | Add a property-related export under the given section.
exportProperty :: HaddockSection -> SymbolName -> CodeGen ()
exportProperty s n = export (Export (ExportProperty s) n)

-- | Add a signal-related export under the given section.
exportSignal :: HaddockSection -> SymbolName -> CodeGen ()
exportSignal s n = export (Export (ExportSignal s) n)

-- | Set the language pragmas for the current module.
setLanguagePragmas :: [Text] -> CodeGen ()
setLanguagePragmas ps =
    modify' $ \s -> s{modulePragmas = Set.fromList ps}

-- | Set the GHC options for compiling this module (in a OPTIONS_GHC pragma).
setGHCOptions :: [Text] -> CodeGen ()
setGHCOptions opts =
    modify' $ \s -> s{moduleGHCOpts = Set.fromList opts}

-- | Set the given flags for the module.
setModuleFlags :: [ModuleFlag] -> CodeGen ()
setModuleFlags flags =
    modify' $ \s -> s{moduleFlags = Set.fromList flags}

-- | Set the minimum base version supported by the current module.
setModuleMinBase :: BaseVersion -> CodeGen ()
setModuleMinBase v =
    modify' $ \s -> s{moduleMinBase = max v (moduleMinBase s)}

-- | Add the given text to the module-level documentation for the
-- module being generated.
addModuleDocumentation :: Maybe Documentation -> CodeGen ()
addModuleDocumentation Nothing = return ()
addModuleDocumentation (Just doc) =
    modify' $ \s -> s{moduleDoc = moduleDoc s <> Just (docText doc)}

-- | Return a text representation of the `Code`.
codeToText :: Code -> Text
codeToText c = T.concat $ str 0 c []
    where
      str :: Int -> Code -> [Text] -> [Text]
      str _ NoCode cont = cont
      str n (Line s) cont =  paddedLine n s : cont
      str n (Indent c) cont = str (n + 1) c cont
      str n (Sequence s) cont = deseq n (S.viewl s) cont
      str n (Group c) cont = str n c cont

      deseq _ S.EmptyL cont = cont
      deseq n (c :< cs) cont = str n c (deseq n (S.viewl cs) cont)

-- | Pad a line to the given number of leading spaces, and add a
-- newline at the end.
paddedLine :: Int -> Text -> Text
paddedLine n s = T.replicate (n * 4) " " <> s <> "\n"

-- | Put a (padded) comma at the end of the text.
comma :: Text -> Text
comma s = padTo 40 s <> ","

-- | Format the list of exported modules.
formatExportedModules :: [Export] -> Maybe Text
formatExportedModules [] = Nothing
formatExportedModules exports =
    Just . T.concat . map ( paddedLine 1
                           . comma
                           . ("module " <>)
                           . exportSymbol)
          . filter ((== ExportModule) . exportType) $ exports

-- | Format the toplevel exported symbols.
formatToplevel :: [Export] -> Maybe Text
formatToplevel [] = Nothing
formatToplevel exports =
    Just . T.concat . map (paddedLine 1 . comma . exportSymbol)
         . filter ((== ExportToplevel) . exportType) $ exports

-- | Format the type declarations section.
formatTypeDecls :: [Export] -> Maybe Text
formatTypeDecls exports =
    let exportedTypes = filter ((== ExportTypeDecl) . exportType) exports
    in if exportedTypes == []
       then Nothing
       else Just . T.unlines $ [ "-- * Exported types"
                               , T.concat . map ( paddedLine 1
                                                . comma
                                                . exportSymbol )
                                      $ exportedTypes ]

-- | Format a given section made of subsections.
formatSection :: Text -> (Export -> Maybe (HaddockSection, SymbolName)) ->
                 [Export] -> Maybe Text
formatSection section filter exports =
    if M.null exportedSubsections
    then Nothing
    else Just . T.unlines $ [" -- * " <> section
                            , ( T.unlines
                              . map formatSubsection
                              . M.toList ) exportedSubsections]

    where
      filteredExports :: [(HaddockSection, SymbolName)]
      filteredExports = catMaybes (map filter exports)

      exportedSubsections :: M.Map HaddockSection (Set.Set SymbolName)
      exportedSubsections = foldr extract M.empty filteredExports

      extract :: (HaddockSection, SymbolName) ->
                 M.Map Text (Set.Set Text) -> M.Map Text (Set.Set Text)
      extract (subsec, m) secs =
          M.insertWith Set.union subsec (Set.singleton m) secs

      formatSubsection :: (HaddockSection, Set.Set SymbolName) -> Text
      formatSubsection (subsec, symbols) =
          T.unlines [ "-- ** " <> subsec
                    , ( T.concat
                      . map (paddedLine 1 . comma)
                      . Set.toList ) symbols]

-- | Format the list of methods.
formatMethods :: [Export] -> Maybe Text
formatMethods = formatSection "Methods" toMethod
    where toMethod :: Export -> Maybe (HaddockSection, SymbolName)
          toMethod (Export (ExportMethod s) m) = Just (s, m)
          toMethod _ = Nothing

-- | Format the list of properties.
formatProperties :: [Export] -> Maybe Text
formatProperties = formatSection "Properties" toProperty
    where toProperty :: Export -> Maybe (HaddockSection, SymbolName)
          toProperty (Export (ExportProperty s) m) = Just (s, m)
          toProperty _ = Nothing

-- | Format the list of signals.
formatSignals :: [Export] -> Maybe Text
formatSignals = formatSection "Signals" toSignal
    where toSignal :: Export -> Maybe (HaddockSection, SymbolName)
          toSignal (Export (ExportSignal s) m) = Just (s, m)
          toSignal _ = Nothing

-- | Format the given export list. This is just the inside of the
-- parenthesis.
formatExportList :: [Export] -> Text
formatExportList exports =
    T.unlines . catMaybes $ [ formatExportedModules exports
                            , formatToplevel exports
                            , formatTypeDecls exports
                            , formatMethods exports
                            , formatProperties exports
                            , formatSignals exports ]

-- | Write down the list of language pragmas.
languagePragmas :: [Text] -> Text
languagePragmas [] = ""
languagePragmas ps = "{-# LANGUAGE " <> T.intercalate ", " ps <> " #-}\n"

-- | Write down the list of GHC options.
ghcOptions :: [Text] -> Text
ghcOptions [] = ""
ghcOptions opts = "{-# OPTIONS_GHC " <> T.intercalate ", " opts <> " #-}\n"

-- | Standard fields for every module.
standardFields :: Text
standardFields = T.unlines [ "Copyright  : " <> authors
                           , "License    : " <> license
                           , "Maintainer : " <> maintainers ]

-- | The haddock header for the module, including optionally a description.
moduleHaddock :: Maybe Text -> Text
moduleHaddock Nothing = T.unlines ["{- |", standardFields <> "-}"]
moduleHaddock (Just description) = T.unlines ["{- |", standardFields,
                                              description, "-}"]

-- | Generic module prelude. We reexport all of the submodules.
modulePrelude :: Text -> [Export] -> [Text] -> Text
modulePrelude name [] [] = "module " <> name <> " () where\n"
modulePrelude name exports [] =
    "module " <> name <> "\n    ( "
    <> formatExportList exports
    <> "    ) where\n"
modulePrelude name [] reexportedModules =
    "module " <> name <> "\n    ( "
    <> formatExportList (map (Export ExportModule) reexportedModules)
    <> "    ) where\n\n"
    <> T.unlines (map ("import " <>) reexportedModules)
modulePrelude name exports reexportedModules =
    "module " <> name <> "\n    ( "
    <> formatExportList (map (Export ExportModule) reexportedModules)
    <> "\n"
    <> formatExportList exports
    <> "    ) where\n\n"
    <> T.unlines (map ("import " <>) reexportedModules)

-- | Code for loading the needed dependencies. One needs to give the
-- prefix for the namespace being currently generated, modules with
-- this prefix will be imported as {-# SOURCE #-}, and otherwise will
-- be imported normally.
importDeps :: ModuleName -> [ModuleName] -> Text
importDeps _ [] = ""
importDeps prefix deps = T.unlines . map toImport $ deps
    where toImport :: ModuleName -> Text
          toImport dep = let impSt = if importSource dep
                                     then "import {-# SOURCE #-} qualified "
                                     else "import qualified "
                         in impSt <> dotModuleName dep <>
                                " as " <> qualifiedModuleName dep
          importSource :: ModuleName -> Bool
          importSource ["GI", _, "Callbacks"] = False
          importSource mn = take (length prefix) mn == prefix

-- | Standard imports.
moduleImports :: Text
moduleImports = T.unlines [ "import Data.GI.Base.ShortPrelude"
                          , "import qualified Prelude as P"
                          , ""
                          , "import qualified Data.GI.Base.Attributes as GI.Attributes"
                          , "import qualified Data.Text as T"
                          , "import qualified Data.ByteString.Char8 as B"
                          , "import qualified Data.Map as Map" ]

-- | Write to disk the code for a module, under the given base
-- directory. Does not write submodules recursively, for that use
-- `writeModuleTree`.
writeModuleInfo :: Bool -> Maybe FilePath -> ModuleInfo -> IO ()
writeModuleInfo verbose dirPrefix minfo = do
  let submoduleNames = map (moduleName) (M.elems (submodules minfo))
      -- We reexport any submodules.
      submoduleExports = map dotModuleName submoduleNames
      fname = moduleNameToPath dirPrefix (moduleName minfo) ".hs"
      dirname = takeDirectory fname
      code = codeToText (moduleCode minfo)
      pragmas = languagePragmas (Set.toList $ modulePragmas minfo)
      optionsGHC = ghcOptions (Set.toList $ moduleGHCOpts minfo)
      prelude = modulePrelude (dotModuleName $ moduleName minfo)
                (F.toList (moduleExports minfo))
                submoduleExports
      imports = if ImplicitPrelude `Set.member` moduleFlags minfo
                then ""
                else moduleImports
      pkgRoot = take 2 (moduleName minfo)
      deps = importDeps pkgRoot (Set.toList $ qualifiedImports minfo)
      haddock = moduleHaddock (moduleDoc minfo)

  when verbose $ putStrLn ((T.unpack . dotModuleName . moduleName) minfo
                           ++ " -> " ++ fname)
  createDirectoryIfMissing True dirname
  TIO.writeFile fname (T.unlines [pragmas, optionsGHC, haddock, prelude,
                                  imports, deps, code])
  when (bootCode minfo /= NoCode) $ do
    let bootFName = moduleNameToPath dirPrefix (moduleName minfo) ".hs-boot"
    TIO.writeFile bootFName (genHsBoot minfo)

-- | Generate the .hs-boot file for the given module.
genHsBoot :: ModuleInfo -> Text
genHsBoot minfo =
    "module " <> (dotModuleName . moduleName) minfo <> " where\n\n" <>
    moduleImports <> "\n" <>
    codeToText (bootCode minfo)

-- | Construct the filename corresponding to the given module.
moduleNameToPath :: Maybe FilePath -> ModuleName -> FilePath -> FilePath
moduleNameToPath dirPrefix mn ext =
    joinPath (fromMaybe "" dirPrefix : map T.unpack mn) ++ ext

-- | Turn an abstract module name into its dotted representation. For
-- instance, ["GI", "Gtk", "Types"] -> GI.Gtk.Types.
dotModuleName :: ModuleName -> Text
dotModuleName mn = T.intercalate "." mn

-- | Write down the code for a module and its submodules to disk under
-- the given base directory. It returns the list of written modules.
writeModuleTree :: Bool -> Maybe FilePath -> ModuleInfo -> IO [Text]
writeModuleTree verbose dirPrefix minfo = do
  submoduleNames <- concat <$> forM (M.elems (submodules minfo))
                                    (writeModuleTree verbose dirPrefix)
  writeModuleInfo verbose dirPrefix minfo
  return $ (dotModuleName (moduleName minfo) : submoduleNames)

-- | Return the list of modules `writeModuleTree` would write, without
-- actually writing anything to disk.
listModuleTree :: ModuleInfo -> [Text]
listModuleTree minfo =
    let submoduleNames = concatMap listModuleTree (M.elems (submodules minfo))
    in dotModuleName (moduleName minfo) : submoduleNames
