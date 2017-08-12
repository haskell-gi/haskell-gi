{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.GI.CodeGen.Code
    ( Code
    , ModuleInfo(moduleCode, moduleDoc)
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
    , increaseIndent
    , bline
    , line
    , blank
    , group
    , cppIf
    , CPPGuard(..)
    , hsBoot
    , submodule
    , setLanguagePragmas
    , setGHCOptions
    , setModuleFlags
    , setModuleMinBase

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
    , getC2HMap

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
import Data.Monoid ((<>), mempty)
import qualified Data.Map.Strict as M
import Data.Sequence (ViewL ((:<)), viewl, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as LT

import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, takeDirectory)

import Data.GI.CodeGen.API (API, Name(..))
import Data.GI.CodeGen.Config (Config(..))
import {-# SOURCE #-} Data.GI.CodeGen.CtoHaskellMap (cToHaskellMap,
                                                     Hyperlink)
import Data.GI.CodeGen.GtkDoc (CRef)
import Data.GI.CodeGen.ModulePath (ModulePath(..), dotModulePath, (/.))
import Data.GI.CodeGen.Type (Type(..))
import Data.GI.CodeGen.Util (tshow, terror, padTo, utf8WriteFile)
import Data.GI.CodeGen.ProjectInfo (authors, license, maintainers)

-- | Set of CPP conditionals understood by the code generator.
data CPPConditional = CPPIf Text -- ^ #if Foo
  deriving (Eq, Show, Ord)

-- | The generated `Code` is a sequence of `CodeToken`s.
newtype Code = Code (Seq.Seq CodeToken)
  deriving (Monoid, Eq, Show, Ord)

-- | Initializes a code block to the empty sequence.
emptyCode :: Code
emptyCode = Code Seq.empty

-- | Checks whether the given code block is empty.
isCodeEmpty :: Code -> Bool
isCodeEmpty (Code seq) = Seq.null seq

-- | A block of code consisting of a single token.
codeSingleton :: CodeToken -> Code
codeSingleton t = Code (Seq.singleton t)

-- | Possible code tokens.
data CodeToken
    = Line Text           -- ^ A single line, indented to current indentation.
    | Indent Code         -- ^ Indented region.
    | Group Code          -- ^ A grouped set of lines
    | IncreaseIndent      -- ^ Increase the indentation for the rest
                          -- of the lines in the group.
    | CPPBlock CPPConditional Code -- ^ A block of code guarded by the
                                   -- given CPP conditional
    deriving (Eq, Ord, Show)

type Deps = Set.Set Text

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
    , exportGuards  :: [CPPConditional] -- ^ Protect the export by the
                                        -- given CPP export guards.
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
      modulePath :: ModulePath -- ^ Full module name: ["Gtk", "Label"].
    , moduleCode :: Code       -- ^ Generated code for the module.
    , bootCode   :: Code       -- ^ Interfaces going into the .hs-boot file.
    , submodules :: M.Map Text ModuleInfo -- ^ Indexed by the relative
                                          -- module name.
    , moduleDeps :: Deps -- ^ Set of dependencies for this module.
    , moduleExports :: Seq.Seq Export -- ^ Exports for the module.
    , qualifiedImports :: Set.Set ModulePath -- ^ Qualified (source) imports.
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
emptyModule :: ModulePath -> ModuleInfo
emptyModule m = ModuleInfo { modulePath = m
                           , moduleCode = emptyCode
                           , bootCode = emptyCode
                           , submodules = M.empty
                           , moduleDeps = Set.empty
                           , moduleExports = Seq.empty
                           , qualifiedImports = Set.empty
                           , modulePragmas = Set.empty
                           , moduleGHCOpts = Set.empty
                           , moduleFlags = Set.empty
                           , moduleDoc = Nothing
                           , moduleMinBase = Base47
                           }

-- | Information for the code generator.
data CodeGenConfig = CodeGenConfig {
      hConfig     :: Config          -- ^ Ambient config.
    , loadedAPIs  :: M.Map Name API  -- ^ APIs available to the generator.
    , c2hMap      :: M.Map CRef Hyperlink -- ^ Map from C references
                                          -- to Haskell symbols.
    }

-- | Set of errors for the code generator.
data CGError = CGErrorNotImplemented Text
             | CGErrorBadIntrospectionInfo Text
             | CGErrorMissingInfo Text
               deriving (Show)

-- | Temporaty state for the code generator.
data CGState = CGState {
  cgsCPPConditionals :: [CPPConditional] -- ^ Active CPP conditionals,
                                         -- outermost condition first.
  }

-- | Clean slate for `CGState`.
emptyCGState = CGState {
  cgsCPPConditionals = []
  }

-- | The base type for the code generator monad.
type BaseCodeGen excType a =
  ReaderT CodeGenConfig (StateT (CGState, ModuleInfo) (Except excType)) a

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

-- | Run a `CodeGen` with given `Config` and initial state, returning
-- either the resulting exception, or the result and final module info.
runCodeGen :: BaseCodeGen e a -> CodeGenConfig -> (CGState, ModuleInfo) ->
              (Either e (a, ModuleInfo))
runCodeGen cg cfg state =
  dropCGState <$> runExcept (runStateT (runReaderT cg cfg) state)
  where dropCGState :: (a, (CGState, ModuleInfo)) -> (a, ModuleInfo)
        dropCGState (x, (_, m)) = (x, m)

-- | This is useful when we plan run a subgenerator, and `mconcat` the
-- result to the original structure later.
cleanInfo :: ModuleInfo -> ModuleInfo
cleanInfo info = info { moduleCode = emptyCode, submodules = M.empty,
                        bootCode = emptyCode, moduleExports = Seq.empty,
                        qualifiedImports = Set.empty,
                        moduleDoc = Nothing, moduleMinBase = Base47 }

-- | Run the given code generator using the state and config of an
-- ambient CodeGen, but without adding the generated code to
-- `moduleCode`, instead returning it explicitly.
recurseCG :: BaseCodeGen e a -> BaseCodeGen e (a, Code)
recurseCG = recurseWithState id

-- | Like `recurseCG`, but we allow for explicitly setting the state
-- of the inner code generator.
recurseWithState :: (CGState -> CGState) -> BaseCodeGen e a
                 -> BaseCodeGen e (a, Code)
recurseWithState cgsSet cg = do
  cfg <- ask
  (cgs, oldInfo) <- get
  -- Start the subgenerator with no code and no submodules.
  let info = cleanInfo oldInfo
  case runCodeGen cg cfg (cgsSet cgs, info) of
     Left e -> throwError e
     Right (r, new) -> put (cgs, mergeInfoState oldInfo new) >>
                       return (r, moduleCode new)

-- | Like `recurseCG`, giving explicitly the set of loaded APIs and C to
-- Haskell map for the subgenerator.
recurseWithAPIs :: M.Map Name API -> CodeGen () -> CodeGen ()
recurseWithAPIs apis cg = do
  cfg <- ask
  (cgs, oldInfo) <- get
  -- Start the subgenerator with no code and no submodules.
  let info = cleanInfo oldInfo
      cfg' = cfg {loadedAPIs = apis,
                  c2hMap = cToHaskellMap (M.toList apis)}
  case runCodeGen cg cfg' (cgs, info) of
    Left e -> throwError e
    Right (_, new) -> put (cgs, mergeInfo oldInfo new)

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
addSubmodule :: Text -> ModuleInfo -> (CGState, ModuleInfo)
             -> (CGState, ModuleInfo)
addSubmodule modName submodule (cgs, current) =
  (cgs, current { submodules = M.insertWith mergeInfo modName submodule (submodules current)})

-- | Run the given CodeGen in order to generate a single submodule of the
-- current module. Note that we do not generate the submodule if the
-- code generator generated no code and the module does not have
-- submodules.
submodule' :: Text -> BaseCodeGen e () -> BaseCodeGen e ()
submodule' modName cg = do
  cfg <- ask
  (_, oldInfo) <- get
  let info = emptyModule (modulePath oldInfo /. modName)
  case runCodeGen cg cfg (emptyCGState, info) of
    Left e -> throwError e
    Right (_, smInfo) -> if isCodeEmpty (moduleCode smInfo) &&
                            M.null (submodules smInfo)
                         then return ()
                         else modify' (addSubmodule modName smInfo)

-- | Run the given CodeGen in order to generate a submodule (specified
-- an an ordered list) of the current module.
submodule :: ModulePath -> BaseCodeGen e () -> BaseCodeGen e ()
submodule (ModulePath []) cg = cg
submodule (ModulePath (m:ms)) cg = submodule' m (submodule (ModulePath ms) cg)

-- | Try running the given `action`, and if it fails run `fallback`
-- instead.
handleCGExc :: (CGError -> CodeGen a) -> ExcCodeGen a -> CodeGen a
handleCGExc fallback
 action = do
    cfg <- ask
    (cgs, oldInfo) <- get
    let info = cleanInfo oldInfo
    case runCodeGen action cfg (cgs, info) of
      Left e -> fallback e
      Right (r, newInfo) -> do
        put (cgs, mergeInfo oldInfo newInfo)
        return r

-- | Return the currently loaded set of dependencies.
getDeps :: CodeGen Deps
getDeps = moduleDeps . snd <$> get

-- | Return the ambient configuration for the code generator.
config :: CodeGen Config
config = hConfig <$> ask

-- | Return the name of the current module.
currentModule :: CodeGen Text
currentModule = do
  (_, s) <- get
  return (dotWithPrefix (modulePath s))

-- | Return the list of APIs available to the generator.
getAPIs :: CodeGen (M.Map Name API)
getAPIs = loadedAPIs <$> ask

-- | Return the C -> Haskell available to the generator.
getC2HMap :: CodeGen (M.Map CRef Hyperlink)
getC2HMap = c2hMap <$> ask

-- | Due to the `forall` in the definition of `CodeGen`, if we want to
-- run the monad transformer stack until we get a result, our only
-- option is ignoring the possible error code from `runExcept`. This
-- is perfectly safe, since there is no way to construct a computation
-- in the `CodeGen` monad that throws an exception, due to the higher
-- rank type.
unwrapCodeGen :: CodeGen a -> CodeGenConfig -> (CGState, ModuleInfo)
              -> (a, ModuleInfo)
unwrapCodeGen cg cfg info =
    case runCodeGen cg cfg info of
      Left _ -> error "unwrapCodeGen:: The impossible happened!"
      Right (r, newInfo) -> (r, newInfo)

-- | Run a code generator, and return the information for the
-- generated module together with the return value of the generator.
evalCodeGen :: Config -> M.Map Name API ->
               ModulePath -> CodeGen a -> (a, ModuleInfo)
evalCodeGen cfg apis mPath cg =
  let initialInfo = emptyModule mPath
      cfg' = CodeGenConfig {hConfig = cfg, loadedAPIs = apis,
                            c2hMap = cToHaskellMap (M.toList apis)}
  in unwrapCodeGen cg cfg' (emptyCGState, initialInfo)

-- | Like `evalCodeGen`, but discard the resulting output value.
genCode :: Config -> M.Map Name API ->
           ModulePath -> CodeGen () -> ModuleInfo
genCode cfg apis mPath cg = snd $ evalCodeGen cfg apis mPath cg

-- | Mark the given dependency as used by the module.
registerNSDependency :: Text -> CodeGen ()
registerNSDependency name = do
    deps <- getDeps
    unless (Set.member name deps) $ do
        let newDeps = Set.insert name deps
        modify' $ \(cgs, s) -> (cgs, s {moduleDeps = newDeps})

-- | Return the transitive set of dependencies, i.e. the union of
-- those of the module and (transitively) its submodules.
transitiveModuleDeps :: ModuleInfo -> Deps
transitiveModuleDeps minfo =
    Set.unions (moduleDeps minfo
               : map transitiveModuleDeps (M.elems $ submodules minfo))

-- | Given a module name and a symbol in the module (including a
-- proper namespace), return a qualified name for the symbol.
qualified :: ModulePath -> Name -> CodeGen Text
qualified mp (Name ns s) = do
  cfg <- config
  -- Make sure the module is listed as a dependency.
  when (modName cfg /= ns) $
    registerNSDependency ns
  (_, minfo) <- get
  if mp == modulePath minfo
  then return s
  else do
    qm <- qualifiedImport mp
    return (qm <> "." <> s)

-- | Import the given module name qualified (as a source import if the
-- namespace is the same as the current one), and return the name
-- under which the module was imported.
qualifiedImport :: ModulePath -> CodeGen Text
qualifiedImport mp = do
  modify' $ \(cgs, s) -> (cgs, s {qualifiedImports = Set.insert mp (qualifiedImports s)})
  return (qualifiedModuleName mp)

-- | Construct a simplified version of the module name, suitable for a
-- qualified import.
qualifiedModuleName :: ModulePath -> Text
qualifiedModuleName (ModulePath [ns, "Objects", o]) = ns <> "." <> o
qualifiedModuleName (ModulePath [ns, "Interfaces", i]) = ns <> "." <> i
qualifiedModuleName (ModulePath [ns, "Structs", s]) = ns <> "." <> s
qualifiedModuleName (ModulePath [ns, "Unions", u]) = ns <> "." <> u
qualifiedModuleName mp = dotModulePath mp

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
findAPI (TInterface n) = Just <$> findAPIByName n
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
tellCode :: CodeToken -> CodeGen ()
tellCode c = modify' (\(cgs, s) -> (cgs, s {moduleCode = moduleCode s <>
                                                         codeSingleton c}))

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

-- | Increase the indentation level for the rest of the lines in the
-- current group.
increaseIndent :: CodeGen ()
increaseIndent = tellCode IncreaseIndent

-- | Group a set of related code.
group :: BaseCodeGen e a -> BaseCodeGen e a
group cg = do
  (x, code) <- recurseCG cg
  tellCode (Group code)
  blank
  return x

-- | Guard a block of code with @#if@.
cppIfBlock :: Text -> BaseCodeGen e a -> BaseCodeGen e a
cppIfBlock cond cg = do
  (x, code) <- recurseWithState addConditional cg
  tellCode (CPPBlock (CPPIf cond) code)
  blank
  return x
    where addConditional :: CGState -> CGState
          addConditional cgs = CGState {cgsCPPConditionals = CPPIf cond :
                                         cgsCPPConditionals cgs}

-- | Possible features to test via CPP.
data CPPGuard = CPPOverloading -- ^ Enable overloading

-- | Guard a code block with CPP code, such that it is included only
-- if the specified feature is enabled.
cppIf :: CPPGuard -> BaseCodeGen e a -> BaseCodeGen e a
cppIf CPPOverloading = cppIfBlock "defined(ENABLE_OVERLOADING) && !defined(__HADDOCK_VERSION__)" -- Do not generate docs for the overloading machinery.

-- | Write the given code into the .hs-boot file for the current module.
hsBoot :: BaseCodeGen e a -> BaseCodeGen e a
hsBoot cg = do
  (x, code) <- recurseCG cg
  modify' (\(cgs, s) -> (cgs, s{bootCode = bootCode s <>
                               addGuards (cgsCPPConditionals cgs) code}))
  return x
  where addGuards :: [CPPConditional] -> Code -> Code
        addGuards [] c = c
        addGuards (cond : conds) c = codeSingleton $ CPPBlock cond (addGuards conds c)

-- | Add a export to the current module.
export :: ([CPPConditional] -> Export) -> CodeGen ()
export partial =
    modify' $ \(cgs, s) -> (cgs,
                            let e = partial $ cgsCPPConditionals cgs
                            in s{moduleExports = moduleExports s |> e})

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
    modify' $ \(cgs, s) -> (cgs, s{modulePragmas = Set.fromList ps})

-- | Set the GHC options for compiling this module (in a OPTIONS_GHC pragma).
setGHCOptions :: [Text] -> CodeGen ()
setGHCOptions opts =
    modify' $ \(cgs, s) -> (cgs, s{moduleGHCOpts = Set.fromList opts})

-- | Set the given flags for the module.
setModuleFlags :: [ModuleFlag] -> CodeGen ()
setModuleFlags flags =
    modify' $ \(cgs, s) -> (cgs, s{moduleFlags = Set.fromList flags})

-- | Set the minimum base version supported by the current module.
setModuleMinBase :: BaseVersion -> CodeGen ()
setModuleMinBase v =
    modify' $ \(cgs, s) -> (cgs, s{moduleMinBase = max v (moduleMinBase s)})

-- | Format a CPP conditional.
cppCondFormat :: CPPConditional -> (Text, Text)
cppCondFormat (CPPIf c) = ("#if " <> c <> "\n", "#endif\n")

-- | Return a text representation of the `Code`.
codeToText :: Code -> Text
codeToText (Code seq) = LT.toStrict . B.toLazyText $ genCode 0 (viewl seq)
  where genCode :: Int -> ViewL CodeToken -> B.Builder
        genCode _ Seq.EmptyL = mempty
        genCode n (Line s :< rest) = B.fromText (paddedLine n s) <>
                                      genCode n (viewl rest)
        genCode n (Indent (Code seq) :< rest) = genCode (n+1) (viewl seq) <>
                                      genCode n (viewl rest)
        genCode n (Group (Code seq) :< rest) = genCode n (viewl seq) <>
                                               genCode n (viewl rest)
        genCode n (CPPBlock cond (Code seq) :< rest) =
          let (condBegin, condEnd) = cppCondFormat cond
          in B.fromText condBegin <> genCode n (viewl seq) <>
             B.fromText condEnd <> genCode n (viewl rest)
        genCode n (IncreaseIndent :< rest) = genCode (n+1) (viewl rest)

-- | Pad a line to the given number of leading spaces, and add a
-- newline at the end.
paddedLine :: Int -> Text -> Text
paddedLine n s = T.replicate (n * 4) " " <> s <> "\n"

-- | Put a (padded) comma at the end of the text.
comma :: Text -> Text
comma s = padTo 40 s <> ","

-- | Format the given export symbol.
formatExport :: (Export -> Text) -> Export -> Text
formatExport formatName export = go (exportGuards export)
  where go :: [CPPConditional] -> Text
        go [] = (paddedLine 1 . comma . formatName) export
        go (c:cs) = let (begin, end) = cppCondFormat c
                    in begin <> go cs <> end

-- | Format the list of exported modules.
formatExportedModules :: [Export] -> Maybe Text
formatExportedModules [] = Nothing
formatExportedModules exports =
    Just . T.concat . map (formatExport (("module " <>) . exportSymbol))
          . filter ((== ExportModule) . exportType) $ exports

-- | Format the toplevel exported symbols.
formatToplevel :: [Export] -> Maybe Text
formatToplevel [] = Nothing
formatToplevel exports =
    Just . T.concat . map (formatExport exportSymbol)
         . filter ((== ExportToplevel) . exportType) $ exports

-- | Format the type declarations section.
formatTypeDecls :: [Export] -> Maybe Text
formatTypeDecls exports =
    let exportedTypes = filter ((== ExportTypeDecl) . exportType) exports
    in if exportedTypes == []
       then Nothing
       else Just . T.unlines $ [ "-- * Exported types"
                               , T.concat . map ( formatExport exportSymbol )
                                      $ exportedTypes ]

-- | A subsection name, with an optional anchor name.
data Subsection = Subsection { subsectionTitle  :: Text
                             , subsectionAnchor :: Maybe Text
                             } deriving (Eq, Show, Ord)

-- | A subsection with an anchor given by the title and @prefix:title@ anchor.
subsecWithPrefix prefix title =
  Subsection { subsectionTitle = title
             , subsectionAnchor = Just (prefix <> ":" <> title) }

-- | Format a given section made of subsections.
formatSection :: Text -> (Export -> Maybe (Subsection, Export)) ->
                 [Export] -> Maybe Text
formatSection section filter exports =
    if M.null exportedSubsections
    then Nothing
    else Just . T.unlines $ [" -- * " <> section
                            , ( T.unlines
                              . map formatSubsection
                              . M.toList ) exportedSubsections]

    where
      filteredExports :: [(Subsection, Export)]
      filteredExports = catMaybes (map filter exports)

      exportedSubsections :: M.Map Subsection (Set.Set Export)
      exportedSubsections = foldr extract M.empty filteredExports

      extract :: (Subsection, Export) -> M.Map Subsection (Set.Set Export)
              -> M.Map Subsection (Set.Set Export)
      extract (subsec, m) secs =
          M.insertWith Set.union subsec (Set.singleton m) secs

      formatSubsection :: (Subsection, Set.Set Export) -> Text
      formatSubsection (subsec, symbols) =
          T.unlines [ "-- ** " <> case subsectionAnchor subsec of
                                    Just anchor -> subsectionTitle subsec <>
                                                   " #" <> anchor <> "#"
                                    Nothing -> subsectionTitle subsec
                    , ( T.concat
                      . map (formatExport exportSymbol)
                      . Set.toList ) symbols]

-- | Format the list of methods.
formatMethods :: [Export] -> Maybe Text
formatMethods = formatSection "Methods" toMethod
    where toMethod :: Export -> Maybe (Subsection, Export)
          toMethod e@(Export (ExportMethod s) _ _) =
            Just (subsecWithPrefix "method" s, e)
          toMethod _ = Nothing

-- | Format the list of properties.
formatProperties :: [Export] -> Maybe Text
formatProperties = formatSection "Properties" toProperty
    where toProperty :: Export -> Maybe (Subsection, Export)
          toProperty e@(Export (ExportProperty s) _ _) =
            Just (subsecWithPrefix "attr" s, e)
          toProperty _ = Nothing

-- | Format the list of signals.
formatSignals :: [Export] -> Maybe Text
formatSignals = formatSection "Signals" toSignal
    where toSignal :: Export -> Maybe (Subsection, Export)
          toSignal e@(Export (ExportSignal s) _ _) =
            Just (subsecWithPrefix "signal" s, e)
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
    <> formatExportList (map (\m -> Export ExportModule m []) reexportedModules)
    <> "    ) where\n\n"
    <> T.unlines (map ("import " <>) reexportedModules)
modulePrelude name exports reexportedModules =
    "module " <> name <> "\n    ( "
    <> formatExportList (map (\m -> Export ExportModule m []) reexportedModules)
    <> "\n"
    <> formatExportList exports
    <> "    ) where\n\n"
    <> T.unlines (map ("import " <>) reexportedModules)

-- | Code for loading the needed dependencies. One needs to give the
-- prefix for the namespace being currently generated, modules with
-- this prefix will be imported as {-# SOURCE #-}, and otherwise will
-- be imported normally.
importDeps :: ModulePath -> [ModulePath] -> Text
importDeps _ [] = ""
importDeps (ModulePath prefix) deps = T.unlines . map toImport $ deps
    where toImport :: ModulePath -> Text
          toImport dep = let impSt = if importSource dep
                                     then "import {-# SOURCE #-} qualified "
                                     else "import qualified "
                         in impSt <> dotWithPrefix dep <>
                                " as " <> qualifiedModuleName dep
          importSource :: ModulePath -> Bool
          importSource (ModulePath [_, "Callbacks"]) = False
          importSource (ModulePath mp) = take (length prefix) mp == prefix

-- | Standard imports.
moduleImports :: Text
moduleImports = T.unlines [
                 "import Data.GI.Base.ShortPrelude"
                , "import qualified Data.GI.Base.ShortPrelude as SP"
                , "import qualified Data.GI.Base.Overloading as O"
                , "import qualified Prelude as P"
                , ""
                , "import qualified Data.GI.Base.Attributes as GI.Attributes"
                , "import qualified Data.GI.Base.ManagedPtr as B.ManagedPtr"
                , "import qualified Data.GI.Base.GError as B.GError"
                , "import qualified Data.GI.Base.GVariant as B.GVariant"
                , "import qualified Data.GI.Base.GParamSpec as B.GParamSpec"
                , "import qualified Data.GI.Base.CallStack as B.CallStack"
                , "import qualified Data.Text as T"
                , "import qualified Data.ByteString.Char8 as B"
                , "import qualified Data.Map as Map"
                , "import qualified Foreign.Ptr as FP" ]

-- | Like `dotModulePath`, but add a "GI." prefix.
dotWithPrefix :: ModulePath -> Text
dotWithPrefix mp = dotModulePath ("GI" <> mp)

-- | Write to disk the code for a module, under the given base
-- directory. Does not write submodules recursively, for that use
-- `writeModuleTree`.
writeModuleInfo :: Bool -> Maybe FilePath -> ModuleInfo -> IO ()
writeModuleInfo verbose dirPrefix minfo = do
  let submodulePaths = map (modulePath) (M.elems (submodules minfo))
      -- We reexport any submodules.
      submoduleExports = map dotWithPrefix submodulePaths
      fname = modulePathToFilePath dirPrefix (modulePath minfo) ".hs"
      dirname = takeDirectory fname
      code = codeToText (moduleCode minfo)
      pragmas = languagePragmas (Set.toList $ modulePragmas minfo)
      optionsGHC = ghcOptions (Set.toList $ moduleGHCOpts minfo)
      prelude = modulePrelude (dotWithPrefix $ modulePath minfo)
                (F.toList (moduleExports minfo))
                submoduleExports
      imports = if ImplicitPrelude `Set.member` moduleFlags minfo
                then ""
                else moduleImports
      pkgRoot = ModulePath (take 1 (modulePathToList $ modulePath minfo))
      deps = importDeps pkgRoot (Set.toList $ qualifiedImports minfo)
      haddock = moduleHaddock (moduleDoc minfo)

  when verbose $ putStrLn ((T.unpack . dotWithPrefix . modulePath) minfo
                           ++ " -> " ++ fname)
  createDirectoryIfMissing True dirname
  utf8WriteFile fname (T.unlines [pragmas, optionsGHC, haddock,
                                 prelude, imports, deps, code])
  when (not . isCodeEmpty $ bootCode minfo) $ do
    let bootFName = modulePathToFilePath dirPrefix (modulePath minfo) ".hs-boot"
    utf8WriteFile bootFName (genHsBoot minfo)

-- | Generate the .hs-boot file for the given module.
genHsBoot :: ModuleInfo -> Text
genHsBoot minfo =
    "module " <> (dotWithPrefix . modulePath) minfo <> " where\n\n" <>
    moduleImports <> "\n" <>
    codeToText (bootCode minfo)

-- | Construct the filename corresponding to the given module.
modulePathToFilePath :: Maybe FilePath -> ModulePath -> FilePath -> FilePath
modulePathToFilePath dirPrefix (ModulePath mp) ext =
    joinPath (fromMaybe "" dirPrefix : "GI" : map T.unpack mp) ++ ext

-- | Write down the code for a module and its submodules to disk under
-- the given base directory. It returns the list of written modules.
writeModuleTree :: Bool -> Maybe FilePath -> ModuleInfo -> IO [Text]
writeModuleTree verbose dirPrefix minfo = do
  submodulePaths <- concat <$> forM (M.elems (submodules minfo))
                                    (writeModuleTree verbose dirPrefix)
  writeModuleInfo verbose dirPrefix minfo
  return $ (dotWithPrefix (modulePath minfo) : submodulePaths)

-- | Return the list of modules `writeModuleTree` would write, without
-- actually writing anything to disk.
listModuleTree :: ModuleInfo -> [Text]
listModuleTree minfo =
    let submodulePaths = concatMap listModuleTree (M.elems (submodules minfo))
    in dotWithPrefix (modulePath minfo) : submodulePaths
