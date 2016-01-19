module GI.Code
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
    , codeToText
    , transitiveModuleDeps

    , loadDependency
    , getDeps
    , recurse
    , recurseWithAPIs
    , tellCode

    , handleCGExc
    , describeCGError
    , notImplementedError
    , badIntroError
    , missingInfoError

    , indent
    , line
    , blank
    , group
    , submodule
    , export
    , setLanguagePragmas
    , setModuleFlags

    , findAPI
    , findAPIByName
    , getAPIs

    , config
    , currentModule

    -- From Data.Monoid, for convenience
    , (<>)
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(..))
#endif
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
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

import GI.API (API, Name(..))
import GI.Config (Config(..))
import GI.Type (Type(..))

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

-- | Information on a generated module.
data ModuleInfo = ModuleInfo {
      moduleName :: ModuleName -- ^ Full module name: ["GI", "Gtk", "Label"].
    , moduleCode :: Code       -- ^ Generated code for the module.
    , submodules :: M.Map Text ModuleInfo -- ^ Indexed by the relative
                                          -- module name.
    , moduleDeps :: Deps -- ^ Set of dependencies for this module.
    , moduleExports :: Set.Set Text -- ^ Set of exports for the current module.
    , modulePragmas :: Set.Set Text -- ^ Set of language pragmas for the module.
    , moduleFlags   :: Set.Set ModuleFlag -- ^ Flags for the module.
    }

-- | Flags for module code generation.
data ModuleFlag = ImplicitPrelude  -- ^ Use the standard prelude,
                                   -- instead of the haskell-gi-base short one.
                | NoTypesImport    -- ^ Do not import a "Types" submodule.
                  deriving (Show, Eq, Ord)

-- | Generate the empty module.
emptyModule :: ModuleName -> ModuleInfo
emptyModule m = ModuleInfo { moduleName = m
                           , moduleCode = NoCode
                           , submodules = M.empty
                           , moduleDeps = Set.empty
                           , moduleExports = Set.empty
                           , modulePragmas = Set.empty
                           , moduleFlags = Set.empty
                           }

-- | Information for the code generator.
data CodeGenConfig = CodeGenConfig {
      hConfig     :: Config        -- ^ Ambient config.
    , loadedAPIs :: M.Map Name API -- ^ APIs available to the generator.
    }

data CGError = CGErrorNotImplemented String
             | CGErrorBadIntrospectionInfo String
             | CGErrorMissingInfo String
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

-- | Run the given code generator using the state and config of an
-- ambient CodeGen, but without adding the generated code to
-- `moduleCode`, instead returning it explicitly.
recurseCG :: BaseCodeGen e a -> BaseCodeGen e (a, Code)
recurseCG cg = do
  cfg <- ask
  oldInfo <- get
  -- Start the subgenerator with no code and no submodules.
  let info = oldInfo { moduleCode = NoCode, submodules = M.empty }
  liftIO (runCodeGen cg cfg info) >>= \case
     Left e -> throwError e
     Right (r, new) -> put (mergeInfoState oldInfo new) >>
                       return (r, moduleCode new)

-- | Like `recurseCG`, but for generators returning a unit value, where
-- we can just drop the result.
recurse :: CodeGen () -> CodeGen Code
recurse cg = snd <$> recurseCG cg

-- | Like `recurse`, giving explicitly the set of loaded APIs for the
-- subgenerator.
recurseWithAPIs :: M.Map Name API -> CodeGen () -> CodeGen Code
recurseWithAPIs apis cg = do
  cfg <- ask
  oldInfo <- get
  -- Start the subgenerator with no code and no submodules.
  let info = oldInfo { moduleCode = NoCode, submodules = M.empty }
      cfg' = cfg {loadedAPIs = apis}
  liftIO (runCodeGen cg cfg' info) >>= \case
     Left e -> throwError e
     Right (_, new) -> put (mergeInfoState oldInfo new) >>
                       return (moduleCode new)

-- | Merge the dependencies, exports and submodules of the two given
-- `ModuleInfo`s (but not the generated code).
mergeInfoState :: ModuleInfo -> ModuleInfo -> ModuleInfo
mergeInfoState oldState newState =
    let newDeps = Set.union (moduleDeps oldState) (moduleDeps newState)
        newSubmodules = M.unionWith mergeInfo (submodules oldState) (submodules newState)
        newExports = Set.union (moduleExports oldState) (moduleExports newState)
        newPragmas = Set.union (modulePragmas oldState) (modulePragmas newState)
        newFlags = Set.union (moduleFlags oldState) (moduleFlags newState)
    in oldState {moduleDeps = newDeps, submodules = newSubmodules,
                 moduleExports = newExports, modulePragmas = newPragmas,
                 moduleFlags = newFlags }

-- | Merge the infos, including code too.
mergeInfo :: ModuleInfo -> ModuleInfo -> ModuleInfo
mergeInfo oldInfo newInfo =
    let info = mergeInfoState oldInfo newInfo
    in info {moduleCode = moduleCode oldInfo <> moduleCode newInfo}

-- | Add the given submodule to the list of submodules of the current
-- module.
addSubmodule :: Text -> ModuleInfo -> ModuleInfo -> ModuleInfo
addSubmodule modName submodule current = current { submodules = M.insertWith mergeInfo modName submodule (submodules current)}

-- | Run the given CodeGen in order to generate a submodule of the
-- current module.
submodule :: Text -> BaseCodeGen e () -> BaseCodeGen e ()
submodule modName cg = do
  cfg <- ask
  oldInfo <- get
  let info = emptyModule (moduleName oldInfo ++ [modName])
  liftIO (runCodeGen cg cfg info) >>= \case
         Left e -> throwError e
         Right (_, smInfo) -> modify' (addSubmodule modName smInfo)

-- | Try running the given `action`, and if it fails run `fallback`
-- instead.
handleCGExc :: (CGError -> CodeGen a) -> ExcCodeGen a -> CodeGen a
handleCGExc fallback
 action = do
    cfg <- ask
    oldInfo <- get
    let info = oldInfo { moduleCode = NoCode, submodules = M.empty }
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
loadDependency :: Text -> CodeGen ()
loadDependency name = do
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

-- | Give a friendly textual description of the error for presenting
-- to the user.
describeCGError :: CGError -> String
describeCGError (CGErrorNotImplemented e) = "Not implemented: " ++ show e
describeCGError (CGErrorBadIntrospectionInfo e) = "Bad introspection data: " ++ show e
describeCGError (CGErrorMissingInfo e) = "Missing info: " ++ show e

notImplementedError :: String -> ExcCodeGen a
notImplementedError s = throwError $ CGErrorNotImplemented s

badIntroError :: String -> ExcCodeGen a
badIntroError s = throwError $ CGErrorBadIntrospectionInfo s

missingInfoError :: String -> ExcCodeGen a
missingInfoError s = throwError $ CGErrorMissingInfo s

findAPI :: Type -> CodeGen (Maybe API)
findAPI TError = Just <$> findAPIByName (Name "GLib" "Error")
findAPI (TInterface ns n) = Just <$> findAPIByName (Name ns n)
findAPI _ = return Nothing

findAPIByName :: Name -> CodeGen API
findAPIByName n@(Name ns _) = do
    apis <- getAPIs
    case M.lookup n apis of
        Just api -> return api
        Nothing ->
            error $ "couldn't find API description for " ++ ns ++ "." ++ name n

-- | Add some code to the current generator.
tellCode :: Code -> CodeGen ()
tellCode c = modify' (\s -> s {moduleCode = moduleCode s <> c})

-- | Print out a (newline-terminated) line.
line :: String -> CodeGen ()
line = tellCode . Line . T.pack

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

-- | Add a export to the current module.
export :: String -> CodeGen ()
export e =
    modify' $ \s -> s{moduleExports = Set.insert (T.pack e) (moduleExports s)}

-- | Set the language pragmas for the current module.
setLanguagePragmas :: [Text] -> CodeGen ()
setLanguagePragmas ps =
    modify' $ \s -> s{modulePragmas = Set.fromList ps}

-- | Set the given flags for the module.
setModuleFlags :: [ModuleFlag] -> CodeGen ()
setModuleFlags flags =
    modify' $ \s -> s{moduleFlags = Set.fromList flags}

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

-- | Format the given export list (at the given indent level). This is
-- just the inside of the parenthesis, so the first term is treated
-- differently from the rest.
formatExportList :: [Text] -> Text
formatExportList [] = ""
formatExportList (f:rest) = T.concat $
                            f <> "\n" : (map (paddedLine 1 . (", " <>))) rest

-- | Write down the list of language pragmas.
languagePragmas :: [Text] -> Text
languagePragmas [] = ""
languagePragmas ps = "{-# LANGUAGE " <> T.intercalate ", " ps <> " #-}\n"

-- | Generic module prelude. We reexport all of the submodules.
modulePrelude :: Text -> [Text] -> [Text] -> Text
modulePrelude name [] [] = "module " <> name <> " () where\n"
modulePrelude name exports [] =
    "module " <> name <> "\n    ( "
    <> formatExportList exports
    <> "    ) where\n"
modulePrelude name [] reexportedModules =
    "module " <> name <> "\n    ( "
    <> formatExportList (map ("module " <>) reexportedModules)
    <> "    ) where\n\n"
    <> T.unlines (map ("import " <>) reexportedModules)
modulePrelude name exports reexportedModules =
    "module " <> name <> "\n    ( "
    <> formatExportList (map ("module " <>) reexportedModules)
    <> "\n    , "
    <> formatExportList exports
    <> "    ) where\n\n"
    <> T.unlines (map ("import " <>) reexportedModules)

-- | Code for loading the needed dependencies.
importDeps :: [Text] -> Text
importDeps [] = ""
importDeps deps = T.unlines . map toImport $ deps
    where toImport :: Text -> Text
          toImport dep = let ucFirst :: Text -> Text
                             ucFirst "" = error "importDeps: empty module name!"
                             ucFirst t = T.cons (toUpper $ T.head t) (T.tail t)
                             ucDep = ucFirst dep
                         in "import qualified GI." <> ucDep <> " as " <> ucDep

-- | Standard imports.
moduleImports :: Text
moduleImports = T.unlines [ "import Prelude ()"
                          , "import Data.GI.Base.ShortPrelude"
                          , ""
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
      fname = moduleNameToPath (moduleName minfo)
      dirname = takeDirectory fname
      code = codeToText (moduleCode minfo)
      pragmas = languagePragmas (Set.toList $ modulePragmas minfo)
      prelude = modulePrelude (dotModuleName $ moduleName minfo)
                (Set.toList $ moduleExports minfo)
                submoduleExports
      imports = if ImplicitPrelude `Set.member` moduleFlags minfo
                then ""
                else moduleImports
      deps = importDeps (Set.toList $ moduleDeps minfo)
      pkgRoot = take 2 (moduleName minfo)
      typesModule = pkgRoot ++ ["Types"]
      types = if moduleName minfo == typesModule ||
              NoTypesImport `Set.member` moduleFlags minfo
              then ""
              else "import " <> dotModuleName typesModule

  when verbose $ putStrLn ((T.unpack . dotModuleName . moduleName) minfo
                           ++ " -> " ++ fname)
  createDirectoryIfMissing True dirname
  TIO.writeFile fname (T.unlines [pragmas, prelude,
                                  imports, types, deps, code])

  where
    moduleNameToPath :: ModuleName -> FilePath
    moduleNameToPath mn = joinPath (fromMaybe "" dirPrefix : map T.unpack mn)
                          ++ ".hs"

-- | Turn an abstract module name into its dotted representation. For
-- instance, ["GI", "Gtk", "Types"] -> GI.Gtk.Types.
dotModuleName :: ModuleName -> Text
dotModuleName mn = T.intercalate "." mn

-- | Write down the code for a module to disk under the given base
-- directory. This also writes the submodules. It returns the list of
-- written modules.
writeModuleTree :: Bool -> Maybe FilePath -> ModuleInfo -> IO [Text]
writeModuleTree verbose dirPrefix minfo = do
  submoduleNames <- concat <$> forM (M.elems (submodules minfo))
                                    (writeModuleTree verbose dirPrefix)
  writeModuleInfo verbose dirPrefix minfo
  return $ (dotModuleName (moduleName minfo) : submoduleNames)
