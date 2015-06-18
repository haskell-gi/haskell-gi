module GI.Code
    ( Code(..)
    , BaseCodeGen
    , CodeGen
    , ExcCodeGen
    , CGError(..)
    , genCode
    , evalCodeGen
    , codeToString
    , loadDependency
    , getDeps
    , getAPIs
    , injectAPIs
    , recurse
    , recurse'
    , handleCGExc
    , describeCGError
    , notImplementedError
    , badIntroError
    , missingInfoError
    , indent
    , line
    , blank
    , group
    , foreignImport
    , deprecatedPragma
    , findAPI
    , findAPIByName
    , config
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad.RWS
import Control.Monad.Except
import Data.Sequence (Seq, ViewL ((:<)), (><), (|>), (<|))
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set

import GI.API (API, Name(..))
import GI.Config (Config(..))
import GI.Overrides (loadFilteredAPI)
import GI.Type (Type(..))

data Code = NoCode
          | Line String
          | Indent Code
          | Sequence (Seq Code)
          | ForeignImport Code
          | Group Code
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

type Deps = Set.Set String
data CodeGenState = CodeGenState { moduleDeps :: Deps
                                 , loadedAPIs :: M.Map Name API }

data CGError = CGErrorNotImplemented String
             | CGErrorBadIntrospectionInfo String
             | CGErrorMissingInfo String
             deriving (Show)

type BaseCodeGen excType a =
    RWST Config Code CodeGenState (ExceptT excType IO) a

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

-- | Due to the `forall` in the definition of `CodeGen`, if we want to
-- run the monad transformer stack until we get an `IO` action, our
-- only option is ignoring the possible error code from
-- `runExceptT`. This is perfectly safe, since there is no way to
-- construct a computation in the `CodeGen` monad that throws an
-- exception, due to the higher rank type.
unwrapCodeGen :: Config -> CodeGenState -> CodeGen a ->
                 IO (a, CodeGenState, Code)
unwrapCodeGen cfg state cg =
    runExceptT (runRWST cg cfg state) >>= \case
        Left _ -> error "unwrapCodeGen:: The impossible happened!"
        Right (r, s, c) -> return (r, s, c)

-- | Run the given code generator, merging its resulting state into
-- the ambient state, and turning its output into a value.
recurse :: BaseCodeGen e a -> BaseCodeGen e (a, Code)
recurse cg = do
    cfg <- config
    oldState <- get
    liftIO (runExceptT $ runRWST cg cfg oldState) >>= \case
        Left e -> throwError e
        Right (r, st, c) -> put (mergeState oldState st) >> return (r, c)

-- | Try running the given `action`, and if it fails run `fallback`
-- instead.
handleCGExc :: (CGError -> CodeGen a) -> ExcCodeGen a -> CodeGen a
handleCGExc fallback action = do
    cfg <- config
    oldState <- get
    liftIO (runExceptT $ runRWST action cfg oldState) >>= \case
        Left e -> fallback e
        Right (r, s, c) -> do
            put $ mergeState oldState s
            tell c
            return r

emptyState :: CodeGenState
emptyState = CodeGenState {moduleDeps = Set.empty, loadedAPIs = M.empty}

getDeps :: CodeGen Deps
getDeps = moduleDeps <$> get

getAPIs :: CodeGen (M.Map Name API)
getAPIs = loadedAPIs <$> get

-- | Inject the given APIs into loaded set.
injectAPIs :: [(Name, API)] -> CodeGen()
injectAPIs newAPIs = do
    oldState <- get
    put $ oldState {loadedAPIs =
                    M.union (loadedAPIs oldState) (M.fromList newAPIs)}

-- | Merge two states of a code generator.
mergeState :: CodeGenState -> CodeGenState -> CodeGenState
mergeState oldState newState =
    -- If no dependencies were added we do not need to merge, this saves
    -- quite a bit of work.
    if Set.size (moduleDeps oldState) /= Set.size (moduleDeps newState)
    then let newDeps = Set.union (moduleDeps oldState) (moduleDeps newState)
             newAPIs = M.union (loadedAPIs oldState) (loadedAPIs newState)
         in CodeGenState {moduleDeps = newDeps, loadedAPIs = newAPIs}
    else oldState

-- | Run a code generator, and return the dependencies encountered
-- when generating code.
genCode :: Config -> CodeGen () -> IO (Deps, Code)
genCode cfg cg = do
    (_, st, code) <- unwrapCodeGen cfg emptyState cg
    return (moduleDeps st, code)

-- | Like `genCode`, but keep the final value and output, discarding
-- the state.
evalCodeGen :: Config -> CodeGen a -> IO (a, Code)
evalCodeGen cfg cg = do
  (r, _, code) <- unwrapCodeGen cfg emptyState cg
  return (r, code)

-- | Like `recurse`, but for generators returning a unit value, where
-- we can just drop the result.
recurse' :: CodeGen () -> CodeGen Code
recurse' cg = snd <$> recurse cg

loadDependency :: String -> CodeGen ()
loadDependency name = do
    deps <- getDeps
    unless (Set.member name deps) $ do
        apis <- getAPIs
        cfg <- config
        imported <- M.fromList <$>
                    liftIO (loadFilteredAPI (verbose cfg) (overrides cfg) name)
        let newDeps = Set.insert name deps
            newAPIs = M.union apis imported
        put CodeGenState {moduleDeps = newDeps, loadedAPIs = newAPIs}

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
        Nothing -> do
            deps <- getDeps
            if not (Set.member ns deps)
            -- If we get asked for a module not yet loaded, load it and retry.
            then do
                loadDependency ns
                findAPIByName n
            else error $ "couldn't find API description for " ++ ns ++ "." ++ name n

config :: CodeGen Config
config = ask

line :: String -> CodeGen ()
line = tell . Line

blank :: CodeGen ()
blank = line ""

indent :: BaseCodeGen e a -> BaseCodeGen e a
indent cg = do
    (x, code) <- recurse cg
    tell $ Indent code
    return x

group :: BaseCodeGen e a -> BaseCodeGen e a
group cg = do
    (x, code) <- recurse cg
    tell $ Group code
    blank
    return x

foreignImport :: BaseCodeGen e a -> BaseCodeGen e a
foreignImport cg = do
    (a, c) <- recurse cg
    tell $ ForeignImport c
    return a

deprecatedPragma :: Bool -> String -> CodeGen ()
deprecatedPragma isDeprecated name =
    when isDeprecated $ line $ "{-# DEPRECATED " ++ name ++ " \"\" #-}"

codeToString c = unlines $ str 0 c []
    where str _ NoCode cont = cont
          str n (Line s) cont = (replicate (n * 4) ' ' ++ s) : cont
          str n (Indent c) cont = str (n + 1) c cont
          str n (ForeignImport c) cont = str n c cont
          str n (Sequence s) cont = deseq n (S.viewl s) cont
          -- str n (Sequence s) cont = F.foldr (\code rest -> str n code : rest) cont s
          str n (Group c) cont = str n c cont

          deseq _ S.EmptyL cont = cont
          deseq n (c :< cs) cont = str n c (deseq n (S.viewl cs) cont)
