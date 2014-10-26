
module GI.Code
    ( Code(..)
    , CodeGen
    , Config(..)
    , genCode
    , codeToString
    , codeToList
    , loadDependency
    , getDeps
    , getAPIs
    , injectAPIs
    , recurse
    , recurse'
    , indent
    , line
    , blank
    , group
    , foreignImport
    , findAPI
    , findAPIByName
    , config
    ) where

import Control.Monad.RWS
import Data.Sequence (Seq, ViewL ((:<)), (><), (|>), (<|))
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set

import Control.Applicative ((<$>))

import GI.API (API, Name(..), loadAPI)
import GI.Type (Type(..))

data Code
    = NoCode
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

data Config = Config {
      prefixes :: M.Map String String,
      names :: M.Map String String,
      modName :: String,
      ignoredMethods :: [String],
      sealedStructs :: [Name]
    }

type Deps = Set.Set String
data CodeGenState = CodeGenState {
      moduleDeps :: Deps,
      loadedAPIs :: M.Map Name API }
type CodeGen = RWST Config Code CodeGenState IO

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

mergeState :: CodeGenState -> CodeGen ()
mergeState newState = do
  oldState <- get
  -- If no dependencies were added we do not need to merge, this saves
  -- quite a bit of work.
  when (Set.size (moduleDeps oldState) /= Set.size (moduleDeps newState)) $ do
    let newDeps = Set.union (moduleDeps oldState) (moduleDeps newState)
        newAPIs = M.union (loadedAPIs oldState) (loadedAPIs newState)
    put $ CodeGenState {moduleDeps = newDeps, loadedAPIs = newAPIs}

runCodeGen :: Config -> CodeGenState -> CodeGen a -> IO (a, CodeGenState, Code)
runCodeGen cfg state f = runRWST f cfg state

genCode :: Config -> CodeGen () -> IO (Deps, Code)
genCode cfg f = do
  (_, st, code) <- runCodeGen cfg emptyState f
  return (moduleDeps st, code)

recurse :: CodeGen a -> CodeGen (a, Code)
recurse cg = do
    cfg <- config
    state <- get
    (result, st, code) <- liftIO $ runCodeGen cfg state cg
    mergeState st
    return (result, code)

recurse' :: CodeGen () -> CodeGen Code
recurse' cg = snd <$> recurse cg

loadDependency :: String -> CodeGen ()
loadDependency name = do
  deps <- getDeps
  when (not $ Set.member name deps) $
       do
         apis <- getAPIs
         imported <- M.fromList <$> liftIO (loadAPI name)
         let newDeps = Set.insert name deps
             newAPIs = M.union apis imported
         put $ CodeGenState {moduleDeps = newDeps, loadedAPIs = newAPIs}

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
      else error $ "couldn't find API description for "
                       ++ ns ++ "." ++ name n

line :: String -> CodeGen ()
line = tell . Line

blank = line ""

config :: CodeGen Config
config = ask

indent :: CodeGen a -> CodeGen a
indent cg = do
  (x, code) <- recurse cg
  tell $ Indent code
  return x

group :: CodeGen a -> CodeGen a
group cg = do
  (x, code) <- recurse cg
  tell $ Group code
  return x

codeToString c = concatMap (++ "\n") $ str 0 c []
    where str _ NoCode cont = cont
          str n (Line s) cont = (replicate (n * 4) ' ' ++ s) : cont
          str n (Indent c) cont = str (n + 1) c cont
          str n (ForeignImport c) cont = str n c cont
          str n (Sequence s) cont = deseq n (S.viewl s) cont
          -- str n (Sequence s) cont = F.foldr (\code rest -> str n code : rest) cont s
          str n (Group c) cont = str n c cont

          deseq _ S.EmptyL cont = cont
          deseq n (c :< cs) cont = str n c (deseq n (S.viewl cs) cont)

codeToList c = list c []
    where list NoCode cont = cont
          list (Sequence s) cont = F.foldr (:) cont s
          list c cont = c : cont

foreignImport :: CodeGen a -> CodeGen a
foreignImport cg = do
  (a, c) <- recurse cg
  tell $ ForeignImport c
  return a
