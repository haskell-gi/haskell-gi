
module GI.Code
    ( Code(..)
    , CodeGen
    , Config(..)
    , runCodeGen
    , runCodeGen'
    , codeToString
    , codeToList
    , indent
    , line
    , blank
    , group
    , foreignImport
    , findAPI
    , config
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Sequence (Seq, ViewL ((:<)), (><), (|>), (<|))
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as S

import GI.API (API, Name(..))
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
  imports :: [String],
  prefixes :: M.Map String String,
  names :: M.Map String String,
  modName :: String,
  instances :: M.Map Name Name,
  ignoredMethods :: [String],
  -- XXX: Blegh.
  input :: M.Map Name API }

type CodeGen = WriterT Code (Reader Config)

runCodeGen :: Config -> CodeGen a -> (a, Code)
runCodeGen config = flip runReader config . runWriterT

runCodeGen' :: Config -> CodeGen () -> Code
runCodeGen' cfg = snd . runCodeGen cfg

recurse :: CodeGen a -> CodeGen (a, Code)
recurse cg = do
    cfg <- config
    return $ runCodeGen cfg cg

findAPI :: Type -> CodeGen (Maybe API)
findAPI (TInterface ns n) = do
    cfg <- config
    case M.lookup (Name ns n) (input cfg) of
      Just api -> return $ Just api
      Nothing -> error $
                 "couldn't find API description for type " ++
                 ns ++ "." ++ n
findAPI _ = return Nothing

line :: String -> CodeGen ()
line = tell . Line

blank = line ""

config :: CodeGen Config
config = lift ask

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

