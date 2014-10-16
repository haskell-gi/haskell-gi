module GI.Struct (genStructFields)
    where

import Control.Monad (forM, forM_, when)
import Control.Applicative ((<$>))
import Control.Monad.Writer (tell)
import Data.List (intercalate, isSuffixOf)
import Data.Tuple (swap)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M
import qualified Data.Set as S

import GI.API
import GI.Conversions
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Type
import GI.Util
import GI.Value
import GI.Internal.ArgInfo
import GI.Internal.FunctionInfo
import GI.Internal.TypeInfo

genStructFields :: Name -> Struct -> CodeGen ()
genStructFields n@(Name ns _) s = do
  cfg <- config
  name' <- upperName n

  forM_ (structFields s) $ \field -> group $ do
    when (not . isJust . fieldCallback $ field) $ do
     hType <- show <$> haskellType (fieldType field)
     fType <- show <$> foreignType (fieldType field)
     when (not $ "Private" `isSuffixOf` hType) $ do
        fName <- upperName $ Name ns (fieldName field)
        let getter = lcFirst name' ++ "Read" ++ fName
        line $ getter ++ " :: " ++ name' ++ " -> IO " ++
                    if ' ' `elem` hType
                    then parenthesize hType
                    else hType
        line $ getter ++ " s = withManagedPtr s $ \\ptr -> do"
        indent $ do
          line $ "val <- peek (ptr `plusPtr` " ++ show (fieldOffset field)
               ++ ") :: IO " ++ if ' ' `elem` fType
                               then parenthesize fType
                               else fType
          result <- convert "val" $ fToH (fieldType field) TransferNothing
          line $ "return " ++ result
