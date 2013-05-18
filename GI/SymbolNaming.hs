module GI.SymbolNaming
    ( nsHaskellType
    , nsForeignType
    , qualify
    , ucFirst
    , lcFirst
    , literalName
    , lowerName
    , upperName
    , escapeReserved
    , interfaceClassName
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Char (toLower, toUpper)
import Data.Typeable (TypeRep)
import qualified Data.Map as M

import GI.API
import GI.Code
import GI.Type
import GI.Util (split)

interfaceClassName = (++"Iface_")

ucFirst (x:xs) = toUpper x : xs
ucFirst "" = error "ucFirst: empty string"

lcFirst (x:xs) = toLower x : xs
lcFirst "" = error "lcFirst: empty string"

getPrefix :: String -> CodeGen String
getPrefix ns = do
    cfg <- config
    case M.lookup ns (prefixes cfg) of
        Just p -> return p
        Nothing -> return ns

specifiedName s fallback = do
    cfg <- config

    case M.lookup s (names cfg) of
        Just s' -> return s'
        Nothing -> fallback

literalName (Name ns s) = specifiedName s lit
    where lit = do
              prefix <- getPrefix ns
              return $ lcFirst prefix ++ "_" ++ s

lowerName (Name _ s) = specifiedName s lowered
    where lowered = return $ concat . rename $ split '_' s

          rename [w] = [lcFirst w]
          rename (w:ws) = lcFirst w : map ucFirst' ws
          rename [] = error "rename: empty list"

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

upperName (Name ns s) = do
          name <- specifiedName s uppered
          prefix <- qualify ns
          return $ prefix ++ name
    where uppered = return $ concatMap ucFirst' $ split '_' $ sanitize s
          -- Move leading underscores to the end (for example in
          -- GObject::_Value_Data_Union -> GObject::Value_Data_Union_)
          sanitize ('_':xs) = sanitize xs ++ "_"
          sanitize xs = xs

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

-- Return a qualified prefix for the given namespace. In case the
-- namespace corresponds to the current module the empty string is returned.
qualify :: String -> CodeGen String
qualify ns = do
     cfg <- config
     return $ if modName cfg == ns then
                ""
              else
                ucFirst ns ++ "."

mapPrefixes :: Type -> CodeGen Type
mapPrefixes t@(TBasicType _) = return t
mapPrefixes (TArray t) = TArray <$> mapPrefixes t
mapPrefixes (TGList t) = TGList <$> mapPrefixes t
mapPrefixes (TGSList t) = TGSList <$> mapPrefixes t
mapPrefixes (TGHash ta tb) =
  TGHash <$> mapPrefixes ta <*> mapPrefixes tb
mapPrefixes t@TError = return t
mapPrefixes (TInterface ns s) = do
    -- We qualify symbols with their namespace, unless they are in the
    -- current module.
    prefix <- qualify ns
    return $ TInterface undefined $ prefix ++ s

nsHaskellType :: Type -> CodeGen TypeRep
nsHaskellType t = haskellType <$> mapPrefixes t

nsForeignType :: Type -> CodeGen TypeRep
nsForeignType t = do
  isScalar <- getIsScalar
  if isScalar
     -- Enum and flag values are represented by machine words.
    then return $ "Word" `con` []
    else foreignType <$> mapPrefixes t

  where getIsScalar = do
          a <- findAPI t
          case a of
            Nothing -> return False
            (Just (APIEnum _)) -> return True
            (Just (APIFlags _)) -> return True
            _ -> return False

escapeReserved "type" = "type_"
escapeReserved "in" = "in_"
escapeReserved "data" = "data_"
escapeReserved "instance" = "instance_"
escapeReserved "where" = "where_"
-- Reserved because we generate code that uses this name.
escapeReserved "result" = "result_"
escapeReserved s = s