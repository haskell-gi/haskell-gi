module GI.SymbolNaming
    ( qualify
    , ucFirst
    , lcFirst
    , literalName
    , lowerName
    , upperName
    , escapeReserved
    , interfaceClassName
    ) where

import Data.Char (toLower, toUpper)
import qualified Data.Map as M

import GI.API
import GI.Code
import GI.Util (split)

interfaceClassName = (++"IKlass")

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

escapeReserved "type" = "type_"
escapeReserved "in" = "in_"
escapeReserved "data" = "data_"
escapeReserved "instance" = "instance_"
escapeReserved "where" = "where_"
-- Reserved because we generate code that uses these names.
escapeReserved "result" = "result_"
escapeReserved "length" = "length_"
escapeReserved s = s
