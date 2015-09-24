{-# LANGUAGE OverloadedStrings #-}
module GI.SymbolNaming
    ( qualify
    , qualifyWithSuffix
    , ucFirst
    , lcFirst
    , lowerName
    , upperName
    , noName
    , escapeReserved
    , interfaceClassName
    , hyphensToCamelCase
    , underscoresToCamelCase
    ) where

import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))

import GI.API
import GI.Code
import GI.Config (Config(modName))
import GI.Util (split)

interfaceClassName :: String -> String
interfaceClassName = (++"Klass")

ucFirst :: String -> String
ucFirst (x:xs) = toUpper x : xs
ucFirst "" = error "ucFirst: empty string"

lcFirst :: String -> String
lcFirst (x:xs) = toLower x : xs
lcFirst "" = error "lcFirst: empty string"

lowerName :: Name -> CodeGen String
lowerName (Name _ s) = return $ concat . rename $ split '_' s
    where
      rename [w] = [lcFirst w]
      rename (w:ws) = lcFirst w : map ucFirst' ws
      rename [] = error "rename: empty list"

      ucFirst' "" = "_"
      ucFirst' x = ucFirst x

upperName :: Name -> CodeGen String
upperName (Name ns s) = do
          prefix <- qualify ns
          return $ prefix ++ uppered
    where uppered = concatMap ucFirst' $ split '_' $ sanitize s
          -- Move leading underscores to the end (for example in
          -- GObject::_Value_Data_Union -> GObject::Value_Data_Union_)
          sanitize ('_':xs) = sanitize xs ++ "_"
          sanitize xs = xs

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

-- Return a qualified prefix for the given namespace. In case the
-- namespace corresponds to the current module the empty string is
-- returned, otherwise the namespace ++ suffix is returned. Suffix is
-- typically just ".", see "qualify" below.
qualifyWithSuffix :: String -> String -> CodeGen String
qualifyWithSuffix suffix ns = do
     cfg <- config
     if modName cfg == Just ns then
         return ""
     else do
       loadDependency ns -- Make sure that the given namespace is listed
                         -- as a dependency of this module.
       return $ ucFirst ns ++ suffix

qualify:: String -> CodeGen String
qualify = qualifyWithSuffix "."

-- Save a bit of typing for optional arguments in the case that we
-- want to pass Nothing.
noName :: String -> CodeGen ()
noName name' = group $ do
                 line $ "no" ++ name' ++ " :: Maybe " ++ name'
                 line $ "no" ++ name' ++ " = Nothing"

-- | For a string of the form "one-sample-string" return "OneSampleString"
hyphensToCamelCase :: String -> String
hyphensToCamelCase str = concatMap ucFirst $ split '-' str

-- | Similarly, turn a name separated_by_underscores into CamelCase.
underscoresToCamelCase :: String -> String
underscoresToCamelCase str = concatMap ucFirst $ split '_' str

escapeReserved :: Text -> String
escapeReserved "type" = "type_"
escapeReserved "in" = "in_"
escapeReserved "data" = "data_"
escapeReserved "instance" = "instance_"
escapeReserved "where" = "where_"
escapeReserved "module" = "module_"
-- Reserved because we generate code that uses these names.
escapeReserved "result" = "result_"
escapeReserved "return" = "return_"
escapeReserved "show" = "show_"
escapeReserved "fromEnum" = "fromEnum_"
escapeReserved "toEnum" = "toEnum_"
escapeReserved "undefined" = "undefined_"
escapeReserved "error" = "error_"
escapeReserved "map" = "map_"
escapeReserved "length" = "length_"
escapeReserved "mapM" = "mapM__"
escapeReserved "mapM_" = "mapM___"
escapeReserved "fromIntegral" = "fromIntegral_"
escapeReserved "realToFrac" = "realToFrac_"
escapeReserved "peek" = "peek_"
escapeReserved "poke" = "poke_"
escapeReserved "sizeOf" = "sizeOf_"
escapeReserved "when" = "when_"
escapeReserved "default" = "default_"
escapeReserved s
    | "set_" `T.isPrefixOf` s = T.unpack s <> "_"
    | "get_" `T.isPrefixOf` s = T.unpack s <> "_"
    | otherwise = T.unpack s
