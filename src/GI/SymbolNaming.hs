{-# LANGUAGE ViewPatterns #-}
module GI.SymbolNaming
    ( qualify
    , lowerName
    , upperName
    , noName
    , escapedArgName
    , classConstraint
    , hyphensToCamelCase
    , underscoresToCamelCase
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif
import Data.Text (Text)
import qualified Data.Text as T

import GI.API
import GI.Code
import GI.Config (Config(modName))
import GI.Util (lcFirst, ucFirst)

classConstraint :: Text -> Text
classConstraint n = n <> "K"

lowerName :: Name -> CodeGen Text
lowerName (Name _ s) = return . T.concat . rename . T.split (== '_') $ s
    where
      rename [w] = [lcFirst w]
      rename (w:ws) = lcFirst w : map ucFirst' ws
      rename [] = error "rename: empty list"

      ucFirst' "" = "_"
      ucFirst' x = ucFirst x

upperNameWithSuffix :: Text -> Name -> CodeGen Text
upperNameWithSuffix suffix (Name ns s) = do
          prefix <- qualifyWithSuffix suffix ns
          return $ prefix <> uppered
    where uppered = T.concat . map ucFirst' . T.split (== '_') $ sanitize s
          -- Move leading underscores to the end (for example in
          -- GObject::_Value_Data_Union -> GObject::Value_Data_Union_)
          sanitize (T.uncons -> Just ('_', xs)) = sanitize xs <> "_"
          sanitize xs = xs

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

upperName :: Name -> CodeGen Text
upperName = upperNameWithSuffix "."

-- | Return a qualified prefix for the given namespace. In case the
-- namespace corresponds to the current module the empty string is
-- returned, otherwise the namespace ++ suffix is returned. Suffix is
-- typically just ".", see `qualify` below.
qualifyWithSuffix :: Text -> Text -> CodeGen Text
qualifyWithSuffix suffix ns = do
     cfg <- config
     if modName cfg == Just ns then
         return ""
     else do
       loadDependency ns -- Make sure that the given namespace is
                         -- listed as a dependency of this module.
       return $ ucFirst ns <> suffix

-- | Return the qualified namespace (ns ++ "." or "", depending on
-- whether ns is the current namespace).
qualify :: Text -> CodeGen Text
qualify = qualifyWithSuffix "."

-- | Save a bit of typing for optional arguments in the case that we
-- want to pass Nothing.
noName :: Text -> CodeGen ()
noName name' = group $ do
                 line $ "no" <> name' <> " :: Maybe " <> name'
                 line $ "no" <> name' <> " = Nothing"
                 exportDecl ("no" <> name')

-- | For a string of the form "one-sample-string" return "OneSampleString"
hyphensToCamelCase :: Text -> Text
hyphensToCamelCase = T.concat . map ucFirst . T.split (== '-')

-- | Similarly, turn a name separated_by_underscores into CamelCase.
underscoresToCamelCase :: Text -> Text
underscoresToCamelCase = T.concat . map ucFirst . T.split (== '_')

-- | Name for the given argument, making sure it is a valid Haskell
-- argument name (and escaping it if not).
escapedArgName :: Arg -> Text
escapedArgName arg
    | "_" `T.isPrefixOf` argCName arg = argCName arg
    | otherwise =
        escapeReserved . lcFirst . underscoresToCamelCase . argCName $ arg

-- | Reserved symbols, either because they are Haskell syntax or
-- because the clash with symbols in scope for the generated bindings.
escapeReserved :: Text -> Text
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
    | "set_" `T.isPrefixOf` s = s <> "_"
    | "get_" `T.isPrefixOf` s = s <> "_"
    | otherwise = s
