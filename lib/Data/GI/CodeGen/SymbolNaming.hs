{-# LANGUAGE ViewPatterns #-}
module Data.GI.CodeGen.SymbolNaming
    ( lowerName
    , lowerSymbol
    , upperName
    , noName
    , escapedArgName

    , classConstraint
    , typeConstraint

    , hyphensToCamelCase
    , underscoresToCamelCase

    , callbackCType
    , callbackHTypeWithClosures
    , callbackDropClosures
    , callbackDynamicWrapper
    , callbackWrapperAllocator
    , callbackHaskellToForeign
    , callbackHaskellToForeignWithClosures
    , callbackClosureGenerator

    , signalHaskellName
    , signalInfoName

    , submoduleLocation
    , qualifiedAPI
    , qualifiedSymbol
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code (CodeGen, group, line, exportDecl,
                             qualified, getAPI)
import Data.GI.CodeGen.ModulePath (ModulePath, (/.), toModulePath)
import Data.GI.CodeGen.Type (Type(TInterface))
import Data.GI.CodeGen.Util (lcFirst, ucFirst, modifyQualified)

-- | Return a qualified form of the constraint for the given name
-- (which should correspond to a valid `TInterface`).
classConstraint :: Name -> CodeGen Text
classConstraint n@(Name _ s) = qualifiedSymbol ("Is" <> s) n

-- | Same as `classConstraint`, but applicable directly to a type. The
-- type should be a `TInterface`, otherwise an error will be raised.
typeConstraint :: Type -> CodeGen Text
typeConstraint (TInterface n) = classConstraint n
typeConstraint t = error $ "Class constraint for non-interface type: " <> show t

-- | Foreign type associated with a callback type. It can be passed in
-- qualified.
callbackCType :: Text -> Text
callbackCType = modifyQualified ("C_" <>)

-- | Haskell type exposing the closure arguments, which are generally
-- elided.
callbackHTypeWithClosures :: Text -> Text
callbackHTypeWithClosures = modifyQualified (<> "_WithClosures")

-- | The name of the dynamic wrapper for the given callback type. It
-- can be passed in qualified.
callbackDynamicWrapper :: Text -> Text
callbackDynamicWrapper = modifyQualified ("dynamic_" <>)

-- | The name of the Haskell to foreign wrapper for the given callback
-- type. It can be passed in qualified.
callbackHaskellToForeign :: Text -> Text
callbackHaskellToForeign = modifyQualified ("wrap_" <>)

-- | The name of the Haskell to foreign wrapper for the given callback
-- type, keeping the closure arguments (we usually elide them). The
-- callback type can be passed in qualified.
callbackHaskellToForeignWithClosures :: Text -> Text
callbackHaskellToForeignWithClosures = modifyQualified ("with_closures_" <>)

-- | The name of a function which takes a callback without closure
-- arguments, and generates a function which does accep the closures,
-- but simply ignores them.
callbackDropClosures :: Text -> Text
callbackDropClosures = modifyQualified ("drop_closures_" <>)

-- | The name for the foreign wrapper allocator (@foreign import
-- "wrapper" ...@) for the given callback type. It can be passed in
-- qualified.
callbackWrapperAllocator :: Text -> Text
callbackWrapperAllocator = modifyQualified ("mk_" <>)

-- | The name for the closure generator for the given callback
-- type. It can be passed in qualified.
callbackClosureGenerator :: Text -> Text
callbackClosureGenerator = modifyQualified ("genClosure_" <>)

-- | Move leading underscores to the end.
--
-- === Examples
-- >>> sanitize "_Value_Data_Union"
-- "Value_Data_Union_"
sanitize :: Text -> Text
sanitize (T.uncons -> Just ('_', xs)) = sanitize xs <> "_"
sanitize xs = xs

-- | Same as `lowerSymbol`, but accepts a `Name`. The namespace part
-- of the name will be discarded.
--
-- === __Examples__
-- >>> lowerName (Name "Gtk" "main_quit")
-- "mainQuit"
lowerName :: Name -> Text
lowerName (Name _ s) = lowerSymbol s

-- | Turn the given identifier into camelCase, starting with a
-- lowercase letter.
--
-- === __Examples__
-- >>> lowerSymbol "main_quit"
-- "mainQuit"
lowerSymbol :: Text -> Text
lowerSymbol s = case underscoresToCamelCase (sanitize s) of
                  "" -> error "empty name!!"
                  n -> lcFirst n

-- | Turn the given `Name` into CamelCase, starting with a capital letter.
--
-- === __Examples__
-- >>> upperName (Name "Foo" "bar_baz")
-- "BarBaz"
upperName :: Name -> Text
upperName (Name _ s) = underscoresToCamelCase (sanitize s)

-- | Construct the submodule path where the given API element will
-- live. This is the path relative to the root for the corresponding
-- namespace. I.e. the "GI.Gtk" part is not prepended.
submoduleLocation :: Name -> API -> ModulePath
submoduleLocation _ (APIConst _) = "Constants"
submoduleLocation _ (APIFunction _) = "Functions"
submoduleLocation _ (APICallback _) = "Callbacks"
submoduleLocation _ (APIEnum _) = "Enums"
submoduleLocation _ (APIFlags _) = "Flags"
submoduleLocation n (APIInterface _) = "Interfaces" /. upperName n
submoduleLocation n (APIObject _) = "Objects" /. upperName n
submoduleLocation n (APIStruct _) = "Structs" /. upperName n
submoduleLocation n (APIUnion _) = "Unions" /. upperName n

-- | Return an identifier for the given interface type valid in the current
-- module.
qualifiedAPI :: Name -> CodeGen Text
qualifiedAPI n@(Name ns _) = do
  api <- getAPI (TInterface n)
  qualified (toModulePath (ucFirst ns) <> submoduleLocation n api) n

-- | Construct an identifier for the given symbol in the given API.
qualifiedSymbol :: Text -> Name -> CodeGen Text
qualifiedSymbol s n@(Name ns _) = do
  api <- getAPI (TInterface n)
  qualified (toModulePath (ucFirst ns) <> submoduleLocation n api) (Name ns s)

-- | Save a bit of typing for optional arguments in the case that we
-- want to pass Nothing.
noName :: Text -> CodeGen ()
noName name' = group $ do
  -- We should use `writeHaddock` here, but it would give rise to a
  -- cyclic import.
  line $ "-- | A convenience alias for `Nothing` :: `Maybe` `" <> name' <> "`."
  line $ "no" <> name' <> " :: Maybe " <> name'
  line $ "no" <> name' <> " = Nothing"
  exportDecl ("no" <> name')

-- | Turn a hyphen-separated identifier into camel case.
--
-- === __Examples__
-- >>> hyphensToCamelCase "one-sample-string"
-- "OneSampleString"
hyphensToCamelCase :: Text -> Text
hyphensToCamelCase = T.concat . map ucFirst . T.split (== '-')

-- | Similarly to `hyphensToCamelCase`, turn a name
-- separated_by_underscores into CamelCase. We preserve final and
-- initial underscores, and n>1 consecutive underscores are
-- transformed into n-1 underscores.
--
-- === __Examples__
-- >>> underscoresToCamelCase "sample_id"
-- "SampleId"
--
-- >>> underscoresToCamelCase "_internal_id_"
-- "_InternalId_"
--
-- >>> underscoresToCamelCase "multiple___underscores"
-- "Multiple__Underscores"
underscoresToCamelCase :: Text -> Text
underscoresToCamelCase =
    T.concat . map normalize . map ucFirst . T.split (== '_')
        where normalize :: Text -> Text
              normalize "" = "_"
              normalize s = s

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

-- | Qualified name for the "(sigName, info)" tag for a given signal.
signalInfoName :: Name -> Signal -> CodeGen Text
signalInfoName n signal = do
  let infoName = upperName n <> (ucFirst . signalHaskellName . sigName) signal
                 <> "SignalInfo"
  qualifiedSymbol infoName n

-- | Return the name for the signal in Haskell CamelCase conventions.
signalHaskellName :: Text -> Text
signalHaskellName sn = let (w:ws) = T.split (== '-') sn
                       in w <> T.concat (map ucFirst ws)
