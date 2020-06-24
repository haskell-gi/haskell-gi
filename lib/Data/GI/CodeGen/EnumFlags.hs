-- | Support for enums and flags.
module Data.GI.CodeGen.EnumFlags
    ( genEnum
    , genFlags
    ) where

import Control.Monad (when, forM_)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Set as S

import Foreign.C (CUInt)
import Foreign.Storable (sizeOf)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Haddock (deprecatedPragma, writeDocumentation,
                                writeHaddock, RelativeDocPosition(..))
import Data.GI.CodeGen.SymbolNaming (upperName)
import Data.GI.CodeGen.Util (tshow)

-- | Given a list of named enum members, filter out those that have
-- the same value as a previous entry in the list.
dropDuplicated :: [(Text, EnumerationMember)] -> [(Text, EnumerationMember)]
dropDuplicated namedMembers = go namedMembers enumMemberValue S.empty
  where go :: Ord c => [(a, b)] -> (b->c) -> S.Set c -> [(a, b)]
        go [] _ _ = []
        go ((n, m) : rest) f seen =
          if S.member (f m) seen
             -- already seen, discard
          then go rest f seen
          else (n,m) : go rest f (S.insert (f m) seen)


genEnumOrFlags :: HaddockSection -> Name -> Enumeration -> ExcCodeGen ()
genEnumOrFlags docSection n@(Name ns name) e = do
  -- Conversion functions expect enums and flags to map to CUInt,
  -- which we assume to be of 32 bits. Fail early, instead of giving
  -- strange errors at runtime.
  when (sizeOf (0 :: CUInt) /= 4) $
       notImplementedError $ "Unsupported CUInt size: " <> tshow (sizeOf (0 :: CUInt))
  when (enumStorageBytes e /= 4) $
       notImplementedError $ "Storage of size /= 4 not supported : " <> tshow (enumStorageBytes e)

  let name' = upperName n
      members' = flip map (enumMembers e) $ \member ->
        let n = upperName $ Name ns (name <> "_" <> enumMemberName member)
        in (n, member)

  deprecatedPragma name' (enumDeprecated e)

  group $ do
    export docSection (name' <> "(..)")
    hsBoot . line $ "data " <> name'
    writeDocumentation DocBeforeSymbol (enumDocumentation e)
    line $ "data " <> name' <> " = "
    indent $
      case members' of
        ((fieldName, firstMember):fs) -> do
          line $ "  " <> fieldName
          writeDocumentation DocAfterSymbol (enumMemberDoc firstMember)
          forM_ fs $ \(n, member) -> do
            line $ "| " <> n
            writeDocumentation DocAfterSymbol (enumMemberDoc member)
          line $ "| Another" <> name' <> " Int"
          writeHaddock DocAfterSymbol "Catch-all for unknown values"
          line "deriving (Show, Eq)"
        _ -> return ()

  group $ do
    bline $ "instance P.Enum " <> name' <> " where"
    indent $ do
            forM_ members' $ \(n, m) ->
                line $ "fromEnum " <> n <> " = " <> tshow (enumMemberValue m)
            line $ "fromEnum (Another" <> name' <> " k) = k"
    blank
    indent $ do
            forM_ (dropDuplicated members') $ \(n, m) ->
                line $ "toEnum " <> tshow (enumMemberValue m) <> " = " <> n
            line $ "toEnum k = Another" <> name' <> " k"

  group $ do
    line $ "instance P.Ord " <> name' <> " where"
    indent $ line "compare a b = P.compare (P.fromEnum a) (P.fromEnum b)"

  maybe (return ()) (genErrorDomain docSection name') (enumErrorDomain e)

genBoxedEnum :: Name -> Text -> CodeGen ()
genBoxedEnum n typeInit = do
  let name' = upperName n

  group $ do
    line $ "type instance O.ParentTypes " <> name' <> " = '[]"
    bline $ "instance O.HasParentTypes " <> name'

  group $ do
    line $ "foreign import ccall \"" <> typeInit <> "\" c_" <>
            typeInit <> " :: "
    indent $ line "IO GType"
  group $ do
       bline $ "instance B.Types.TypedObject " <> name' <> " where"
       indent $ line $ "glibType = c_" <> typeInit

  group $ do
    bline $ "instance B.Types.BoxedEnum " <> name'

genEnum :: Name -> Enumeration -> CodeGen ()
genEnum n@(Name _ name) enum = do
  line $ "-- Enum " <> name

  let docSection = NamedSubsection EnumSection (upperName n)
  handleCGExc (\e -> do
                  line $ "-- XXX Code Generation error"
                  printCGError e)
              (do genEnumOrFlags docSection n enum
                  case enumTypeInit enum of
                    Nothing -> return ()
                    Just ti -> genBoxedEnum n ti)

genBoxedFlags :: Name -> Text -> CodeGen ()
genBoxedFlags n typeInit = do
  let name' = upperName n

  group $ do
    line $ "type instance O.ParentTypes " <> name' <> " = '[]"
    bline $ "instance O.HasParentTypes " <> name'

  group $ do
    line $ "foreign import ccall \"" <> typeInit <> "\" c_" <>
            typeInit <> " :: "
    indent $ line "IO GType"
  group $ do
       bline $ "instance B.Types.TypedObject " <> name' <> " where"
       indent $ line $ "glibType = c_" <> typeInit

  group $ do
    bline $ "instance B.Types.BoxedFlags " <> name'

-- | Very similar to enums, but we also declare ourselves as members of
-- the IsGFlag typeclass.
genFlags :: Name -> Flags -> CodeGen ()
genFlags n@(Name _ name) (Flags enum) = do
  line $ "-- Flags " <> name

  let docSection = NamedSubsection FlagSection (upperName n)
  handleCGExc (\e -> do
                  line "-- XXX Code generation error"
                  printCGError e)
              (do
                genEnumOrFlags docSection n enum

                case enumTypeInit enum of
                  Nothing -> return ()
                  Just ti -> genBoxedFlags n ti

                let name' = upperName n
                group $ bline $ "instance IsGFlag " <> name')

-- | Support for enums encapsulating error codes.
genErrorDomain :: HaddockSection -> Text -> Text -> CodeGen ()
genErrorDomain docSection name' domain = do
  group $ do
    line $ "instance GErrorClass " <> name' <> " where"
    indent $ line $
               "gerrorClassDomain _ = \"" <> domain <> "\""
  -- Generate type specific error handling (saves a bit of typing, and
  -- it's clearer to read).
  group $ do
    let catcher = "catch" <> name'
    writeHaddock DocBeforeSymbol catcherDoc
    line $ catcher <> " ::"
    indent $ do
            line   "IO a ->"
            line $ "(" <> name' <> " -> GErrorMessage -> IO a) ->"
            line   "IO a"
    line $ catcher <> " = catchGErrorJustDomain"

  group $ do
    let handler = "handle" <> name'
    writeHaddock DocBeforeSymbol handleDoc
    line $ handler <> " ::"
    indent $ do
            line $ "(" <> name' <> " -> GErrorMessage -> IO a) ->"
            line   "IO a ->"
            line   "IO a"
    line $ handler <> " = handleGErrorJustDomain"
  export docSection ("catch" <> name')
  export docSection ("handle" <> name')

  where
    catcherDoc :: Text
    catcherDoc = "Catch exceptions of type `" <> name' <> "`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`."

    handleDoc :: Text
    handleDoc = "Handle exceptions of type `" <> name' <> "`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`."
