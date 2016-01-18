module GI.Constant
    ( genConstant
    ) where

import qualified Data.Text as T
import Data.Text (Text)

import GI.API
import GI.Code
import GI.Conversions
import GI.Type

-- | Data for a bidrectional pattern synonym. It is either a simple
-- one of the form "pattern Name = value :: Type" or an explicit one
-- of the form
-- > pattern Name <- (view -> value) :: Type where
-- >    Name = expression value :: Type
data PatternSynonym = SimpleSynonym PSValue PSType
                    | ExplicitSynonym PSView PSExpression PSValue PSType

-- Some simple types for legibility
type PSValue = Text
type PSType = Text
type PSView = Text
type PSExpression = Text

writePattern :: Text -> PatternSynonym -> CodeGen ()
writePattern name (SimpleSynonym value t) = line . T.unpack $
      "pattern " <> name <> " = " <> value <> " :: " <> t
writePattern name (ExplicitSynonym view expression value t) = do
    line . T.unpack $
      "pattern " <> name <> " <- (" <> view <> " -> " <> value <> ") :: " <> t <> " where"
    indent $ line . T.unpack $
          name <> " = " <> expression <> " " <> value <> " :: " <> t

genConstant :: Name -> Constant -> CodeGen ()
genConstant (Name _ name) (Constant t value deprecated) =
    submodule "Constants" $ group $ do
      setLanguagePragmas ["PatternSynonyms", "ScopedTypeVariables",
                          "ViewPatterns"]
      line $ deprecatedPragma name deprecated

      handleCGExc (\e -> line $ "-- XXX: Could not generate constant: " ++ describeCGError e)
                  (assignValue name t value >>
                   export ("pattern " <> name))

-- | Assign to the given name the given constant value, in a way that
-- can be assigned to the corresponding Haskell type.
assignValue :: String -> Type -> Text -> ExcCodeGen ()
assignValue name t@(TBasicType TVoid) value = do
  ht <- T.pack . show <$> haskellType t
  writePattern (T.pack name) (ExplicitSynonym "ptrToIntPtr" "intPtrToPtr" value ht)
assignValue name t@(TBasicType b) value = do
  ht <- T.pack . show <$> haskellType t
  hv <- showBasicType b value
  writePattern (T.pack name) (SimpleSynonym hv ht)
assignValue name t@(TInterface _ _) value = do
  ht <- T.pack . show <$> haskellType t
  api <- findAPI t
  case api of
    Just (APIEnum _) ->
        writePattern (T.pack name) (ExplicitSynonym "fromEnum" "toEnum" value ht)
    Just (APIFlags _) ->
        writePattern (T.pack name) (ExplicitSynonym "gflagsToWord" "wordToGFlags" value ht)
    _ -> notImplementedError $ "Don't know how to treat constants of type " ++ show t
assignValue _ t _ = notImplementedError $ "Don't know how to treat constants of type " ++ show t

-- | Show a basic type, in a way that can be assigned to the
-- corresponding Haskell type.
showBasicType                  :: BasicType -> Text -> ExcCodeGen Text
showBasicType TInt8    i       = return i
showBasicType TUInt8   i       = return i
showBasicType TInt16   i       = return i
showBasicType TUInt16  i       = return i
showBasicType TInt32   i       = return i
showBasicType TUInt32  i       = return i
showBasicType TInt64   i       = return i
showBasicType TUInt64  i       = return i
showBasicType TBoolean "0"     = return "False"
showBasicType TBoolean "false" = return "False"
showBasicType TBoolean "1"     = return "True"
showBasicType TBoolean "true"  = return "True"
showBasicType TBoolean b       = notImplementedError $ "Could not parse boolean \"" ++ T.unpack b ++ "\""
showBasicType TFloat   f       = return f
showBasicType TDouble  d       = return d
showBasicType TUTF8    s       = return . T.pack $ show s
showBasicType TFileName fn     = return . T.pack $ show fn
showBasicType TUniChar c       = return $ "'" <> c <> "'"
showBasicType TGType   gtype   = return $ "GType " <> gtype
-- We take care of this one separately above
showBasicType TVoid    _       = notImplementedError $ "Cannot directly show a pointer"
