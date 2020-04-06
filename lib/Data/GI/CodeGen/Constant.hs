module Data.GI.CodeGen.Constant
    ( genConstant
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.Haddock (deprecatedPragma, writeDocumentation,
                                RelativeDocPosition(..))
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util (tshow, ucFirst)

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
writePattern name (SimpleSynonym value t) = line $
      "pattern " <> ucFirst name <> " = " <> value <> " :: " <> t
writePattern name (ExplicitSynonym view expression value t) = do
  -- Supported only on ghc >= 7.10
  setModuleMinBase Base48
  line $ "pattern " <> ucFirst name <> " <- (" <> view <> " -> "
           <> value <> ") :: " <> t <> " where"
  indent $ line $
          ucFirst name <> " = " <> expression <> " " <> value <> " :: " <> t

genConstant :: Name -> Constant -> CodeGen ()
genConstant (Name _ name) c = group $ do
  setLanguagePragmas ["PatternSynonyms", "ScopedTypeVariables", "ViewPatterns"]
  deprecatedPragma name (constantDeprecated c)

  handleCGExc (\e -> do
                  line $ "-- XXX: Could not generate constant"
                  printCGError e
              )
    (do writeDocumentation DocBeforeSymbol (constantDocumentation c)
        assignValue name (constantType c) (constantValue c)
        export ToplevelSection ("pattern " <> ucFirst name))

-- | Assign to the given name the given constant value, in a way that
-- can be assigned to the corresponding Haskell type.
assignValue :: Text -> Type -> Text -> ExcCodeGen ()
assignValue name t@(TBasicType TPtr) value = do
  ht <- typeShow <$> haskellType t
  writePattern name (ExplicitSynonym "ptrToIntPtr" "intPtrToPtr" value ht)
assignValue name t@(TBasicType b) value = do
  ht <- typeShow <$> haskellType t
  hv <- showBasicType b value
  writePattern name (SimpleSynonym hv ht)
assignValue name t@(TInterface _) value = do
  ht <- typeShow <$> haskellType t
  api <- findAPI t
  case api of
    Just (APIEnum _) ->
        writePattern name (ExplicitSynonym "fromEnum" "toEnum" value ht)
    Just (APIFlags _) -> do
        -- gflagsToWord and wordToGFlags are polymorphic, so in this
        -- case we need to specialize so the type of the pattern is
        -- not ambiguous.
        let wordValue = "(" <> value <> " :: Word64)"
        writePattern name (ExplicitSynonym "gflagsToWord" "wordToGFlags" wordValue ht)
    _ -> notImplementedError $ "Don't know how to treat constants of type " <> tshow t
assignValue _ t _ = notImplementedError $ "Don't know how to treat constants of type " <> tshow t

-- | Show a basic type, in a way that can be assigned to the
-- corresponding Haskell type.
showBasicType                  :: BasicType -> Text -> ExcCodeGen Text
showBasicType TInt     i       = return i
showBasicType TUInt    i       = return i
showBasicType TLong    i       = return i
showBasicType TULong   i       = return i
showBasicType TInt8    i       = return i
showBasicType TUInt8   i       = return i
showBasicType TInt16   i       = return i
showBasicType TUInt16  i       = return i
showBasicType TInt32   i       = return i
showBasicType TUInt32  i       = return i
showBasicType TInt64   i       = return i
showBasicType TUInt64  i       = return i
showBasicType TBoolean "0"     = return "P.False"
showBasicType TBoolean "false" = return "P.False"
showBasicType TBoolean "1"     = return "P.True"
showBasicType TBoolean "true"  = return "P.True"
showBasicType TBoolean b       = notImplementedError $ "Could not parse boolean \"" <> b <> "\""
showBasicType TFloat   f       = return f
showBasicType TDouble  d       = return d
showBasicType TUTF8    s       = return . tshow $ s
showBasicType TFileName fn     = return . tshow $ fn
showBasicType TUniChar c       = return $ "'" <> c <> "'"
showBasicType TGType   gtype   = return $ "GType " <> gtype
showBasicType TIntPtr  ptr     = return ptr
showBasicType TUIntPtr ptr     = return ptr
-- We take care of this one separately above
showBasicType TPtr    _        = notImplementedError $ "Cannot directly show a pointer"
