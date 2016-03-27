module GI.Struct ( genStructOrUnionFields
                 , genZeroStruct
                 , genZeroUnion
                 , extractCallbacksInStruct
                 , fixAPIStructs
                 , ignoreStruct)
    where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, when)

import Data.Maybe (mapMaybe, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import GI.API
import GI.Conversions
import GI.Code
import GI.SymbolNaming
import GI.Type
import GI.Util

-- | Whether (not) to generate bindings for the given struct.
ignoreStruct :: Name -> Struct -> Bool
ignoreStruct (Name _ name) s = isJust (gtypeStructFor s) ||
                               "Private" `T.isSuffixOf` name

-- | Canonical name for the type of a callback type embedded in a
-- struct field.
fieldCallbackType :: Text -> Field -> Text
fieldCallbackType structName field =
    structName <> (underscoresToCamelCase . fieldName) field <> "FieldCallback"

-- | Fix the interface names of callback fields in the struct to
-- correspond to the ones that we are going to generate.
fixCallbackStructFields :: Name -> Struct -> Struct
fixCallbackStructFields (Name ns structName) s = s {structFields = fixedFields}
    where fixedFields :: [Field]
          fixedFields = map fixField (structFields s)

          fixField :: Field -> Field
          fixField field =
              case fieldCallback field of
                Nothing -> field
                Just _ -> let n' = fieldCallbackType structName field
                          in field {fieldType = TInterface ns n'}

-- | Fix the interface names of callback fields in an APIStruct to
-- correspond to the ones that we are going to generate. If something
-- other than an APIStruct is passed in we don't touch it.
fixAPIStructs :: (Name, API) -> (Name, API)
fixAPIStructs (n, APIStruct s) = (n, APIStruct $ fixCallbackStructFields n s)
fixAPIStructs api = api

-- | Extract the callback types embedded in the fields of structs, and
-- at the same time fix the type of the corresponding fields. Returns
-- the list of APIs associated to this struct, not including the
-- struct itself.
extractCallbacksInStruct :: (Name, API) -> [(Name, API)]
extractCallbacksInStruct (n@(Name ns structName), APIStruct s)
    | ignoreStruct n s = []
    | otherwise =
        mapMaybe callbackInField (structFields s)
            where callbackInField :: Field -> Maybe (Name, API)
                  callbackInField field = do
                    callback <- fieldCallback field
                    let n' = fieldCallbackType structName field
                    return (Name ns n', APICallback callback)
extractCallbacksInStruct _ = []

-- | The name of the type encoding the information for a field in a
-- struct/union.
infoType :: Name -> Field -> CodeGen Text
infoType owner field = do
  name <- upperName owner
  let fName = (underscoresToCamelCase . fieldName) field
  return $ name <> fName <> "FieldInfo"

-- | Extract a field from a struct.
buildFieldReader :: Name -> Field -> ExcCodeGen ()
buildFieldReader n field = group $ do
  name' <- upperName n
  let getter = fieldGetter n field

  hType <- tshow <$> haskellType (fieldType field)
  fType <- tshow <$> foreignType (fieldType field)

  line $ getter <> " :: MonadIO m => " <> name' <> " -> m " <>
              if T.any (== ' ') hType
              then parenthesize hType
              else hType
  line $ getter <> " s = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $ do
    line $ "val <- peek (ptr `plusPtr` " <> tshow (fieldOffset field)
         <> ") :: IO " <> if T.any (== ' ') fType
                         then parenthesize fType
                         else fType
    result <- convert "val" $ fToH (fieldType field) TransferNothing
    line $ "return " <> result

-- | Write a field into a struct. Note that, since we cannot know for
-- sure who will be deallocating the fields in the struct, we leave
-- any conversions that involve pointers to the caller. What this
-- means in practice is that scalar fields will get marshalled to/from
-- Haskell, while anything that involves pointers will be returned in
-- the C representation.
buildFieldWriter :: Name -> Field -> ExcCodeGen ()
buildFieldWriter n field = group $ do
  name' <- upperName n
  let setter = fieldSetter n field

  nullable <- isNullable (fieldType field)

  fType <- tshow <$> foreignType (fieldType field)
  hType <- if nullable
           then return fType
           else tshow <$> haskellType (fieldType field)

  line $ setter <> " :: MonadIO m => " <> name' <> " -> "
           <> hType <> " -> m ()"
  line $ setter <> " s val = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $ do
    converted <- if nullable
                 then return "val"
                 else convert "val" $ hToF (fieldType field) TransferNothing
    line $ "poke (ptr `plusPtr` " <> tshow (fieldOffset field)
         <> ") (" <> converted <> " :: " <> fType <> ")"

-- | Name for the getter function
fieldGetter :: Name -> Field -> Text
fieldGetter name' field = lowerName name' <> "Read" <> fName field

-- | Name for the setter function
fieldSetter :: Name -> Field -> Text
fieldSetter name' field = lowerName name' <> "Write" <> fName field

-- | Haskell name for the field
fName :: Field -> Text
fName = underscoresToCamelCase . fieldName

-- | Support for modifying fields as attributes. Returns a tuple with
-- the name of the overloaded label to be used for the field, and the
-- associated info type.
genAttrInfo :: Name -> Field -> CodeGen Text
genAttrInfo owner field = do
  it <- infoType owner field
  on <- upperName owner

  nullable <- isNullable (fieldType field)

  outType <- tshow <$> haskellType (fieldType field)
  inType <- if nullable
            then tshow <$> foreignType (fieldType field)
            else tshow <$> haskellType (fieldType field)

  line $ "data " <> it
  line $ "instance AttrInfo " <> it <> " where"
  indent $ do
    line $ "type AttrAllowedOps " <> it <> " = '[ 'AttrSet, 'AttrGet]"
    line $ "type AttrSetTypeConstraint " <> it <> " = (~) "
             <> if T.any (== ' ') inType
                then parenthesize inType
                else inType
    line $ "type AttrBaseTypeConstraint " <> it <> " = (~) " <> on
    line $ "type AttrGetType " <> it <> " = " <> outType
    line $ "type AttrLabel " <> it <> " = \"" <> fieldName field <> "\""
    line $ "attrGet _ = " <> fieldGetter owner field
    line $ "attrSet _ = " <> fieldSetter owner field
    line $ "attrConstruct = undefined"

  return $ "'(\"" <> (lcFirst  . fName) field <> "\", " <> it <> ")"

buildFieldAttributes :: Name -> Field -> ExcCodeGen (Maybe Text)
buildFieldAttributes n field
    | not (fieldVisible field) = return Nothing
    | otherwise = group $ do

  hType <- tshow <$> haskellType (fieldType field)
  if ("Private" `T.isSuffixOf` hType ||
     not (fieldVisible field))
  then return Nothing
  else do
     buildFieldReader n field
     buildFieldWriter n field

     exportProperty (fName field) (fieldGetter n field)
     exportProperty (fName field) (fieldSetter n field)

     Just <$> genAttrInfo n field

genStructOrUnionFields :: Name -> [Field] -> CodeGen ()
genStructOrUnionFields n fields = do
  name' <- upperName n

  attrs <- forM fields $ \field ->
      handleCGExc (\e -> line ("-- XXX Skipped attribute for \"" <> name' <>
                               ":" <> fieldName field <> "\" :: " <>
                               describeCGError e) >>
                   return Nothing)
                  (buildFieldAttributes n field)

  blank

  group $ do
    let attrListName = name' <> "AttributeList"
    line $ "type instance AttributeList " <> name' <> " = " <> attrListName
    line $ "type " <> attrListName <> " = ('[ " <>
         T.intercalate ", " (catMaybes attrs) <> "] :: [(Symbol, *)])"


-- | Generate a constructor for a zero-filled struct/union of the given
-- type, using the boxed (or GLib, for unboxed types) allocator.
genZeroSU :: Name -> Int -> Bool -> CodeGen ()
genZeroSU n size isBoxed =
    when (size /= 0) $ group $ do
      name <- upperName n
      let builder = "newZero" <> name
          tsize = tshow size
      line $ "-- | Construct a `" <> name <> "` struct initialized to zero."
      line $ builder <> " :: MonadIO m => m " <> name
      line $ builder <> " = liftIO $ " <>
           if isBoxed
           then "callocBoxedBytes " <> tsize <> " >>= wrapBoxed " <> name
           else "callocBytes " <> tsize <> " >>= wrapPtr " <> name
      exportDecl builder

      blank

      -- Overloaded "new"
      group $ do
        line $ "instance tag ~ 'AttrSet => Constructible " <> name <> " tag where"
        indent $ do
           line $ "new _ attrs = do"
           indent $ do
              line $ "o <- " <> builder
              line $ "GI.Attributes.set o attrs"
              line $ "return o"

-- | Specialization for structs of `genZeroSU`.
genZeroStruct :: Name -> Struct -> CodeGen ()
genZeroStruct n s = genZeroSU n (structSize s) (structIsBoxed s)

-- | Specialization for unions of `genZeroSU`.
genZeroUnion :: Name -> Union -> CodeGen ()
genZeroUnion n u = genZeroSU n (unionSize u) (unionIsBoxed u)
