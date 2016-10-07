-- | Marshalling of structs and unions.
module Data.GI.CodeGen.Struct ( genStructOrUnionFields
                              , genZeroStruct
                              , genZeroUnion
                              , extractCallbacksInStruct
                              , fixAPIStructs
                              , ignoreStruct
                              , genWrappedPtr
                              ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, when)

import Data.Maybe (mapMaybe, isJust, catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Config (Config(..), CodeGenFlags(..))
import Data.GI.CodeGen.SymbolNaming
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

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
  let name = upperName owner
  let fName = (underscoresToCamelCase . fieldName) field
  return $ name <> fName <> "FieldInfo"

-- | Extract a field from a struct.
buildFieldReader :: Name -> Field -> ExcCodeGen ()
buildFieldReader n field = group $ do
  let name' = upperName n
  let getter = fieldGetter n field

  nullConvert <- maybeNullConvert (fieldType field)
  hType <- tshow <$> if isJust nullConvert
                     then maybeT <$> isoHaskellType (fieldType field)
                     else isoHaskellType (fieldType field)
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
    result <- case nullConvert of
              Nothing -> convert "val" $ fToH (fieldType field) TransferNothing
              Just nullConverter -> do
                line $ "result <- " <> nullConverter <> " val $ \\val' -> do"
                indent $ do
                  val' <- convert "val'" $ fToH (fieldType field) TransferNothing
                  line $ "return " <> val'
                return "result"
    line $ "return " <> result

-- | Write a field into a struct. Note that, since we cannot know for
-- sure who will be deallocating the fields in the struct, we leave
-- any conversions that involve pointers to the caller. What this
-- means in practice is that scalar fields will get marshalled to/from
-- Haskell, while anything that involves pointers will be returned in
-- the C representation.
buildFieldWriter :: Name -> Field -> ExcCodeGen ()
buildFieldWriter n field = group $ do
  let name' = upperName n
  let setter = fieldSetter n field

  isPtr <- typeIsPtr (fieldType field)

  fType <- tshow <$> foreignType (fieldType field)
  hType <- if isPtr
           then return fType
           else tshow <$> haskellType (fieldType field)

  line $ setter <> " :: MonadIO m => " <> name' <> " -> "
           <> hType <> " -> m ()"
  line $ setter <> " s val = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $ do
    converted <- if isPtr
                 then return "val"
                 else convert "val" $ hToF (fieldType field) TransferNothing
    line $ "poke (ptr `plusPtr` " <> tshow (fieldOffset field)
         <> ") (" <> converted <> " :: " <> fType <> ")"

-- | Write a @NULL@ into a field of a struct of type `Ptr`.
buildFieldClear :: Name -> Field -> Text -> ExcCodeGen ()
buildFieldClear n field nullPtr = group $ do
  let name' = upperName n
  let clear = fieldClear n field

  fType <- tshow <$> foreignType (fieldType field)

  line $ clear <> " :: MonadIO m => " <> name' <> " -> m ()"
  line $ clear <> " s = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $
    line $ "poke (ptr `plusPtr` " <> tshow (fieldOffset field)
         <> ") ("  <> nullPtr <> " :: " <> fType <> ")"

-- | Name for the getter function
fieldGetter :: Name -> Field -> Text
fieldGetter name' field = "get" <> upperName name' <> fName field

-- | Name for the setter function
fieldSetter :: Name -> Field -> Text
fieldSetter name' field = "set" <> upperName name' <> fName field

-- | Name for the clear function
fieldClear :: Name -> Field -> Text
fieldClear name' field = "clear" <> upperName name' <> fName field

-- | Haskell name for the field
fName :: Field -> Text
fName = underscoresToCamelCase . fieldName

-- | Support for modifying fields as attributes. Returns a tuple with
-- the name of the overloaded label to be used for the field, and the
-- associated info type.
genAttrInfo :: Name -> Field -> CodeGen Text
genAttrInfo owner field = do
  it <- infoType owner field
  let on = upperName owner

  isPtr <- typeIsPtr (fieldType field)

  isNullable <- typeIsNullable (fieldType field)
  outType <- tshow <$> if isNullable
                       then maybeT <$> isoHaskellType (fieldType field)
                       else isoHaskellType (fieldType field)
  inType <- if isPtr
            then tshow <$> foreignType (fieldType field)
            else tshow <$> haskellType (fieldType field)

  line $ "data " <> it
  line $ "instance AttrInfo " <> it <> " where"
  indent $ do
    line $ "type AttrAllowedOps " <> it <>
             if isPtr
             then " = '[ 'AttrSet, 'AttrGet, 'AttrClear]"
             else " = '[ 'AttrSet, 'AttrGet]"
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
    line $ "attrClear _ = " <> if isPtr
                               then fieldClear owner field
                               else "undefined"

  blank

  group $ do
    let labelProxy = lcFirst on <> "_" <> lcFirst (fName field)
    line $ labelProxy <> " :: AttrLabelProxy \"" <> lcFirst (fName field) <> "\""
    line $ labelProxy <> " = AttrLabelProxy"

    exportProperty (fName field) labelProxy

  return $ "'(\"" <> (lcFirst  . fName) field <> "\", " <> it <> ")"

buildFieldAttributes :: Name -> Field -> ExcCodeGen (Maybe Text)
buildFieldAttributes n field
    | not (fieldVisible field) = return Nothing
    | privateType (fieldType field) = return Nothing
    | otherwise = group $ do
     nullPtr <- nullPtrForType (fieldType field)

     buildFieldReader n field
     buildFieldWriter n field
     maybe (return ()) (buildFieldClear n field) nullPtr

     exportProperty (fName field) (fieldGetter n field)
     exportProperty (fName field) (fieldSetter n field)
     when (isJust nullPtr) $
           exportProperty (fName field) (fieldClear n field)

     cfg <- config
     if cgOverloadedProperties (cgFlags cfg)
     then Just <$> genAttrInfo n field
     else return Nothing

    where privateType :: Type -> Bool
          privateType (TInterface _ n) = "Private" `T.isSuffixOf` n
          privateType _ = False

genStructOrUnionFields :: Name -> [Field] -> CodeGen ()
genStructOrUnionFields n fields = do
  let name' = upperName n

  attrs <- forM fields $ \field ->
      handleCGExc (\e -> line ("-- XXX Skipped attribute for \"" <> name' <>
                               ":" <> fieldName field <> "\" :: " <>
                               describeCGError e) >>
                   return Nothing)
                  (buildFieldAttributes n field)

  blank

  cfg <- config
  when (cgOverloadedProperties (cgFlags cfg)) $ group $ do
    let attrListName = name' <> "AttributeList"
    line $ "instance O.HasAttributeList " <> name'
    line $ "type instance O.AttributeList " <> name' <> " = " <> attrListName
    line $ "type " <> attrListName <> " = ('[ " <>
         T.intercalate ", " (catMaybes attrs) <> "] :: [(Symbol, *)])"

-- | Generate a constructor for a zero-filled struct/union of the given
-- type, using the boxed (or GLib, for unboxed types) allocator.
genZeroSU :: Name -> Int -> Bool -> CodeGen ()
genZeroSU n size isBoxed = group $ do
      let name = upperName n
      let builder = "newZero" <> name
          tsize = tshow size
      line $ "-- | Construct a `" <> name <> "` struct initialized to zero."
      line $ builder <> " :: MonadIO m => m " <> name
      line $ builder <> " = liftIO $ " <>
           if isBoxed
           then "callocBoxedBytes " <> tsize <> " >>= wrapBoxed " <> name
           else "wrappedPtrCalloc >>= wrapPtr " <> name
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
genZeroStruct n s =
    when (allocCalloc (structAllocationInfo s) /= AllocationOp "none" &&
          structSize s /= 0) $
    genZeroSU n (structSize s) (structIsBoxed s)

-- | Specialization for unions of `genZeroSU`.
genZeroUnion :: Name -> Union -> CodeGen ()
genZeroUnion n u =
    when (allocCalloc (unionAllocationInfo u ) /= AllocationOp "none" &&
          unionSize u /= 0) $
    genZeroSU n (unionSize u) (unionIsBoxed u)

-- | Construct a import with the given prefix.
prefixedForeignImport :: Text -> Text -> Text -> CodeGen Text
prefixedForeignImport prefix symbol prototype = group $ do
  line $ "foreign import ccall \"" <> symbol <> "\" " <> prefix <> symbol
           <> " :: " <> prototype
  return (prefix <> symbol)

-- | Same as `prefixedForeignImport`, but import a `FunPtr` to the symbol.
prefixedFunPtrImport :: Text -> Text -> Text -> CodeGen Text
prefixedFunPtrImport prefix symbol prototype = group $ do
  line $ "foreign import ccall \"&" <> symbol <> "\" " <> prefix <> symbol
           <> " :: FunPtr (" <> prototype <> ")"
  return (prefix <> symbol)

-- | Generate the typeclass with information for how to
-- allocate/deallocate unboxed structs and unions.
genWrappedPtr :: Name -> AllocationInfo -> Int -> CodeGen ()
genWrappedPtr n info size = group $ do
  let name' = upperName n

  let prefix = \op -> "_" <> name' <> "_" <> op <> "_"

  when (size == 0) $
       line $ "-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?"

  calloc <- case allocCalloc info of
              AllocationOp "none" ->
                  return ("error \"calloc not permitted for " <> name' <> "\"")
              AllocationOp op ->
                  prefixedForeignImport (prefix "calloc") op "IO (Ptr a)"
              AllocationOpUnknown ->
                  if size > 0
                  then return ("callocBytes " <> tshow size)
                  else return "return nullPtr"

  copy <- case allocCopy info of
            AllocationOp op ->
                prefixedForeignImport (prefix "copy") op "Ptr a -> IO (Ptr a)"
            AllocationOpUnknown ->
                if size > 0
                then return ("copyPtr " <> tshow size)
                else return "return"

  free <- case allocFree info of
            AllocationOp op -> ("Just " <>) <$>
                prefixedFunPtrImport (prefix "free") op "Ptr a -> IO ()"
            AllocationOpUnknown ->
                if size > 0
                then return "Just ptr_to_g_free"
                else return "Nothing"

  line $ "instance WrappedPtr " <> name' <> " where"
  indent $ do
      line $ "wrappedPtrCalloc = " <> calloc
      line $ "wrappedPtrCopy = " <> copy
      line $ "wrappedPtrFree = " <> free

  hsBoot $ line $ "instance WrappedPtr " <> name' <> " where"

