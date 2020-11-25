-- | Marshalling of structs and unions.
module Data.GI.CodeGen.Struct ( genStructOrUnionFields
                              , genZeroStruct
                              , genZeroUnion
                              , extractCallbacksInStruct
                              , fixAPIStructs
                              , ignoreStruct
                              , genBoxed
                              , genWrappedPtr
                              ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, when)

import Data.Maybe (mapMaybe, isJust, catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Haddock (addSectionDocumentation, writeHaddock,
                                RelativeDocPosition(DocBeforeSymbol))
import Data.GI.CodeGen.SymbolNaming (upperName, lowerName,
                                     underscoresToCamelCase,
                                     qualifiedSymbol,
                                     callbackHaskellToForeign,
                                     callbackWrapperAllocator)

import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

-- | Whether (not) to generate bindings for the given struct.
ignoreStruct :: Name -> Struct -> Bool
ignoreStruct (Name _ name) s = (isJust (gtypeStructFor s) ||
                               "Private" `T.isSuffixOf` name) &&
                               (not $ structForceVisible s)

-- | Whether the given type corresponds to an ignored struct.
isIgnoredStructType :: Type -> CodeGen Bool
isIgnoredStructType t =
  case t of
    TInterface n -> do
      api <- getAPI t
      case api of
        APIStruct s -> return (ignoreStruct n s)
        _ -> return False
    _ -> return False

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
                          in field {fieldType = TInterface (Name ns n')}

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

-- | Whether a given field is an embedded struct/union.
isEmbedded :: Field -> ExcCodeGen Bool
isEmbedded field = do
  api <- findAPI (fieldType field)
  case api of
    Just (APIStruct _) -> checkEmbedding
    Just (APIUnion _) -> checkEmbedding
    _ -> return False
  where
    checkEmbedding :: ExcCodeGen Bool
    checkEmbedding = case fieldIsPointer field of
      Nothing -> badIntroError "Cannot determine whether the field is embedded."
      Just isPtr -> return (not isPtr)

-- | Name for the getter function
fieldGetter :: Name -> Field -> Text
fieldGetter name' field = "get" <> upperName name' <> fName field

-- | Generate documentation for the given getter.
getterDoc :: Name -> Field -> Text
getterDoc n field = T.unlines [
    "Get the value of the “@" <> fieldName field <> "@” field."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.get' " <> lowerName n <> " #" <> labelName field
  , "@"]

-- Notice that when reading the field we return a copy of any embedded
-- structs, so modifications of the returned struct will not affect
-- the original struct. This is on purpose, in order to increase
-- safety (otherwise the garbage collector may decide to free the
-- parent structure while we are modifying the embedded one, and havoc
-- will ensue).
-- | Extract a field from a struct.
buildFieldReader :: Name -> Field -> ExcCodeGen ()
buildFieldReader n field = group $ do
  let name' = upperName n
      getter = fieldGetter n field

  embedded <- isEmbedded field
  nullConvert <- if embedded
                 then return Nothing
                 else maybeNullConvert (fieldType field)
  hType <- typeShow <$> if isJust nullConvert
                        then maybeT <$> isoHaskellType (fieldType field)
                        else isoHaskellType (fieldType field)
  fType <- typeShow <$> foreignType (fieldType field)

  writeHaddock DocBeforeSymbol (getterDoc n field)

  line $ getter <> " :: MonadIO m => " <> name' <> " -> m " <>
              if T.any (== ' ') hType
              then parenthesize hType
              else hType
  line $ getter <> " s = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $ do
    let peekedType = if T.any (== ' ') fType
                     then parenthesize fType
                     else fType
    if embedded
    then line $ "let val = ptr `plusPtr` " <> tshow (fieldOffset field)
             <> " :: " <> peekedType
    else line $ "val <- peek (ptr `plusPtr` " <> tshow (fieldOffset field)
             <> ") :: IO " <> peekedType
    result <- case nullConvert of
              Nothing -> convert "val" $ fToH (fieldType field) TransferNothing
              Just nullConverter -> do
                line $ "result <- " <> nullConverter <> " val $ \\val' -> do"
                indent $ do
                  val' <- convert "val'" $ fToH (fieldType field) TransferNothing
                  line $ "return " <> val'
                return "result"
    line $ "return " <> result

-- | Name for the setter function
fieldSetter :: Name -> Field -> Text
fieldSetter name' field = "set" <> upperName name' <> fName field

-- | Generate documentation for the given setter.
setterDoc :: Name -> Field -> Text
setterDoc n field = T.unlines [
    "Set the value of the “@" <> fieldName field <> "@” field."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.set' " <> lowerName n <> " [ #" <> labelName field
    <> " 'Data.GI.Base.Attributes.:=' value ]"
  , "@"]

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

  fType <- typeShow <$> foreignType (fieldType field)
  hType <- if isPtr
           then return fType
           else typeShow <$> haskellType (fieldType field)

  writeHaddock DocBeforeSymbol (setterDoc n field)

  line $ setter <> " :: MonadIO m => " <> name' <> " -> "
           <> hType <> " -> m ()"
  line $ setter <> " s val = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $ do
    converted <- if isPtr
                 then return "val"
                 else convert "val" $ hToF (fieldType field) TransferNothing
    line $ "poke (ptr `plusPtr` " <> tshow (fieldOffset field)
         <> ") (" <> converted <> " :: " <> fType <> ")"

-- | Name for the clear function
fieldClear :: Name -> Field -> Text
fieldClear name' field = "clear" <> upperName name' <> fName field

-- | Documentation for the @clear@ method.
clearDoc :: Field -> Text
clearDoc field = T.unlines [
  "Set the value of the “@" <> fieldName field <> "@” field to `Nothing`."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.clear'" <> " #" <> labelName field
  , "@"]

-- | Write a @NULL@ into a field of a struct of type `Ptr`.
buildFieldClear :: Name -> Field -> Text -> ExcCodeGen ()
buildFieldClear n field nullPtr = group $ do
  let name' = upperName n
  let clear = fieldClear n field

  fType <- typeShow <$> foreignType (fieldType field)

  writeHaddock DocBeforeSymbol (clearDoc field)

  line $ clear <> " :: MonadIO m => " <> name' <> " -> m ()"
  line $ clear <> " s = liftIO $ withManagedPtr s $ \\ptr -> do"
  indent $
    line $ "poke (ptr `plusPtr` " <> tshow (fieldOffset field)
         <> ") ("  <> nullPtr <> " :: " <> fType <> ")"

-- | Return whether the given type corresponds to a callback that does
-- not throw exceptions. See [Note: Callables that throw] for the
-- reason why we do not try to wrap callbacks that throw exceptions.
isRegularCallback :: Type -> CodeGen Bool
isRegularCallback t@(TInterface _) = do
  api <- getAPI t
  case api of
    APICallback (Callback {cbCallable = callable}) ->
      return (not $ callableThrows callable)
    _ -> return False
isRegularCallback _ = return False

-- | The types accepted by the allocating set function
-- 'Data.GI.Base.Attributes.(:&=)'.
fieldTransferTypeConstraint :: Type -> CodeGen Text
fieldTransferTypeConstraint t = do
  isPtr <- typeIsPtr t
  isRegularCallback <- isRegularCallback t
  inType <- if isPtr && not isRegularCallback
            then typeShow <$> foreignType t
            else typeShow <$> isoHaskellType t
  return $ "(~)" <> if T.any (== ' ') inType
                    then parenthesize inType
                    else inType

-- | The type generated by 'Data.GI.Base.attrTransfer' for this
-- field. This type should satisfy the
-- 'Data.GI.Base.Attributes.AttrSetTypeConstraint' for the type.
fieldTransferType :: Type -> CodeGen Text
fieldTransferType t = do
  isPtr <- typeIsPtr t
  inType <- if isPtr
            then typeShow <$> foreignType t
            else typeShow <$> haskellType t
  return $ if T.any (== ' ') inType
           then parenthesize inType
           else inType

-- | Generate the field transfer function, which marshals Haskell
-- values to types that we can set, even if we need to allocate memory.
genFieldTransfer :: Text -> Type -> CodeGen ()
genFieldTransfer var t@(TInterface tn@(Name _ n)) = do
  isRegularCallback <- isRegularCallback t
  if isRegularCallback
    then do
      wrapper <- qualifiedSymbol (callbackHaskellToForeign n) tn
      maker <- qualifiedSymbol (callbackWrapperAllocator n) tn
      line $ maker <> " " <>
        parenthesize (wrapper <> " Nothing " <> var)
    else line $ "return " <> var
genFieldTransfer var _ = line $ "return " <> var

-- | Haskell name for the field
fName :: Field -> Text
fName = underscoresToCamelCase . fieldName

-- | Label associated to the field.
labelName :: Field -> Text
labelName = lcFirst  . fName

-- | Support for modifying fields as attributes. Returns a tuple with
-- the name of the overloaded label to be used for the field, and the
-- associated info type.
genAttrInfo :: Name -> Field -> ExcCodeGen Text
genAttrInfo owner field = do
  it <- infoType owner field
  let on = upperName owner

  isPtr <- typeIsPtr (fieldType field)

  embedded <- isEmbedded field
  isNullable <- typeIsNullable (fieldType field)
  outType <- typeShow <$> if not embedded && isNullable
                          then maybeT <$> isoHaskellType (fieldType field)
                          else isoHaskellType (fieldType field)
  inType <- if isPtr
            then typeShow <$> foreignType (fieldType field)
            else typeShow <$> haskellType (fieldType field)
  transferType <- fieldTransferType (fieldType field)
  transferConstraint <- fieldTransferTypeConstraint (fieldType field)

  line $ "data " <> it
  line $ "instance AttrInfo " <> it <> " where"
  indent $ do
    line $ "type AttrBaseTypeConstraint " <> it <> " = (~) " <> on
    line $ "type AttrAllowedOps " <> it <>
             if embedded
             then " = '[ 'AttrGet]"
             else if isPtr
                  then " = '[ 'AttrSet, 'AttrGet, 'AttrClear]"
                  else " = '[ 'AttrSet, 'AttrGet]"
    line $ "type AttrSetTypeConstraint " <> it <> " = (~) "
             <> if T.any (== ' ') inType
                then parenthesize inType
                else inType
    line $ "type AttrTransferTypeConstraint " <> it <> " = " <> transferConstraint
    line $ "type AttrTransferType " <> it <> " = " <> transferType
    line $ "type AttrGetType " <> it <> " = " <> outType
    line $ "type AttrLabel " <> it <> " = \"" <> fieldName field <> "\""
    line $ "type AttrOrigin " <> it <> " = " <> on
    line $ "attrGet = " <> fieldGetter owner field
    line $ "attrSet = " <> if not embedded
                             then fieldSetter owner field
                             else "undefined"
    line $ "attrConstruct = undefined"
    line $ "attrClear = " <> if not embedded && isPtr
                               then fieldClear owner field
                               else "undefined"
    if not embedded
      then do
          line $ "attrTransfer _ v = do"
          indent $ genFieldTransfer "v" (fieldType field)
      else line $ "attrTransfer = undefined"

  blank

  group $ do
    let labelProxy = lcFirst on <> "_" <> lcFirst (fName field)
    line $ labelProxy <> " :: AttrLabelProxy \"" <> lcFirst (fName field) <> "\""
    line $ labelProxy <> " = AttrLabelProxy"

    export (NamedSubsection PropertySection $ lcFirst $ fName field) labelProxy

  return $ "'(\"" <> labelName field <> "\", " <> it <> ")"

-- | Build code for a single field.
buildFieldAttributes :: Name -> Field -> ExcCodeGen (Maybe Text)
buildFieldAttributes n field
    | not (fieldVisible field) = return Nothing
    | privateType (fieldType field) = return Nothing
    | otherwise = group $ do

     -- We don't generate bindings for private and class structs, so
     -- do not generate bindings for fields pointing to class structs
     -- either.
     ignored <- isIgnoredStructType (fieldType field)
     when ignored $
      notImplementedError "Field type is an unsupported struct type"

     nullPtr <- nullPtrForType (fieldType field)

     embedded <- isEmbedded field

     addSectionDocumentation docSection (fieldDocumentation field)

     buildFieldReader n field
     export docSection (fieldGetter n field)

     when (not embedded) $ do
         buildFieldWriter n field
         export docSection (fieldSetter n field)

         case nullPtr of
           Just null -> do
              buildFieldClear n field null
              export docSection (fieldClear n field)
           Nothing -> return ()

     Just <$> cppIf CPPOverloading (genAttrInfo n field)

    where privateType :: Type -> Bool
          privateType (TInterface n) = "Private" `T.isSuffixOf` name n
          privateType _ = False

          docSection = NamedSubsection PropertySection $ lcFirst $ fName field

-- | Generate code for the given list of fields.
genStructOrUnionFields :: Name -> [Field] -> CodeGen ()
genStructOrUnionFields n fields = do
  let name' = upperName n

  attrs <- forM fields $ \field ->
      handleCGExc (\e -> do
                      line ("-- XXX Skipped attribute for \"" <> name' <>
                             ":" <> fieldName field <> "\"")
                      printCGError e
                      return Nothing)
                  (buildFieldAttributes n field)

  blank

  cppIf CPPOverloading $ do
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

      writeHaddock DocBeforeSymbol ("Construct a `" <> name <>
                                     "` struct initialized to zero.")

      line $ builder <> " :: MonadIO m => m " <> name
      line $ builder <> " = liftIO $ " <>
           if isBoxed
           then "callocBoxedBytes " <> tsize <> " >>= wrapBoxed " <> name
           else "boxedPtrCalloc >>= wrapPtr " <> name
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

-- | Generate a GValue instance for @GBoxed@ objects.
genBoxedGValueInstance :: Name -> Text -> CodeGen ()
genBoxedGValueInstance n get_type_fn = do
  let name' = upperName n
      doc = "Convert '" <> name' <> "' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'."

  writeHaddock DocBeforeSymbol doc

  group $ do
    bline $ "instance B.GValue.IsGValue (Maybe " <> name' <> ") where"
    indent $ group $ do
      line $ "gvalueGType_ = " <> get_type_fn
      line $ "gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr " <> name' <> ")"
      line $ "gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)"
      line $ "gvalueGet_ gv = do"
      indent $ group $ do
        line $ "ptr <- B.GValue.get_boxed gv :: IO (Ptr " <> name' <> ")"
        line $ "if ptr /= FP.nullPtr"
        line $ "then P.Just <$> B.ManagedPtr.newBoxed " <> name' <> " ptr"
        line $ "else return P.Nothing"

-- | Allocation and deallocation for types registered as `GBoxed` in
-- the GLib type system.
genBoxed :: Name -> Text -> CodeGen ()
genBoxed n typeInit = do
  let name' = upperName n
      get_type_fn = "c_" <> typeInit

  group $ do
    line $ "foreign import ccall \"" <> typeInit <> "\" " <>
            get_type_fn <> " :: "
    indent $ line "IO GType"

  group $ do
    line $ "type instance O.ParentTypes " <> name' <> " = '[]"
    bline $ "instance O.HasParentTypes " <> name'

  group $ do
    bline $ "instance B.Types.TypedObject " <> name' <> " where"
    indent $ line $ "glibType = " <> get_type_fn

  group $ do
    bline $ "instance B.Types.GBoxed " <> name'

  genBoxedGValueInstance n get_type_fn

-- | Generate the typeclass with information for how to
-- allocate/deallocate a given type which is not a `GBoxed`.
genWrappedPtr :: Name -> AllocationInfo -> Int -> CodeGen ()
genWrappedPtr n info size = group $ do
  let prefix = \op -> "_" <> name' <> "_" <> op <> "_"

  when (size == 0 && allocFree info == AllocationOpUnknown) $
       line $ "-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?"

  copy <- case allocCopy info of
            AllocationOp op -> do
                copy <- prefixedForeignImport (prefix "copy") op "Ptr a -> IO (Ptr a)"
                return ("\\p -> B.ManagedPtr.withManagedPtr p (" <> copy <>
                        " >=> B.ManagedPtr.wrapPtr " <> name' <> ")")
            AllocationOpUnknown ->
                if size > 0
                then return ("\\p -> B.ManagedPtr.withManagedPtr p (copyBytes "
                              <> tshow size <>
                              " >=> B.ManagedPtr.wrapPtr " <> name' <> ")")
                else return "return"

  free <- case allocFree info of
            AllocationOp op -> do
              free <- prefixedForeignImport (prefix "free") op "Ptr a -> IO ()"
              return $ "\\p -> B.ManagedPtr.withManagedPtr p " <> free
            AllocationOpUnknown ->
                if size > 0
                then return "\\x -> SP.withManagedPtr x SP.freeMem"
                else return "\\_x -> return ()"

  bline $ "instance BoxedPtr " <> name' <> " where"
  indent $ do
      line $ "boxedPtrCopy = " <> copy
      line $ "boxedPtrFree = " <> free

  case allocCalloc info of
    AllocationOp "none" -> return ()
    AllocationOp op -> do
      calloc <- prefixedForeignImport (prefix "calloc") op "IO (Ptr a)"
      callocInstance calloc
    AllocationOpUnknown ->
      if size > 0
      then do
        let calloc = "callocBytes " <> tshow size
        callocInstance calloc
      else return ()

  where name' = upperName n

        callocInstance :: Text -> CodeGen()
        callocInstance calloc = group $ do
          bline $ "instance CallocPtr " <> name' <> " where"
          indent $ do
            line $ "boxedPtrCalloc = " <> calloc
