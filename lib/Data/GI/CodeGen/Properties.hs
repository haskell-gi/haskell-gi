module Data.GI.CodeGen.Properties
    ( genInterfaceProperties
    , genObjectProperties
    , genNamespacedPropLabels
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when, unless)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import Foreign.C.Types (CInt, CUInt)
import Foreign.Storable (sizeOf)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.GObject
import Data.GI.CodeGen.Haddock (addSectionDocumentation, writeHaddock,
                                RelativeDocPosition(DocBeforeSymbol))
import Data.GI.CodeGen.Inheritance (fullObjectPropertyList, fullInterfacePropertyList)
import Data.GI.CodeGen.SymbolNaming (lowerName, upperName, classConstraint,
                                     hyphensToCamelCase, qualifiedSymbol,
                                     typeConstraint, callbackDynamicWrapper,
                                     callbackHaskellToForeign,
                                     callbackWrapperAllocator, safeCast)
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

propTypeStr :: Type -> ExcCodeGen Text
propTypeStr t = case t of
   TBasicType TUTF8 -> return "String"
   TBasicType TFileName -> return "String"
   TBasicType TPtr -> return "Ptr"
   TByteArray -> return "ByteArray"
   TGHash _ _ -> return "Hash"
   TVariant -> return "Variant"
   TParamSpec -> return "ParamSpec"
   TGClosure _ -> return "Closure"
   TError -> return "GError"
   TGValue -> return "GValue"
   TBasicType TInt -> case sizeOf (0 :: CInt) of
                        4 -> return "Int32"
                        n -> error ("Unsupported `gint' type length: " ++
                                    show n)
   TBasicType TUInt -> case sizeOf (0 :: CUInt) of
                        4 -> return "UInt32"
                        n -> error ("Unsupported `guint' type length: " ++
                                    show n)
   TBasicType TLong -> return "Long"
   TBasicType TULong -> return "ULong"
   TBasicType TInt32 -> return "Int32"
   TBasicType TUInt32 -> return "UInt32"
   TBasicType TInt64 -> return "Int64"
   TBasicType TUInt64 -> return "UInt64"
   TBasicType TBoolean -> return "Bool"
   TBasicType TFloat -> return "Float"
   TBasicType TDouble -> return "Double"
   TBasicType TGType -> return "GType"
   TCArray True _ _ (TBasicType TUTF8) -> return "StringArray"
   TCArray True _ _ (TBasicType TFileName) -> return "StringArray"
   TGList (TBasicType TPtr) -> return "PtrGList"
   t@(TInterface n) -> do
     api <- findAPIByName n
     case api of
       APIEnum _ -> return "Enum"
       APIFlags _ -> return "Flags"
       APICallback _ -> return "Callback"
       APIStruct s -> if structIsBoxed s
                      then return "Boxed"
                      else notImplementedError $ "Unboxed struct property : " <> tshow t
       APIUnion u -> if unionIsBoxed u
                     then return "Boxed"
                     else notImplementedError $ "Unboxed union property : " <> tshow t
       APIObject o -> do
                isGO <- isGObject t
                if isGO
                then return "Object"
                else case (objGetValueFunc o, objSetValueFunc o) of
                  (Just _, Just _) -> return "IsGValueInstance"
                  _ -> notImplementedError $ "Non-GObject object property without known gvalue_set and/or gvalue_get: " <> tshow t
       APIInterface _ -> do
                isGO <- isGObject t
                if isGO
                then return "Object"
                else notImplementedError $ "Non-GObject interface property : " <> tshow t
       _ -> notImplementedError $ "Unknown interface property of type : " <> tshow t
   _ -> notImplementedError $ "Don't know how to handle properties of type " <> tshow t

-- | Some types need casting to a concrete type before we can set or
-- construct properties. For example, for non-GObject object
-- properties we accept any instance of @IsX@ for convenience, but
-- instance resolution of the IsGValueSetter requires a concrete
-- type. The following code implements the cast on the given variable,
-- if needed, and returns the name of the new variable of concrete
-- type.
castProp :: Type -> Text -> CodeGen Text
castProp t@(TInterface n) val = do
  api <- findAPIByName n
  case api of
    APIObject o -> do
      isGO <- isGObject t
      if not isGO
        then case (objGetValueFunc o, objSetValueFunc o) of
               (Just _, Just _) -> do
                 let val' = prime val
                 cast <- safeCast n
                 line $ val' <> " <- " <> cast <> " " <> val
                 return val'
               _ -> return val
        else return val
    _ -> return val
castProp _ val = return val

-- | The constraint for setting the given type in properties.
propSetTypeConstraint :: Type -> CodeGen Text
propSetTypeConstraint (TGClosure Nothing) =
  return $ "(~) " <> parenthesize (typeShow ("GClosure" `con` [con0 "()"]))
propSetTypeConstraint t = do
  isGO <- isGObject t
  if isGO
    then typeConstraint t
    else do
      isCallback <- typeIsCallback t
      hInType <- if isCallback
                 then typeShow <$> foreignType t
                 else typeShow <$> haskellType t
      return $ "(~) " <> if T.any (== ' ') hInType
                         then parenthesize hInType
                         else hInType

-- | The constraint for transferring the given type into a property.
propTransferTypeConstraint :: Type -> CodeGen Text
propTransferTypeConstraint t = do
  isGO <- isGObject t
  if isGO
    then typeConstraint t
    else do
      hInType <- typeShow <$> isoHaskellType t
      return $ "(~) " <> if T.any (== ' ') hInType
                         then parenthesize hInType
                         else hInType

-- | The type of the return value of @attrTransfer@ for the given
-- type.
propTransferType :: Type -> CodeGen Text
propTransferType (TGClosure Nothing) =
  return $ typeShow ("GClosure" `con` [con0 "()"])
propTransferType t = do
  isCallback <- typeIsCallback t
  if isCallback
             then typeShow <$> foreignType t
             else typeShow <$> haskellType t

-- | Given a value "v" of the given Haskell type, satisfying the
-- constraint generated by 'propTransferTypeConstraint', convert it
-- (allocating memory is necessary) to the type given by 'propTransferType'.
genPropTransfer :: Text -> Type -> CodeGen ()
genPropTransfer var (TGClosure Nothing) = line $ "return " <> var
genPropTransfer var t = do
  isGO <- isGObject t
  if isGO
    then do
      ht <- typeShow <$> haskellType t
      line $ "unsafeCastTo " <> ht <> " " <> var
    else case t of
           TInterface tn@(Name _ n) -> do
             isCallback <- typeIsCallback t
             if not isCallback
               then line $ "return " <> var
               else do
               -- Callbacks need to be wrapped
               wrapper <- qualifiedSymbol (callbackHaskellToForeign n) tn
               maker <- qualifiedSymbol (callbackWrapperAllocator n) tn
               line $ maker <> " " <>
                 parenthesize (wrapper <> " Nothing " <> var)
           _ -> line $ "return " <> var

-- | Given a property, return the set of constraints on the types, and
-- the type variables for the object and its value.
attrType :: Property -> CodeGen ([Text], Text)
attrType prop = do
  resetTypeVariableScope
  isCallback <- typeIsCallback (propType prop)
  if isCallback
    then do
      ftype <- foreignType (propType prop)
      return ([], typeShow ftype)
    else do
      (t,constraints) <- argumentType (propType prop) WithoutClosures
      return (constraints, t)

-- | Generate documentation for the given setter.
setterDoc :: Name -> Property -> Text
setterDoc n prop = T.unlines [
    "Set the value of the “@" <> propName prop <> "@” property."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.set' " <> lowerName n <> " [ #" <> hPropName prop
    <> " 'Data.GI.Base.Attributes.:=' value ]"
  , "@"]

genPropertySetter :: Text -> Name -> HaddockSection -> Property -> ExcCodeGen ()
genPropertySetter setter n docSection prop = group $ do
  (constraints, t) <- attrType prop
  isNullable <- typeIsNullable (propType prop)
  isCallback <- typeIsCallback (propType prop)
  cls <- classConstraint n
  let constraints' = "MonadIO m":(cls <> " o"):constraints
  tStr <- propTypeStr $ propType prop
  writeHaddock DocBeforeSymbol (setterDoc n prop)
  line $ setter <> " :: (" <> T.intercalate ", " constraints'
           <> ") => o -> " <> t <> " -> m ()"
  line $ setter <> " obj val = MIO.liftIO $ do"
  indent $ do
    val' <- castProp (propType prop) "val"
    line $ "B.Properties.setObjectProperty" <> tStr
             <> " obj \"" <> propName prop
             <> if isNullable && (not isCallback)
                then "\" (Just " <> val' <> ")"
                else "\" " <> val'
  export docSection setter

-- | Generate documentation for the given getter.
getterDoc :: Name -> Property -> Text
getterDoc n prop = T.unlines [
    "Get the value of the “@" <> propName prop <> "@” property."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.get' " <> lowerName n <> " #" <> hPropName prop
  , "@"]

genPropertyGetter :: Text -> Name -> HaddockSection -> Property -> ExcCodeGen ()
genPropertyGetter getter n docSection prop = group $ do
  isNullable <- typeIsNullable (propType prop)
  let isMaybe = isNullable && propReadNullable prop /= Just False
  constructorType <- isoHaskellType (propType prop)
  tStr <- propTypeStr $ propType prop
  cls <- classConstraint n
  let constraints = "(MonadIO m, " <> cls <> " o)"
      outType = if isMaybe
                then maybeT constructorType
                else constructorType
      returnType = typeShow $ "m" `con` [outType]
      getProp = if isNullable && not isMaybe
                then "checkUnexpectedNothing \"" <> getter
                         <> "\" $ B.Properties.getObjectProperty" <> tStr
                else "B.Properties.getObjectProperty" <> tStr
  -- Some property getters require in addition a constructor, which
  -- will convert the foreign value to the wrapped Haskell one.
  constructorArg <-
    if tStr `elem` ["Object", "Boxed"]
    then return $ " " <> typeShow constructorType
    else (if tStr == "Callback"
          then do
             callbackType <- haskellType (propType prop)
             return $ " " <> callbackDynamicWrapper (typeShow callbackType)
          else return "")

  writeHaddock DocBeforeSymbol (getterDoc n prop)
  line $ getter <> " :: " <> constraints <>
                " => o -> " <> returnType
  line $ getter <> " obj = MIO.liftIO $ " <> getProp
           <> " obj \"" <> propName prop <> "\"" <> constructorArg
  export docSection getter

-- | Generate documentation for the given constructor.
constructorDoc :: Property -> Text
constructorDoc prop = T.unlines [
    "Construct a `GValueConstruct` with valid value for the “@" <> propName prop <> "@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`."
    ]

genPropertyConstructor :: Text -> Name -> HaddockSection -> Property -> ExcCodeGen ()
genPropertyConstructor constructor n docSection prop = group $ do
  (constraints, t) <- attrType prop
  tStr <- propTypeStr $ propType prop
  isNullable <- typeIsNullable (propType prop)
  isCallback <- typeIsCallback (propType prop)
  cls <- classConstraint n
  let constraints' = (cls <> " o") : "MIO.MonadIO m" : constraints
      pconstraints = parenthesize (T.intercalate ", " constraints') <> " => "
  writeHaddock DocBeforeSymbol (constructorDoc prop)
  line $ constructor <> " :: " <> pconstraints
           <> t <> " -> m (GValueConstruct o)"
  line $ constructor <> " val = MIO.liftIO $ do"
  indent $ do
    val' <- castProp (propType prop) "val"
    line $ "MIO.liftIO $ B.Properties.constructObjectProperty" <> tStr
           <> " \"" <> propName prop
           <> if isNullable && (not isCallback)
              then "\" (P.Just " <> val' <> ")"
              else "\" " <> val'
  export docSection constructor

-- | Generate documentation for the given setter.
clearDoc :: Property -> Text
clearDoc prop = T.unlines [
    "Set the value of the “@" <> propName prop <> "@” property to `Nothing`."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.clear'" <> " #" <> hPropName prop
  , "@"]

genPropertyClear :: Text -> Name -> HaddockSection -> Property -> ExcCodeGen ()
genPropertyClear clear n docSection prop = group $ do
  cls <- classConstraint n
  let constraints = ["MonadIO m", cls <> " o"]
  tStr <- propTypeStr $ propType prop
  writeHaddock DocBeforeSymbol (clearDoc prop)
  nothingType <- typeShow . maybeT <$> haskellType (propType prop)
  isCallback <- typeIsCallback (propType prop)
  let nothing = if isCallback
                then "FP.nullFunPtr"
                else "(Nothing :: " <> nothingType <> ")"
  line $ clear <> " :: (" <> T.intercalate ", " constraints
           <> ") => o -> m ()"
  line $ clear <> " obj = liftIO $ B.Properties.setObjectProperty" <> tStr
           <> " obj \"" <> propName prop <> "\" " <> nothing
  export docSection clear

-- | The property name as a lexically valid Haskell identifier. Note
-- that this is not escaped, since it is assumed that it will be used
-- with a prefix, so if a property is named "class", for example, this
-- will return "class".
hPropName :: Property -> Text
hPropName = lcFirst . hyphensToCamelCase . propName

genObjectProperties :: Name -> Object -> CodeGen ()
genObjectProperties n o = do
  isGO <- apiIsGObject n (APIObject o)
  -- We do not generate bindings for objects not descending from GObject.
  when isGO $ do
    allProps <- fullObjectPropertyList n o >>=
                mapM (\(owner, prop) -> do
                        pi <- infoType owner prop
                        return $ "'(\"" <> hPropName prop
                                   <> "\", " <> pi <> ")")
    genProperties n (objProperties o) allProps

genInterfaceProperties :: Name -> Interface -> CodeGen ()
genInterfaceProperties n iface = do
  allProps <- fullInterfacePropertyList n iface >>=
                mapM (\(owner, prop) -> do
                        pi <- infoType owner prop
                        return $ "'(\"" <> hPropName prop
                                   <> "\", " <> pi <> ")")
  genProperties n (ifProperties iface) allProps

-- If the given accesor is available (indicated by available == True),
-- generate a fully qualified accesor name, otherwise just return
-- "undefined". accessor is "get", "set" or "construct"
accessorOrUndefined :: Bool -> Text -> Name -> Text -> CodeGen Text
accessorOrUndefined available accessor owner@(Name _ on) cName =
    if not available
    then return "undefined"
    else qualifiedSymbol (accessor <> on <> cName) owner

-- | The name of the type encoding the information for the property of
-- the object.
infoType :: Name -> Property -> CodeGen Text
infoType owner prop =
    let infoType = upperName owner <> (hyphensToCamelCase . propName) prop
                   <> "PropertyInfo"
    in qualifiedSymbol infoType owner

genOneProperty :: Name -> Property -> ExcCodeGen ()
genOneProperty owner prop = do
  let name = upperName owner
      cName = (hyphensToCamelCase . propName) prop
      docSection = NamedSubsection PropertySection (lcFirst cName)
      pName = name <> cName
      flags = propFlags prop
      writable = PropertyWritable `elem` flags &&
                 (PropertyConstructOnly `notElem` flags)
      readable = PropertyReadable `elem` flags
      constructOnly = PropertyConstructOnly `elem` flags

  addSectionDocumentation docSection (propDoc prop)

  -- For properties the meaning of having transfer /= TransferNothing
  -- is not clear (what are the right semantics for GValue setters?),
  -- and the other possibilities are very uncommon, so let us just
  -- assume that TransferNothing is always the case.
  when (propTransfer prop /= TransferNothing) $
       notImplementedError $ "Property " <> pName
                               <> " has unsupported transfer type "
                               <> tshow (propTransfer prop)

  isNullable <- typeIsNullable (propType prop)

  unless (readable || writable || constructOnly) $
       notImplementedError $ "Property is not readable, writable, or constructible: "
                               <> tshow pName

  group $ do
    line $ "-- VVV Prop \"" <> propName prop <> "\""
    line $ "   -- Type: " <> tshow (propType prop)
    line $ "   -- Flags: " <> tshow (propFlags prop)
    line $ "   -- Nullable: " <> tshow (propReadNullable prop,
                                        propWriteNullable prop)

  getter <- accessorOrUndefined readable "get" owner cName
  setter <- accessorOrUndefined writable "set" owner cName
  constructor <- accessorOrUndefined (writable || constructOnly)
                 "construct" owner cName
  clear <- accessorOrUndefined (isNullable && writable &&
                                propWriteNullable prop /= Just False)
           "clear" owner cName

  when (getter /= "undefined") $ genPropertyGetter getter owner docSection prop
  when (setter /= "undefined") $ genPropertySetter setter owner docSection prop
  when (constructor /= "undefined") $
       genPropertyConstructor constructor owner docSection prop
  when (clear /= "undefined") $ genPropertyClear clear owner docSection prop

  outType <- if not readable
             then return "()"
             else do
               sOutType <- if isNullable && propReadNullable prop /= Just False
                           then typeShow . maybeT <$> isoHaskellType (propType prop)
                           else typeShow <$> isoHaskellType (propType prop)
               return $ if T.any (== ' ') sOutType
                        then parenthesize sOutType
                        else sOutType

  -- Polymorphic #label style lens
  cppIf CPPOverloading $ do
    cls <- classConstraint owner
    inConstraint <- if writable || constructOnly
                    then propSetTypeConstraint (propType prop)
                    else return "(~) ()"
    transferConstraint <- if writable || constructOnly
                          then propTransferTypeConstraint (propType prop)
                          else return "(~) ()"
    transferType <- if writable || constructOnly
                    then propTransferType (propType prop)
                    else return "()"
    let allowedOps = (if writable
                      then ["'AttrSet", "'AttrConstruct"]
                      else [])
                     <> (if constructOnly
                         then ["'AttrConstruct"]
                         else [])
                     <> (if readable
                         then ["'AttrGet"]
                         else [])
                     <> (if isNullable && propWriteNullable prop /= Just False
                         then ["'AttrClear"]
                         else [])
    it <- infoType owner prop
    export docSection it
    bline $ "data " <> it
    line $ "instance AttrInfo " <> it <> " where"
    indent $ do
            line $ "type AttrAllowedOps " <> it
                     <> " = '[ " <> T.intercalate ", " allowedOps <> "]"
            line $ "type AttrBaseTypeConstraint " <> it <> " = " <> cls
            line $ "type AttrSetTypeConstraint " <> it
                     <> " = " <> inConstraint
            line $ "type AttrTransferTypeConstraint " <> it
                     <> " = " <> transferConstraint
            line $ "type AttrTransferType " <> it <> " = " <> transferType
            line $ "type AttrGetType " <> it <> " = " <> outType
            line $ "type AttrLabel " <> it <> " = \"" <> propName prop <> "\""
            line $ "type AttrOrigin " <> it <> " = " <> name
            line $ "attrGet = " <> getter
            line $ "attrSet = " <> setter
            if writable || constructOnly
              then do line $ "attrTransfer _ v = do"
                      indent $ genPropTransfer "v" (propType prop)
              else line $ "attrTransfer _ = undefined"
            line $ "attrConstruct = " <> constructor
            line $ "attrClear = " <> clear

-- | Generate a placeholder property for those cases in which code
-- generation failed.
genPlaceholderProperty :: Name -> Property -> CodeGen ()
genPlaceholderProperty owner prop = do
  line $ "-- XXX Placeholder"
  it <- infoType owner prop
  let cName = (hyphensToCamelCase . propName) prop
      docSection = NamedSubsection PropertySection (lcFirst cName)
  export docSection it
  bline $ "data " <> it
  line $ "instance AttrInfo " <> it <> " where"
  indent $ do
    line $ "type AttrAllowedOps " <> it <> " = '[]"
    line $ "type AttrSetTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrTransferTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrTransferType " <> it <> " = ()"
    line $ "type AttrBaseTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrGetType " <> it <> " = ()"
    line $ "type AttrLabel " <> it <> " = \"\""
    line $ "type AttrOrigin " <> it <> " = " <> upperName owner
    line $ "attrGet = undefined"
    line $ "attrSet = undefined"
    line $ "attrConstruct = undefined"
    line $ "attrClear = undefined"
    line $ "attrTransfer = undefined"

genProperties :: Name -> [Property] -> [Text] -> CodeGen ()
genProperties n ownedProps allProps = do
  let name = upperName n

  forM_ ownedProps $ \prop -> do
      handleCGExc (\err -> do
                     line $ "-- XXX Generation of property \""
                              <> propName prop <> "\" of object \""
                              <> name <> "\" failed."
                     printCGError err
                     cppIf CPPOverloading (genPlaceholderProperty n prop))
                  (genOneProperty n prop)

  cppIf CPPOverloading $ do
    let propListType = name <> "AttributeList"
    line $ "instance O.HasAttributeList " <> name
    line $ "type instance O.AttributeList " <> name <> " = " <> propListType
    line $ "type " <> propListType <> " = ('[ "
             <> T.intercalate ", " allProps <> "] :: [(Symbol, *)])"

-- | Generate gtk2hs compatible attribute labels (to ease
-- porting). These are namespaced labels, for examples
-- `widgetSensitive`. We take the list of methods, since there may be
-- name clashes (an example is Auth::is_for_proxy method in libsoup,
-- and the corresponding Auth::is-for-proxy property). When there is a
-- clash we give priority to the method.
genNamespacedPropLabels :: Name -> [Property] -> [Method] -> CodeGen ()
genNamespacedPropLabels owner props methods =
    let lName = lcFirst . hyphensToCamelCase . propName
    in genNamespacedAttrLabels owner (map lName props) methods

genNamespacedAttrLabels :: Name -> [Text] -> [Method] -> CodeGen ()
genNamespacedAttrLabels owner attrNames methods = do
  let name = upperName owner

  let methodNames = S.fromList (map (lowerName . methodName) methods)
      filteredAttrs = filter (`S.notMember` methodNames) attrNames

  forM_ filteredAttrs $ \attr -> group $ do
    let cName = ucFirst attr
        labelProxy = lcFirst name <> cName
        docSection = NamedSubsection PropertySection (lcFirst cName)

    line $ labelProxy <> " :: AttrLabelProxy \"" <> lcFirst cName <> "\""
    line $ labelProxy <> " = AttrLabelProxy"

    export docSection labelProxy
