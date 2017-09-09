module Data.GI.CodeGen.Properties
    ( genInterfaceProperties
    , genObjectProperties
    , genNamespacedPropLabels
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when, unless)
import Data.Monoid ((<>))
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
import Data.GI.CodeGen.SymbolNaming (lowerName, upperName,
                                     classConstraint, typeConstraint,
                                     hyphensToCamelCase, qualifiedSymbol)
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

propTypeStr :: Type -> CodeGen Text
propTypeStr t = case t of
   TBasicType TUTF8 -> return "String"
   TBasicType TFileName -> return "String"
   TBasicType TPtr -> return "Ptr"
   TByteArray -> return "ByteArray"
   TGHash _ _ -> return "Hash"
   TVariant -> return "Variant"
   TParamSpec -> return "ParamSpec"
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
       APIStruct s -> if structIsBoxed s
                      then return "Boxed"
                      else error $ "Unboxed struct property : " ++ show t
       APIUnion u -> if unionIsBoxed u
                     then return "Boxed"
                     else error $ "Unboxed union property : " ++ show t
       APIObject _ -> do
                isGO <- isGObject t
                if isGO
                then return "Object"
                else error $ "Non-GObject object property : " ++ show t
       APIInterface _ -> do
                isGO <- isGObject t
                if isGO
                then return "Object"
                else error $ "Non-GObject interface property : " ++ show t
       _ -> error $ "Unknown interface property of type : " ++ show t
   _ -> error $ "Don't know how to handle properties of type " ++ show t

-- | Given a property, return the set of constraints on the types, and
-- the type variables for the object and its value.
attrType :: Property -> CodeGen ([Text], Text)
attrType prop = do
  (_,t,constraints) <- argumentType ['a'..'l'] $ propType prop
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

genPropertySetter :: Text -> Name -> HaddockSection -> Property -> CodeGen ()
genPropertySetter setter n docSection prop = group $ do
  (constraints, t) <- attrType prop
  isNullable <- typeIsNullable (propType prop)
  cls <- classConstraint n
  let constraints' = "MonadIO m":(cls <> " o"):constraints
  tStr <- propTypeStr $ propType prop
  writeHaddock DocBeforeSymbol (setterDoc n prop)
  line $ setter <> " :: (" <> T.intercalate ", " constraints'
           <> ") => o -> " <> t <> " -> m ()"
  line $ setter <> " obj val = liftIO $ setObjectProperty" <> tStr
           <> " obj \"" <> propName prop
           <> if isNullable
              then "\" (Just val)"
              else "\" val"
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

genPropertyGetter :: Text -> Name -> HaddockSection -> Property -> CodeGen ()
genPropertyGetter getter n docSection prop = group $ do
  isNullable <- typeIsNullable (propType prop)
  let isMaybe = isNullable && propReadNullable prop /= Just False
  constructorType <- haskellType (propType prop)
  tStr <- propTypeStr $ propType prop
  cls <- classConstraint n
  let constraints = "(MonadIO m, " <> cls <> " o)"
      outType = if isMaybe
                then maybeT constructorType
                else constructorType
      getProp = if isNullable && not isMaybe
                then "checkUnexpectedNothing \"" <> getter
                         <> "\" $ getObjectProperty" <> tStr
                else "getObjectProperty" <> tStr
  writeHaddock DocBeforeSymbol (getterDoc n prop)
  line $ getter <> " :: " <> constraints <>
                " => o -> " <> typeShow ("m" `con` [outType])
  line $ getter <> " obj = liftIO $ " <> getProp
           <> " obj \"" <> propName prop <> "\"" <>
           if tStr `elem` ["Object", "Boxed"]
           then " " <> typeShow constructorType -- These require the constructor.
           else ""
  export docSection getter

-- | Generate documentation for the given constructor.
constructorDoc :: Property -> Text
constructorDoc prop = T.unlines [
    "Construct a `GValueConstruct` with valid value for the “@" <> propName prop <> "@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`."
    ]

genPropertyConstructor :: Text -> Name -> HaddockSection -> Property -> CodeGen ()
genPropertyConstructor constructor n docSection prop = group $ do
  (constraints, t) <- attrType prop
  tStr <- propTypeStr $ propType prop
  isNullable <- typeIsNullable (propType prop)
  cls <- classConstraint n
  let constraints' = (cls <> " o") : constraints
      pconstraints = parenthesize (T.intercalate ", " constraints') <> " => "
  writeHaddock DocBeforeSymbol (constructorDoc prop)
  line $ constructor <> " :: " <> pconstraints
           <> t <> " -> IO (GValueConstruct o)"
  line $ constructor <> " val = constructObjectProperty" <> tStr
           <> " \"" <> propName prop
           <> if isNullable
              then "\" (Just val)"
              else "\" val"
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

genPropertyClear :: Text -> Name -> HaddockSection -> Property -> CodeGen ()
genPropertyClear clear n docSection prop = group $ do
  nothingType <- typeShow . maybeT <$> haskellType (propType prop)
  cls <- classConstraint n
  let constraints = ["MonadIO m", cls <> " o"]
  tStr <- propTypeStr $ propType prop
  writeHaddock DocBeforeSymbol (clearDoc prop)
  line $ clear <> " :: (" <> T.intercalate ", " constraints
           <> ") => o -> m ()"
  line $ clear <> " obj = liftIO $ setObjectProperty" <> tStr
           <> " obj \"" <> propName prop <> "\" (Nothing :: "
           <> nothingType <> ")"
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
      docSection = PropertySection (lcFirst cName)
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
                           then typeShow . maybeT <$> haskellType (propType prop)
                           else typeShow <$> haskellType (propType prop)
               return $ if T.any (== ' ') sOutType
                        then parenthesize sOutType
                        else sOutType

  -- Polymorphic #label style lens
  cppIf CPPOverloading $ do
    cls <- classConstraint owner
    inConstraint <- if writable || constructOnly
                    then do
                      inIsGO <- isGObject (propType prop)
                      hInType <- typeShow <$> haskellType (propType prop)
                      if inIsGO
                         then typeConstraint (propType prop)
                         else return $ "(~) " <> if T.any (== ' ') hInType
                                                 then parenthesize hInType
                                                 else hInType
                    else return "(~) ()"
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
            line $ "type AttrSetTypeConstraint " <> it
                     <> " = " <> inConstraint
            line $ "type AttrBaseTypeConstraint " <> it <> " = " <> cls
            line $ "type AttrGetType " <> it <> " = " <> outType
            line $ "type AttrLabel " <> it <> " = \"" <> propName prop <> "\""
            line $ "type AttrOrigin " <> it <> " = " <> name
            line $ "attrGet _ = " <> getter
            line $ "attrSet _ = " <> setter
            line $ "attrConstruct _ = " <> constructor
            line $ "attrClear _ = " <> clear

-- | Generate a placeholder property for those cases in which code
-- generation failed.
genPlaceholderProperty :: Name -> Property -> CodeGen ()
genPlaceholderProperty owner prop = do
  line $ "-- XXX Placeholder"
  it <- infoType owner prop
  let cName = (hyphensToCamelCase . propName) prop
      docSection = PropertySection (lcFirst cName)
  export docSection it
  line $ "data " <> it
  line $ "instance AttrInfo " <> it <> " where"
  indent $ do
    line $ "type AttrAllowedOps " <> it <> " = '[]"
    line $ "type AttrSetTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrBaseTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrGetType " <> it <> " = ()"
    line $ "type AttrLabel " <> it <> " = \"\""
    line $ "type AttrOrigin " <> it <> " = " <> upperName owner
    line $ "attrGet = undefined"
    line $ "attrSet = undefined"
    line $ "attrConstruct = undefined"
    line $ "attrClear = undefined"

genProperties :: Name -> [Property] -> [Text] -> CodeGen ()
genProperties n ownedProps allProps = do
  let name = upperName n

  forM_ ownedProps $ \prop -> do
      handleCGExc (\err -> do
                     line $ "-- XXX Generation of property \""
                              <> propName prop <> "\" of object \""
                              <> name <> "\" failed: " <> describeCGError err
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
        docSection = PropertySection (lcFirst cName)

    line $ labelProxy <> " :: AttrLabelProxy \"" <> lcFirst cName <> "\""
    line $ labelProxy <> " = AttrLabelProxy"

    export docSection labelProxy
