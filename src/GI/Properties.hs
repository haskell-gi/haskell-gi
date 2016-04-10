module GI.Properties
    ( genInterfaceProperties
    , genObjectProperties
    , genNamespacedPropLabels
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import Foreign.Storable (sizeOf)
import Foreign.C (CInt, CUInt)

import GI.API
import GI.Conversions
import GI.Code
import GI.GObject
import GI.Inheritance (fullObjectPropertyList, fullInterfacePropertyList)
import GI.SymbolNaming (upperName, classConstraint, qualify,
                        hyphensToCamelCase, lowerName)
import GI.Type
import GI.Util

propTypeStr :: Type -> CodeGen Text
propTypeStr t = case t of
   TBasicType TUTF8 -> return "String"
   TBasicType TFileName -> return "String"
   TBasicType TVoid -> return "Ptr"
   TByteArray -> return "ByteArray"
   TGHash _ _ -> return "Hash"
   TVariant -> return "Variant"
   TParamSpec -> return "ParamSpec"
   TBasicType TInt32 -> do
     -- This should work for all systems in common use, but rather
     -- than leaving the assumption implicit better double checking.
     when (sizeOf (0 :: CInt) /= 4) $
          error "C Integers are not 4 bytes, unsupported platform."
     return "CInt"
   TBasicType TUInt32 -> do
     when (sizeOf (0 :: CUInt) /= 4) $
          error "C Integers are not 4 bytes, unsupported platform."
     return "CUInt"
   TBasicType TInt64 -> return "Int64"
   TBasicType TUInt64 -> return "UInt64"
   TBasicType TBoolean -> return "Bool"
   TBasicType TFloat -> return "Float"
   TBasicType TDouble -> return "Double"
   TBasicType TGType -> return "GType"
   TCArray True _ _ (TBasicType TUTF8) -> return "StringArray"
   TCArray True _ _ (TBasicType TFileName) -> return "StringArray"
   TGList (TBasicType TVoid) -> return "PtrGList"
   t@(TInterface ns n) -> do
     api <- findAPIByName (Name ns n)
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

genPropertySetter :: Name -> Text -> Property -> CodeGen ()
genPropertySetter n pName prop = group $ do
  oName <- upperName n
  (constraints, t) <- attrType prop
  isNullable <- typeIsNullable (propType prop)
  let constraints' = "MonadIO m":(classConstraint oName <> " o"):constraints
  tStr <- propTypeStr $ propType prop
  line $ "set" <> pName <> " :: (" <> T.intercalate ", " constraints'
           <> ") => o -> " <> t <> " -> m ()"
  line $ "set" <> pName <> " obj val = liftIO $ setObjectProperty" <> tStr
           <> " obj \"" <> propName prop
           <> if isNullable
              then "\" (Just val)"
              else "\" val"

genPropertyGetter :: Name -> Text -> Property -> CodeGen ()
genPropertyGetter n pName prop = group $ do
  oName <- upperName n
  isNullable <- typeIsNullable (propType prop)
  let isMaybe = isNullable && propReadNullable prop /= Just False
  constructorType <- haskellType (propType prop)
  tStr <- propTypeStr $ propType prop
  let constraints = "(MonadIO m, " <> classConstraint oName <> " o)"
      outType = if isMaybe
                then maybeT constructorType
                else constructorType
      getter = if isNullable && not isMaybe
               then "checkUnexpectedNothing \"get" <> pName
                        <> "\" $ getObjectProperty" <> tStr
               else "getObjectProperty" <> tStr
  line $ "get" <> pName <> " :: " <> constraints <>
                " => o -> " <> tshow ("m" `con` [outType])
  line $ "get" <> pName <> " obj = liftIO $ " <> getter
           <> " obj \"" <> propName prop <> "\"" <>
           if tStr `elem` ["Object", "Boxed"]
           then " " <> tshow constructorType -- These require the constructor.
           else ""

genPropertyConstructor :: Text -> Property -> CodeGen ()
genPropertyConstructor pName prop = group $ do
  (constraints, t) <- attrType prop
  tStr <- propTypeStr $ propType prop
  isNullable <- typeIsNullable (propType prop)
  let constraints' =
          case constraints of
            [] -> ""
            _ -> parenthesize (T.intercalate ", " constraints) <> " => "
  line $ "construct" <> pName <> " :: " <> constraints'
           <> t <> " -> IO ([Char], GValue)"
  line $ "construct" <> pName <> " val = constructObjectProperty" <> tStr
           <> " \"" <> propName prop
           <> if isNullable
              then "\" (Just val)"
              else "\" val"

genPropertyClear :: Name -> Text -> Property -> CodeGen ()
genPropertyClear n pName prop = group $ do
  oName <- upperName n
  nothingType <- tshow . maybeT <$> haskellType (propType prop)
  let constraints = ["MonadIO m", classConstraint oName <> " o"]
  tStr <- propTypeStr $ propType prop
  line $ "clear" <> pName <> " :: (" <> T.intercalate ", " constraints
           <> ") => o -> m ()"
  line $ "clear" <> pName <> " obj = liftIO $ setObjectProperty" <> tStr
           <> " obj \"" <> propName prop <> "\" (Nothing :: "
           <> nothingType <> ")"

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
accessorOrUndefined available accessor (Name ons on) cName =
    if not available
    then return "undefined"
    else do
      prefix <- qualify ons
      return $ prefix <> accessor <> on <> cName

-- | The name of the type encoding the information for the property of
-- the object.
infoType :: Name -> Property -> CodeGen Text
infoType owner prop = do
  name <- upperName owner
  let cName = (hyphensToCamelCase . propName) prop
  return $ name <> cName <> "PropertyInfo"

genOneProperty :: Name -> Property -> ExcCodeGen ()
genOneProperty owner prop = do
  name <- upperName owner
  let cName = (hyphensToCamelCase . propName) prop
      pName = name <> cName
      flags = propFlags prop
      writable = PropertyWritable `elem` flags &&
                 (PropertyConstructOnly `notElem` flags)
      readable = PropertyReadable `elem` flags
      constructOnly = PropertyConstructOnly `elem` flags

  -- For properties the meaning of having transfer /= TransferNothing
  -- is not clear (what are the right semantics for GValue setters?),
  -- and the other possibilities are very uncommon, so let us just
  -- assume that TransferNothing is always the case.
  when (propTransfer prop /= TransferNothing) $
       notImplementedError $ "Property " <> pName
                               <> " has unsupported transfer type "
                               <> tshow (propTransfer prop)

  isNullable <- typeIsNullable (propType prop)

  getter <- accessorOrUndefined readable "get" owner cName
  setter <- accessorOrUndefined writable "set" owner cName
  constructor <- accessorOrUndefined (writable || constructOnly)
                 "construct" owner cName
  clear <- accessorOrUndefined (isNullable && writable &&
                                propWriteNullable prop /= Just False)
           "clear" owner cName

  unless (readable || writable || constructOnly) $
       notImplementedError $ "Property is not readable, writable, or constructible: "
                               <> tshow pName

  group $ do
    line $ "-- VVV Prop \"" <> propName prop <> "\""
    line $ "   -- Type: " <> tshow (propType prop)
    line $ "   -- Flags: " <> tshow (propFlags prop)
    line $ "   -- Nullable: " <> tshow (propReadNullable prop,
                                        propWriteNullable prop)

  when readable $ genPropertyGetter owner pName prop
  when writable $ genPropertySetter owner pName prop
  when (writable || constructOnly) $ genPropertyConstructor pName prop
  when (isNullable && writable && propWriteNullable prop /= Just False) $
       genPropertyClear owner pName prop

  outType <- if not readable
             then return "()"
             else do
               sOutType <- if isNullable && propReadNullable prop /= Just False
                           then tshow . maybeT <$> haskellType (propType prop)
                           else tshow <$> haskellType (propType prop)
               return $ if T.any (== ' ') sOutType
                        then parenthesize sOutType
                        else sOutType

  -- Polymorphic _label style lens
  group $ do
    inIsGO <- isGObject (propType prop)
    hInType <- tshow <$> haskellType (propType prop)
    let inConstraint = if writable || constructOnly
                       then if inIsGO
                            then classConstraint hInType
                            else "(~) " <> if T.any (== ' ') hInType
                                           then parenthesize hInType
                                           else hInType
                       else "(~) ()"
        allowedOps = (if writable
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
    exportProperty cName it
    when (getter /= "undefined") (exportProperty cName getter)
    when (setter /= "undefined") (exportProperty cName setter)
    when (constructor /= "undefined") (exportProperty cName constructor)
    when (clear /= "undefined") (exportProperty cName clear)
    bline $ "data " <> it
    line $ "instance AttrInfo " <> it <> " where"
    indent $ do
            line $ "type AttrAllowedOps " <> it
                     <> " = '[ " <> T.intercalate ", " allowedOps <> "]"
            line $ "type AttrSetTypeConstraint " <> it
                     <> " = " <> inConstraint
            line $ "type AttrBaseTypeConstraint " <> it
                     <> " = " <> classConstraint name
            line $ "type AttrGetType " <> it <> " = " <> outType
            line $ "type AttrLabel " <> it <> " = \"" <> propName prop <> "\""
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
  exportProperty cName it
  line $ "data " <> it
  line $ "instance AttrInfo " <> it <> " where"
  indent $ do
    line $ "type AttrAllowedOps " <> it <> " = '[]"
    line $ "type AttrSetTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrBaseTypeConstraint " <> it <> " = (~) ()"
    line $ "type AttrGetType " <> it <> " = ()"
    line $ "type AttrLabel " <> it <> " = \"\""
    line $ "attrGet = undefined"
    line $ "attrSet = undefined"
    line $ "attrConstruct = undefined"
    line $ "attrClear = undefined"

genProperties :: Name -> [Property] -> [Text] -> CodeGen ()
genProperties n ownedProps allProps = do
  name <- upperName n

  forM_ ownedProps $ \prop -> do
      handleCGExc (\err -> do
                     line $ "-- XXX Generation of property \""
                              <> propName prop <> "\" of object \""
                              <> name <> "\" failed: " <> describeCGError err
                     genPlaceholderProperty n prop)
                  (genOneProperty n prop)

  group $ do
    let propListType = name <> "AttributeList"
    line $ "type instance AttributeList " <> name <> " = " <> propListType
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
  name <- upperName owner

  let methodNames = S.fromList (map (lowerName . methodName) methods)
      filteredAttrs = filter (`S.notMember` methodNames) attrNames

  forM_ filteredAttrs $ \attr -> group $ do
    let cName = ucFirst attr
        labelProxy = lcFirst name <> cName

    line $ labelProxy <> " :: AttrLabelProxy \"" <> lcFirst cName <> "\""
    line $ labelProxy <> " = AttrLabelProxy"

    exportProperty cName labelProxy
