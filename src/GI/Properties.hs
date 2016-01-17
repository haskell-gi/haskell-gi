module GI.Properties
    ( genInterfaceProperties
    , genObjectProperties
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when, unless)
import Data.List (intercalate)
import qualified Data.Text as T

import Foreign.Storable (sizeOf)
import Foreign.C (CInt, CUInt)

import GI.API
import GI.Conversions
import GI.Code
import GI.GObject
import GI.Inheritance (fullObjectPropertyList, fullInterfacePropertyList)
import GI.SymbolNaming (upperName, classConstraint, qualify, hyphensToCamelCase)
import GI.Type
import GI.Util

propTypeStr :: Type -> CodeGen String
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

-- Given a property, return the set of constraints on the types, and
-- the type variables for the object and its value.
attrType :: Property -> CodeGen ([String], String)
attrType prop = do
  (_,t,constraints) <- argumentType ['a'..'l'] $ propType prop
  if ' ' `elem` t
  then return (constraints, parenthesize t)
  else return (constraints, t)

genPropertySetter :: Name -> String -> Property -> CodeGen ()
genPropertySetter n pName prop = group $ do
  oName <- upperName n
  (constraints, t) <- attrType prop
  let constraints' = "MonadIO m":(classConstraint oName ++ " o"):constraints
  tStr <- propTypeStr $ propType prop
  line $ "set" ++ pName ++ " :: (" ++ intercalate ", " constraints'
           ++ ") => o -> " ++ t ++ " -> m ()"
  line $ "set" ++ pName ++ " obj val = liftIO $ setObjectProperty" ++ tStr
           ++ " obj \"" ++ T.unpack (propName prop) ++ "\" val"

genPropertyGetter :: Name -> String -> Property -> CodeGen ()
genPropertyGetter n pName prop = group $ do
  oName <- upperName n
  outType <- haskellType (propType prop)
  let constraints = "(MonadIO m, " ++ classConstraint oName ++ " o)"
  line $ "get" ++ pName ++ " :: " ++ constraints ++
                " => o -> " ++ show ("m" `con` [outType])
  tStr <- propTypeStr $ propType prop
  line $ "get" ++ pName ++ " obj = liftIO $ getObjectProperty" ++ tStr
        ++ " obj \"" ++ T.unpack (propName prop) ++ "\"" ++
           if tStr `elem` ["Object", "Boxed"]
           then " " ++ show outType -- These require the constructor too.
           else ""

genPropertyConstructor :: String -> Property -> CodeGen ()
genPropertyConstructor pName prop = group $ do
  (constraints, t) <- attrType prop
  tStr <- propTypeStr $ propType prop
  let constraints' =
          case constraints of
            [] -> ""
            _ -> parenthesize (intercalate ", " constraints) ++ " => "
  line $ "construct" ++ pName ++ " :: " ++ constraints'
           ++ t ++ " -> IO ([Char], GValue)"
  line $ "construct" ++ pName ++ " val = constructObjectProperty" ++ tStr
           ++ " \"" ++ T.unpack (propName prop) ++ "\" val"

genObjectProperties :: Name -> Object -> CodeGen ()
genObjectProperties n o = do
  isGO <- apiIsGObject n (APIObject o)
  -- We do not generate bindings for objects not descending from GObject.
  when isGO $ do
    allProps <- fullObjectPropertyList n o >>=
                mapM (\(owner, prop) -> do
                        pi <- infoType owner prop
                        return $ "'(\"" ++ T.unpack (propName prop)
                                   ++ "\", " ++ pi ++ ")")
    genProperties n (objProperties o) allProps

genInterfaceProperties :: Name -> Interface -> CodeGen ()
genInterfaceProperties n iface = do
  allProps <- fullInterfacePropertyList n iface >>=
                mapM (\(owner, prop) -> do
                        pi <- infoType owner prop
                        return $ "'(\"" ++ T.unpack (propName prop)
                                   ++ "\", " ++ pi ++ ")")
  genProperties n (ifProperties iface) allProps

-- If the given accesor is available (indicated by available == True),
-- generate a fully qualified accesor name, otherwise just return
-- "undefined". accessor is "get", "set" or "construct"
accessorOrUndefined :: Bool -> String -> Name -> String -> CodeGen String
accessorOrUndefined available accessor (Name ons on) cName =
    if not available
    then return "undefined"
    else do
      prefix <- qualify ons
      return $ prefix ++ accessor ++ on ++ cName

-- | The name of the type encoding the information for the property of
-- the object.
infoType :: Name -> Property -> CodeGen String
infoType owner prop = do
  name <- upperName owner
  let cName = (hyphensToCamelCase . T.unpack . propName) prop
  return $ name ++ cName ++ "PropertyInfo"

genOneProperty :: Name -> Property -> ExcCodeGen ()
genOneProperty owner prop = do
  name <- upperName owner
  let cName = (hyphensToCamelCase . T.unpack . propName) prop
      pName = name ++ cName
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
       notImplementedError $ "Property " ++ pName
                               ++ " has unsupported transfer type "
                               ++ show (propTransfer prop)

  getter <- accessorOrUndefined readable "get" owner cName
  setter <- accessorOrUndefined writable "set" owner cName
  constructor <- accessorOrUndefined (writable || constructOnly)
                 "construct" owner cName

  unless (readable || writable || constructOnly) $
       notImplementedError $ "Property is not readable, writable, or constructible: "
                               ++ show pName

  group $ do
    line $ "-- VVV Prop \"" ++ T.unpack (propName prop) ++ "\""
    line $ "   -- Type: " ++ show (propType prop)
    line $ "   -- Flags: " ++ show (propFlags prop)

  when readable $ genPropertyGetter owner pName prop
  when writable $ genPropertySetter owner pName prop
  when (writable || constructOnly) $ genPropertyConstructor pName prop

  outType <- if not readable
             then return "()"
             else do
               sOutType <- show <$> haskellType (propType prop)
               return $ if ' ' `elem` sOutType
                        then parenthesize sOutType
                        else sOutType

  -- Polymorphic _label style lens
  group $ do
    inIsGO <- isGObject (propType prop)
    hInType <- show <$> haskellType (propType prop)
    let inConstraint = if writable || constructOnly
                       then if inIsGO
                            then classConstraint hInType
                            else "(~) " ++ if ' ' `elem` hInType
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
    it <- infoType owner prop
    export it
    line $ "data " ++ it
    line $ "instance AttrInfo " ++ it ++ " where"
    indent $ do
            line $ "type AttrAllowedOps " ++ it
                     ++ " = '[ " ++ intercalate ", " allowedOps ++ "]"
            line $ "type AttrSetTypeConstraint " ++ it
                     ++ " = " ++ inConstraint
            line $ "type AttrBaseTypeConstraint " ++ it
                     ++ " = " ++ classConstraint name
            line $ "type AttrGetType " ++ it ++ " = " ++ outType
            line $ "type AttrLabel " ++ it ++ " = \""
                     ++ name ++ "::" ++ T.unpack (propName prop) ++ "\""
            line $ "attrGet _ = " ++ getter
            line $ "attrSet _ = " ++ setter
            line $ "attrConstruct _ = " ++ constructor

-- | Generate a placeholder property for those cases in which code
-- generation failed.
genPlaceholderProperty :: Name -> Property -> CodeGen ()
genPlaceholderProperty owner prop = do
  line $ "-- XXX Placeholder"
  it <- infoType owner prop
  export it
  line $ "data " ++ it
  line $ "instance AttrInfo " ++ it ++ " where"
  indent $ do
    line $ "type AttrAllowedOps " ++ it ++ " = '[]"
    line $ "type AttrSetTypeConstraint " ++ it ++ " = (~) ()"
    line $ "type AttrBaseTypeConstraint " ++ it ++ " = (~) ()"
    line $ "type AttrGetType " ++ it ++ " = ()"
    line $ "type AttrLabel " ++ it ++ " = \"\""
    line $ "attrGet = undefined"
    line $ "attrSet = undefined"
    line $ "attrConstruct = undefined"

genProperties :: Name -> [Property] -> [String] -> CodeGen ()
genProperties n ownedProps allProps = do
  name <- upperName n

  forM_ ownedProps $ \prop -> do
      handleCGExc (\err -> do
                     line $ "-- XXX Generation of property \""
                              ++ T.unpack (propName prop) ++ "\" of object \""
                              ++ name ++ "\" failed: " ++ describeCGError err
                     genPlaceholderProperty n prop)
                  (genOneProperty n prop)

  group $ line $ "type instance AttributeList " ++ name ++ " = '[ "
            ++ intercalate ", " allProps ++ "]"
