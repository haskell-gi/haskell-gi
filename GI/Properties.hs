module GI.Properties
    ( genInterfaceProperties
    , genObjectProperties
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, when, unless)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Foreign.Storable (sizeOf)
import Foreign.C (CInt, CUInt)

import GI.API
import GI.Conversions
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Type
import GI.Util
import GI.Internal.ArgInfo (Transfer(..))
import GI.Internal.ParamFlag (ParamFlag(..))

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
  (_,t,constraints) <- argumentType "abcdefghijklmn" $ propType prop
  if ' ' `elem` t
  then return (constraints, parenthesize t)
  else return (constraints, t)

genPropertySetter :: Name -> String -> Property -> CodeGen ()
genPropertySetter n pName prop = group $ do
  oName <- upperName n
  (constraints, t) <- attrType prop
  let constraints' = (classConstraint oName ++ " o"):constraints
  tStr <- propTypeStr $ propType prop
  line $ "set" ++ pName ++ " :: (" ++ intercalate ", " constraints'
           ++ ") => o -> " ++ t ++ " -> IO ()"
  line $ "set" ++ pName ++ " obj val = setObjectProperty" ++ tStr
           ++ " obj \"" ++ propName prop ++ "\" val"

genPropertyGetter :: Name -> String -> Property -> CodeGen ()
genPropertyGetter n pName prop = group $ do
  oName <- upperName n
  outType <- haskellType (propType prop)
  let constraints = "(" ++ classConstraint oName ++ " o)"
  line $ "get" ++ pName ++ " :: " ++ constraints ++
                " => o -> " ++ show (io outType)
  tStr <- propTypeStr $ propType prop
  line $ "get" ++ pName ++ " obj = getObjectProperty" ++ tStr
        ++ " obj \"" ++ propName prop ++ "\"" ++
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
           ++ " \"" ++ propName prop ++ "\" val"

genObjectProperties :: Name -> Object -> CodeGen ()
genObjectProperties n o = do
  isGO <- apiIsGObject n (APIObject o)
  -- We do not generate bindings for objects not descending from GObject.
  when isGO $ genProperties n (objProperties o)

genInterfaceProperties :: Name -> Interface -> CodeGen ()
genInterfaceProperties n iface = do
  isGO <- apiIsGObject n (APIInterface iface)
  if isGO
  then genProperties n (ifProperties iface)
  else do
    name <- upperName n
    group . line $ "type instance AttributeList " ++ name ++ " = '[]"

-- If the given accesor is available (indicated by available == True),
-- generate a fully qualified accesor name, otherwise just return
-- "undefined". accessor is "get", "set" or "construct"
accessorOrUndefined :: Bool -> String -> Name -> String -> CodeGen String
accessorOrUndefined available accessor (Name ons on) cName =
    if not available
    then return "undefined"
    else do
      prefix <- qualifyWithSuffix "A." ons
      return $ prefix ++ accessor ++ on ++ cName

-- | The name of the type encoding the information for the property of
-- the object.
infoType :: Name -> Property -> CodeGen String
infoType owner prop = do
  name <- upperName owner
  let cName = hyphensToCamelCase (propName prop)
  return $ name ++ cName ++ "PropertyInfo"

genOneProperty :: Name -> Property -> ExcCodeGen (Maybe String)
genOneProperty owner prop = do
  name <- upperName owner
  let cName = hyphensToCamelCase $ propName prop
      pName = name ++ cName
      flags = propFlags prop
      writable = ParamWritable `elem` flags &&
                 (ParamConstructOnly `notElem` flags)
      readable = ParamReadable `elem` flags
      constructOnly = ParamConstructOnly `elem` flags

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
    line $ "-- VVV Prop \"" ++ propName prop ++ "\""
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
                      then ["AttrSet", "AttrConstruct"]
                      else [])
                     <> (if constructOnly
                         then ["AttrConstruct"]
                         else [])
                     <> (if readable
                         then ["AttrGet"]
                         else [])
    it <- infoType owner prop

    line $ "data " ++ it
    line $ "instance AttrInfo " ++ it ++ " where"
    indent $ do
            line $ "type AttrAllowedOps " ++ it
                     ++ " = '[" ++ intercalate ", " allowedOps ++ "]"
            line $ "type AttrSetTypeConstraint " ++ it
                     ++ " = " ++ inConstraint
            line $ "type AttrBaseTypeConstraint " ++ it
                     ++ " = " ++ classConstraint name
            line $ "type AttrGetType " ++ it ++ " = " ++ outType
            line $ "type AttrLabel " ++ it ++ " = \""
                     ++ name ++ "::" ++ propName prop ++ "\""
            line $ "attrGet _ = " ++ getter
            line $ "attrSet _ = " ++ setter
            line $ "attrConstruct _ = " ++ constructor

    return . Just $ "'(\"" ++ propName prop ++ "\", " ++ it ++ ")"

genProperties :: Name -> [Property] -> CodeGen ()
genProperties n props = do
  name <- upperName n

  successfulProps <- forM props $ \prop -> do
      handleCGExc (\err -> do
                     line $ "-- XXX Generation of property \""
                              ++ propName prop ++ "\" of object \""
                              ++ name ++ "\" failed: " ++ describeCGError err
                     return Nothing )
                  (genOneProperty n prop)

  group $ line $ "type instance AttributeList " ++ name ++ " = '[ "
            ++ intercalate ", " (catMaybes successfulProps) ++ "]"
