module GI.Properties
    ( genInterfaceProperties
    , genObjectProperties
    ) where

import Control.Monad (forM_, when, foldM)
import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import qualified Data.Map as M

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
   TBasicType TInt32 -> do
     -- This should work for all systems in common use, but rather
     -- than leaving the assumption implicit better double checking.
     when (sizeOf (0 :: CInt) /= 4) $
          error $ "C Integers are not 4 bytes, unsupported platform."
     return "CInt"
   TBasicType TUInt32 -> do
     when (sizeOf (0 :: CUInt) /= 4) $
          error $ "C Integers are not 4 bytes, unsupported platform."
     return "CUInt"
   TBasicType TInt64 -> return "Int64"
   TBasicType TUInt64 -> return "UInt64"
   TBasicType TBoolean -> return "Bool"
   TBasicType TFloat -> return "Float"
   TBasicType TDouble -> return "Double"
   TCArray True _ _ (TBasicType TUTF8) -> return "StringArray"
   TCArray True _ _ (TBasicType TFileName) -> return "StringArray"
   t@(TInterface ns n) -> do
     api <- findAPIByName (Name ns n)
     case api of
       APIEnum _ -> return "Enum"
       APIFlags _ -> return "Flags"
       APIStruct s -> if structIsBoxed s
                      then return "Boxed"
                      else if t == TInterface "GLib" "Variant"
                           then return "Variant"
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
  let constraints' = (goConstraint oName ++ " o"):constraints
  tStr <- propTypeStr $ propType prop
  line $ "set" ++ pName ++ " :: (" ++ intercalate ", " constraints'
           ++ ") => o -> " ++ t ++ " -> IO ()"
  line $ "set" ++ pName ++ " obj val = setObjectProperty" ++ tStr
           ++ " obj \"" ++ propName prop ++ "\" val"

genPropertyGetter :: Name -> String -> Property -> CodeGen ()
genPropertyGetter n pName prop = group $ do
  oName <- upperName n
  outType <- haskellType (propType prop)
  let constraints = "(" ++ goConstraint oName ++ " o)"
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

-- Returns a list of all properties defined for this object (including
-- those defined by its ancestors and the interfaces it implements),
-- together with the name of the interface defining the property.
apiProps :: Name -> CodeGen [(Name, Property)]
apiProps n = do
  api <- findAPIByName n
  case api of
    APIInterface iface -> return $ map ((,) n) (ifProperties iface)
    APIObject object -> return $ map ((,) n) (objProperties object)
    _ -> error $ "apiProps : Unexpected API : " ++ show n

fullAPIPropertyList :: Name -> CodeGen [(Name, Property)]
fullAPIPropertyList n = do
  api <- findAPIByName n
  case api of
    APIInterface iface -> fullInterfacePropertyList n iface
    APIObject object -> fullObjectPropertyList n object
    _ -> error $ "FullAPIPropertyList : Unexpected API : " ++ show n

fullObjectPropertyList :: Name -> Object -> CodeGen [(Name, Property)]
fullObjectPropertyList n obj = do
  cfg <- config
  (++) <$> (concat <$> (mapM apiProps $ [n] ++ instanceTree (instances cfg) n))
       <*> (concat <$> (mapM apiProps $ objInterfaces obj))

fullInterfacePropertyList :: Name -> Interface -> CodeGen [(Name, Property)]
fullInterfacePropertyList n iface =
    ((++) $ map ((,) n) (ifProperties iface))
       <$> (concat <$> (mapM fullAPIPropertyList $ ifPrerequisites iface))

genObjectProperties :: Name -> Object -> CodeGen ()
genObjectProperties n o = fullObjectPropertyList n o >>= genProperties n

genInterfaceProperties :: Name -> Interface -> CodeGen ()
genInterfaceProperties n iface =
    fullInterfacePropertyList n iface >>= genProperties n

-- It is sometimes the case that a property name is defined both in an
-- object and in one of its ancestors/implemented interfaces. This is
-- harmless if the properties are isomorphic (there will be more than
-- one qualified set of property setters/getters that we can call, but
-- they are all isomorphi). If they are not isomorphic we refuse to
-- set either, and print a warning in the generated code.
removeDuplicateProps :: [(Name, Property)] -> CodeGen [(Name, Property)]
removeDuplicateProps props =
    (filterTainted . M.toList) <$> foldM filterDups M.empty props
    where
      filterDups :: M.Map String (Bool, Name, Property) -> (Name, Property) ->
                    CodeGen (M.Map String (Bool, Name, Property))
      filterDups m (name, prop) =
        case M.lookup (propName prop) m of
          Just (tainted, n, p) ->
              if tainted
              then return m
              else if p == prop
                   then return m -- Duplicated, but isomorphic property
                   else do
                     line $ "--- XXX Property duplicated with different types:"
                     line $ "  --- " ++ show n ++ " -> " ++ show p
                     line $ "  --- " ++ show name ++ " -> " ++ show prop
                     -- Tainted property
                     return $ M.insert (propName prop) (True, n, p) m
          Nothing -> return $ M.insert (propName prop) (False, name, prop) m
      filterTainted :: [(String, (Bool, Name, Property))] -> [(Name, Property)]
      filterTainted xs =
          [(name, prop) | (_, (tainted, name, prop)) <- xs, not tainted]

genProperties :: Name -> [(Name, Property)] -> CodeGen ()
genProperties n props = do
  name <- upperName n

  props' <- removeDuplicateProps props

  forM_ props' $ \(propOwner@(Name ons on), prop) -> do
    propOwnerName <- upperName propOwner
    let cName = hyphensToCamelCase $ propName prop
        pName = propOwnerName ++ cName
        flags = propFlags prop
        writable = ParamWritable `elem` flags &&
                   not (ParamConstructOnly `elem` flags)
        readable = ParamReadable `elem` flags
        constructOnly = ParamConstructOnly `elem` flags
        owned = propOwner == n -- Whether n is the object which
                               -- defined the property, will be False
                               -- for properties inherited from parent
                               -- classes.

    getter <- do
      prefix <- qualify ons
      if readable
      then return $ prefix ++ "get" ++ on ++ cName
      else return "undefined"
    setter <- do
      prefix <- qualify ons
      if writable
      then return $ prefix ++ "set" ++ on ++ cName
      else return "undefined"
    constructor <- do
      prefix <- qualify ons
      if (writable || constructOnly)
      then return $ prefix ++ "construct" ++ on ++ cName
      else return "undefined"

    when owned $ do
      when (not $ readable || writable || constructOnly) $
           error $ "Property is not readable, writable, or constructible: "
                     ++ show pName

      group $ do
        line $ "-- VVV Prop \"" ++ propName prop ++ "\""
        line $ "   -- Type: " ++ show (propType prop)
        line $ "   -- Flags: " ++ show (propFlags prop)

      -- For properties the meaning of having transfer /=
      -- TransferNothing is not totally clear (what are the right
      -- semantics for GValue setters?), and the other possibilities
      -- are in any case unused for Gtk at least, so let us just
      -- assume that TransferNothing is always the case.
      when (propTransfer prop /= TransferNothing) $
         error $ "Property " ++ pName ++ " has unsupported transfer type "
                   ++ show (propTransfer prop)

      when readable $ genPropertyGetter n pName prop
      when writable $ genPropertySetter n pName prop
      when (writable || constructOnly) $ genPropertyConstructor pName prop

    outType <- if not readable
               then return "()"
               else do
                 sOutType <- show <$> haskellType (propType prop)
                 return $ if ' ' `elem` sOutType
                          then parenthesize sOutType
                          else sOutType

    qualifiedLens <- do
      prefix <- qualify ons
      return $ prefix ++ lcFirst on ++ cName

    when owned $ group $ do
      line $ qualifiedLens ++ " :: "
               ++ parenthesize (goConstraint name ++ " o")
               ++ " => Attr \"" ++ cName ++ "\" o w"
      line $ qualifiedLens ++ " = undefined"

    -- Polymorphic _label style lens
    group $ do
      inIsGO <- isGObject (propType prop)
      hInType <- show <$> (haskellType $ propType prop)
      let inConstraint = if writable || constructOnly
                         then if inIsGO
                              then goConstraint hInType
                              else "(~) " ++ if ' ' `elem` hInType
                                             then parenthesize hInType
                                             else hInType
                         else "(~) ()"
          instanceVars = "\"" ++ cName ++ "\" " ++ name
          attrWriteType = if writable
                          then "w"
                          else "NonWritableAttr"

      line $ "instance HasAttr " ++ instanceVars ++ " where"
      indent $ do
              line $ "type AttrIsReadable " ++ instanceVars
                       ++ " = " ++ show readable
              line $ "type AttrIsConstructible " ++ instanceVars
                       ++ " = " ++ show (writable || constructOnly)
              line $ "type AttrGetType " ++ instanceVars
                       ++ " = " ++ outType
              line $ "type " ++ "AttrSetConstraint " ++ instanceVars
                       ++ " = " ++ inConstraint
              line $ "attrLabel _ _ = \"" ++ name ++ ":" ++ cName ++ "\""
              line $ "attrGet _ = " ++ getter
              line $ "attrSet _ = " ++ setter
              line $ "attrConstruct _ = " ++ constructor

      blank

      line $ "instance HasProperty"
               ++ cName ++ " " ++ name ++ " " ++ attrWriteType ++ " where"
      indent $ do
              line $ "_" ++ lcFirst cName ++ " = " ++ qualifiedLens
