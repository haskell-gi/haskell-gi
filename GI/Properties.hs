module GI.Properties
    ( genProperties
    ) where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))
import Data.List (intercalate)

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
   TBasicType TBoolean -> return "Bool"
   TBasicType TFloat -> return "CFloat"
   TBasicType TDouble -> return "CDouble"
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
  (_,t,constraints) <- argumentType "abcdefghijklmn" $ propType prop
  let t' = if ' ' `elem` t
           then parenthesize t
           else t
  tStr <- propTypeStr $ propType prop
  let constraints' =
          case constraints of
            [] -> ""
            _ -> parenthesize (intercalate ", " constraints) ++ " => "
  line $ "construct" ++ pName ++ " :: " ++ constraints'
           ++ t' ++ " -> IO ([Char], GValuePtr)"
  line $ "construct" ++ pName ++ " val = constructObjectProperty" ++ tStr
           ++ " \"" ++ propName prop ++ "\" val"

genProperties :: Name -> [Property] -> CodeGen ()
genProperties n props = do
  name <- upperName n

  forM_ props $ \prop -> do
    let pName = name ++ hyphensToCamelCase (propName prop)
        flags = propFlags prop
        writable = ParamWritable `elem` flags &&
                   not (ParamConstructOnly `elem` flags)
        readable = ParamReadable `elem` flags
        constructOnly = ParamConstructOnly `elem` flags

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
         error $ "Property " ++  pName ++ " has unsupported transfer type "
                   ++ show (propTransfer prop)

    let getter = "get" ++ pName
        setter = "set" ++ pName
        constructor = "construct" ++ pName

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

    (constraints, inType) <- if not (writable || constructOnly)
                             then return ([], "()")
                             else attrType prop

    let constraints' = (goConstraint name ++ " o") : constraints
        lens = lcFirst pName

    let (setterFns, writeType, attrWriteType) =
            if constructOnly
            then if readable
                 then ([constructor], "C", "ConstructibleAttr")
                 else ([constructor], "O", "ConstructibleAttr")
            else if writable
                 then if readable
                      then ([setter, constructor], "W", "w")
                      else ([setter, constructor], "O", "w")
                 else ([], "O", "ReadOnlyAttr")
        (getterFns, readType, attrReadType) =
            if readable
            then ([getter], "R", "ReadableAttr")
            else if constructOnly
                 then ([], "C", "UnreadableAttr")
                 else ([], "W", "UnreadableAttr")
        attrConstructor = readType ++ writeType ++ "Attr"

    group $ do
      line $ lens ++ " :: " ++ parenthesize (intercalate ", " constraints')
               ++ " => Attr o " ++ outType ++ " " ++ inType
                      ++ " " ++ attrReadType ++ " " ++ attrWriteType
      line $ lens ++ " = " ++ attrConstructor ++ " \"" ++ propName prop ++ "\" "
               ++ intercalate " " (getterFns ++ setterFns)
