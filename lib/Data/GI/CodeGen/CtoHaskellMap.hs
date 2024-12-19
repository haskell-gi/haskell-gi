-- | Construct a map from C identifiers to the corresponding Haskell
-- elements in the bindings.
module Data.GI.CodeGen.CtoHaskellMap
  ( cToHaskellMap
  , Hyperlink(..)
  ) where

import qualified Data.Map as M
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.CodeGen.GtkDoc (CRef(..), docName)
import Data.GI.CodeGen.API (API(..), Name(..), Callback(..),
                            Constant(..), Flags(..),
                            Enumeration(..), EnumerationMember(..),
                            Interface(..), Object(..),
                            Function(..), Method(..), Struct(..), Union(..),
                            Signal(..), Property(..))
import Data.GI.CodeGen.ModulePath (dotModulePath)
import Data.GI.CodeGen.SymbolNaming (moduleLocation, lowerName, upperName,
                                     signalHaskellName, haddockSignalAnchor,
                                     haddockAttrAnchor, hyphensToCamelCase)
import Data.GI.CodeGen.Util (ucFirst, lcFirst)

-- | Link to an identifier, module, etc.
data Hyperlink = ValueIdentifier Text
               -- ^ An identifier at the value level: functions, data
               -- constructors, ...
               | TypeIdentifier Text
               -- ^ An identifier at the type level.
               | ModuleLink Text
               -- ^ Link to a module.
               | ModuleLinkWithAnchor (Maybe Text) Text Text
               -- ^ Link to an anchor inside a given module, with an
               -- optional label.
  deriving (Show, Eq)

-- | Given a set of APIs, build a `Map` that given a Text
-- corresponding to a certain C identifier returns the corresponding
-- Haskell element in the bindings. For instance, `gtk_widget_show`
-- will get mapped to `GI.Gtk.Objects.Widget.show`.
cToHaskellMap :: [(Name, API)] -> M.Map CRef Hyperlink
cToHaskellMap apis = M.union (M.fromList builtins)
                     (M.fromList $ concatMap extractRefs apis)
  where extractRefs :: (Name, API) -> [(CRef, Hyperlink)]
        extractRefs (n, APIConst c) = constRefs n c
        extractRefs (n, APIFunction f) = funcRefs n f
        extractRefs (n, api@(APIEnum e)) = enumRefs api n e
        extractRefs (n, api@(APIFlags (Flags e))) = enumRefs api n e
        extractRefs (n, APICallback c) = callbackRefs n c
        extractRefs (n, APIStruct s) = structRefs n s
        extractRefs (n, APIUnion u) = unionRefs n u
        extractRefs (n, APIInterface i) = ifaceRefs n i
        extractRefs (n, APIObject o) = objectRefs n o

        builtins :: [(CRef, Hyperlink)]
        builtins = [(CTypeRef "gboolean", TypeIdentifier "P.Bool"),
                    (ConstantRef "TRUE", ValueIdentifier "P.True"),
                    (ConstantRef "FALSE", ValueIdentifier "P.False"),
                    (CTypeRef "GError", TypeIdentifier "GError"),
                    (CTypeRef "GType", TypeIdentifier "GType"),
                    (CTypeRef "GVariant", TypeIdentifier "GVariant"),
                    (ConstantRef "NULL", ValueIdentifier "P.Nothing"),
                    (TypeRef (docName $ Name "GLib" "Variant"),
                     TypeIdentifier "GVariant"),
                    (TypeRef (docName $ Name "GLib" "HashTable"),
                     TypeIdentifier "GHashTable")
                   ]

-- | Obtain the fully qualified symbol pointing to a value.
fullyQualifiedValue :: Name -> API -> Text -> Hyperlink
fullyQualifiedValue n api symbol =
  ValueIdentifier $ dotModulePath (moduleLocation n api) <> "." <> symbol

-- | Obtain the fully qualified symbol pointing to a type.
fullyQualifiedType :: Name -> API -> Text -> Hyperlink
fullyQualifiedType n api symbol =
  TypeIdentifier $ dotModulePath (moduleLocation n api) <> "." <> symbol

-- | Extract the C name of a constant. These are often referred to as
-- types, so we allow that too.
constRefs :: Name -> Constant -> [(CRef, Hyperlink)]
constRefs n c = [(ConstantRef (constantCType c), qualified),
                 (CTypeRef (constantCType c), qualified),
                 (TypeRef (docName n), qualified)]
  where qualified = fullyQualifiedValue n (APIConst c) $ name n

-- | Extract the C name of a function.
funcRefs :: Name -> Function -> [(CRef, Hyperlink)]
funcRefs n f = [(OldFunctionRef (fnSymbol f), qualified),
                (FunctionRef (docName n), qualified)]
  where qualified = fullyQualifiedValue n (APIFunction f) $ lowerName n

-- | Extract the C names of the fields in an enumeration/flags, and
-- the name of the type itself.
enumRefs :: API -> Name -> Enumeration -> [(CRef, Hyperlink)]
enumRefs api n e = (CTypeRef (enumCType e), qualified)
                   : (TypeRef (docName n), qualified)
                   : map memberToOldRef (enumMembers e)
                   <> map memberToRef (enumMembers e)
  where qualified = fullyQualifiedType n api $ upperName n
        memberToOldRef :: EnumerationMember -> (CRef, Hyperlink)
        memberToOldRef em = (ConstantRef (enumMemberCId em),
                          fullyQualifiedValue n api $ upperName $
                          n {name = name n <> "_" <> enumMemberName em})
        memberToRef :: EnumerationMember -> (CRef, Hyperlink)
        -- Sometimes the references are written in uppercase while the
        -- name of the member in the introspection data is written in
        -- lowercase, so normalise everything to lowercase. (See the
        -- similar annotation in GtkDoc.hs.)
        memberToRef em = (EnumMemberRef (docName n) (T.toLower $ enumMemberName em),
                          fullyQualifiedValue n api $ upperName $
                          n {name = name n <> "_" <> enumMemberName em})

-- | Refs to the methods for a given owner.
methodRefs :: Name -> API -> [Method] -> [(CRef, Hyperlink)]
methodRefs n api methods = concatMap methodRef methods
  where methodRef :: Method -> [(CRef, Hyperlink)]
        methodRef Method{methodSymbol = symbol, methodName = mn} =
          -- Method name namespaced by the owner.
          let mn' = mn {name = name n <> "_" <> name mn}
              qualified = fullyQualifiedValue n api $ lowerName mn'
          in [(OldFunctionRef symbol, qualified),
              (MethodRef (docName n) (name mn), qualified)]

-- | Refs to the signals for a given owner.
signalRefs :: Name -> API -> Maybe Text -> [Signal] -> [(CRef, Hyperlink)]
signalRefs n@(Name _ owner) api maybeCName signals = concatMap signalRef signals
  where signalRef :: Signal -> [(CRef, Hyperlink)]
        signalRef (Signal {sigName = sn}) =
          let mod = dotModulePath (moduleLocation n api)
              sn' = signalHaskellName sn
              ownerCName = case maybeCName of
                Just cname -> cname
                Nothing -> let Name ns owner = n
                           in ucFirst ns <> owner
              label = Just (owner <> "::" <> sn')
              link = ModuleLinkWithAnchor label mod (haddockSignalAnchor <> sn')
          in [(OldSignalRef ownerCName sn, link),
              (SignalRef (docName n) sn, link)]

-- | Refs to the properties for a given owner.
propRefs :: Name -> API -> Maybe Text -> [Property] -> [(CRef, Hyperlink)]
propRefs n@(Name _ owner) api maybeCName props = concatMap propertyRef props
  where propertyRef :: Property -> [(CRef, Hyperlink)]
        propertyRef (Property {propName = pn}) =
          let mod = dotModulePath (moduleLocation n api)
              hn = lcFirst . hyphensToCamelCase $ pn
              ownerCName = case maybeCName of
                Just cname -> cname
                Nothing -> let Name ns owner = n
                           in ucFirst ns <> owner
              label = Just (owner <> ":" <> hn)
              link = ModuleLinkWithAnchor label mod (haddockAttrAnchor <> hn)
          in [(OldPropertyRef ownerCName pn, link),
              (PropertyRef (docName n) pn, link)]

-- | Given an optional C type and the API constructor construct the
-- list of associated refs.
maybeCType :: Name -> API -> Maybe Text -> [(CRef, Hyperlink)]
maybeCType _ _ Nothing = []
maybeCType n api (Just ctype) = [(CTypeRef ctype, qualified),
                                 (TypeRef (docName n), qualified)]
  where qualified = fullyQualifiedType n api (upperName n)

-- | Extract the C name of a callback.
callbackRefs :: Name -> Callback -> [(CRef, Hyperlink)]
callbackRefs n cb = maybeCType n (APICallback cb) (cbCType cb)

-- | Extract the C references in a struct.
structRefs :: Name -> Struct -> [(CRef, Hyperlink)]
structRefs n s = maybeCType n (APIStruct s) (structCType s)
                 <> methodRefs n (APIStruct s) (structMethods s)

-- | Extract the C references in a union.
unionRefs :: Name -> Union -> [(CRef, Hyperlink)]
unionRefs n u = maybeCType n (APIUnion u) (unionCType u)
                 <> methodRefs n (APIUnion u) (unionMethods u)

-- | Extract the C references in an interface.
ifaceRefs :: Name -> Interface -> [(CRef, Hyperlink)]
ifaceRefs n i = maybeCType n (APIInterface i) (ifCType i)
                 <> methodRefs n (APIInterface i) (ifMethods i)
                 <> signalRefs n (APIInterface i) (ifCType i) (ifSignals i)
                 <> propRefs n (APIInterface i) (ifCType i) (ifProperties i)

-- | Extract the C references in an object.
objectRefs :: Name -> Object -> [(CRef, Hyperlink)]
objectRefs n o = maybeCType n (APIObject o) (objCType o)
                 <> methodRefs n (APIObject o) (objMethods o)
                 <> signalRefs n (APIObject o) (objCType o) (objSignals o)
                 <> propRefs n (APIObject o) (objCType o) (objProperties o)
