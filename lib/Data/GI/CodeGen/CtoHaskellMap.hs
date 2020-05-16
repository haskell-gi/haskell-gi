-- | Construct a map from C identifiers to the corresponding Haskell
-- elements in the bindings.
module Data.GI.CodeGen.CtoHaskellMap
  ( cToHaskellMap
  , Hyperlink(..)
  ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Data.GI.CodeGen.GtkDoc (CRef(..))
import Data.GI.CodeGen.API (API(..), Name(..), Callback(..),
                            Constant(..), Flags(..),
                            Enumeration(..), EnumerationMember(..),
                            Interface(..), Object(..),
                            Function(..), Method(..), Struct(..), Union(..),
                            Signal(..))
import Data.GI.CodeGen.ModulePath (ModulePath, dotModulePath, (/.))
import Data.GI.CodeGen.SymbolNaming (submoduleLocation, lowerName, upperName,
                                     signalHaskellName)
import Data.GI.CodeGen.Util (ucFirst)

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
        builtins = [(TypeRef "gboolean", TypeIdentifier "P.Bool"),
                    (ConstantRef "TRUE", ValueIdentifier "P.True"),
                    (ConstantRef "FALSE", ValueIdentifier "P.False"),
                    (TypeRef "GError", TypeIdentifier "GError"),
                    (TypeRef "GType", TypeIdentifier "GType"),
                    (TypeRef "GVariant", TypeIdentifier "GVariant"),
                    (ConstantRef "NULL", ValueIdentifier "P.Nothing")]

-- | Obtain the absolute location of the module where the given `API`
-- lives.
location :: Name -> API -> ModulePath
location n api = ("GI" /. ucFirst (namespace n)) <> submoduleLocation n api

-- | Obtain the fully qualified symbol pointing to a value.
fullyQualifiedValue :: Name -> API -> Text -> Hyperlink
fullyQualifiedValue n api symbol =
  ValueIdentifier $ dotModulePath (location n api) <> "." <> symbol

-- | Obtain the fully qualified symbol pointing to a type.
fullyQualifiedType :: Name -> API -> Text -> Hyperlink
fullyQualifiedType n api symbol =
  TypeIdentifier $ dotModulePath (location n api) <> "." <> symbol

-- | Extract the C name of a constant. These are often referred to as
-- types, so we allow that too.
constRefs :: Name -> Constant -> [(CRef, Hyperlink)]
constRefs n c = [(ConstantRef (constantCType c),
                  fullyQualifiedValue n (APIConst c) $ name n),
                 (TypeRef (constantCType c),
                  fullyQualifiedValue n (APIConst c) $ name n)]

-- | Extract the C name of a function.
funcRefs :: Name -> Function -> [(CRef, Hyperlink)]
funcRefs n f = [(FunctionRef (fnSymbol f),
                 fullyQualifiedValue n (APIFunction f) $ lowerName n)]

-- | Extract the C names of the fields in an enumeration/flags, and
-- the name of the type itself.
enumRefs :: API -> Name -> Enumeration -> [(CRef, Hyperlink)]
enumRefs api n e = (TypeRef (enumCType e),
                    fullyQualifiedType n api $ upperName n) :
                   map memberToRef (enumMembers e)
  where memberToRef :: EnumerationMember -> (CRef, Hyperlink)
        memberToRef em = (ConstantRef (enumMemberCId em),
                          fullyQualifiedValue n api $ upperName $
                          n {name = name n <> "_" <> enumMemberName em})

-- | Refs to the methods for a given owner.
methodRefs :: Name -> API -> [Method] -> [(CRef, Hyperlink)]
methodRefs n api methods = catMaybes $ map methodRef methods
  where methodRef :: Method -> Maybe (CRef, Hyperlink)
        methodRef Method{methodSymbol = symbol, methodName = mn} =
          -- Method name namespaced by the owner.
          let mn' = mn {name = name n <> "_" <> name mn}
          in Just (FunctionRef symbol,
                   fullyQualifiedValue n api $ lowerName mn')

-- | Refs to the signals for a given owner.
signalRefs :: Name -> API -> Maybe Text -> [Signal] -> [(CRef, Hyperlink)]
signalRefs n api maybeCName signals = map signalRef signals
  where signalRef :: Signal -> (CRef, Hyperlink)
        signalRef (Signal {sigName = sn}) =
          let mod = dotModulePath (location n api)
              sn' = signalHaskellName sn
              ownerCName = case maybeCName of
                Just cname -> cname
                Nothing -> let Name ns owner = n
                           in ucFirst ns <> owner
          in (SignalRef ownerCName sn,
              ModuleLinkWithAnchor (Just sn') mod ("g:signal:" <> sn'))

-- | Given an optional C type and the API constructor construct the
-- list of associated refs.
maybeCType :: Name -> API -> Maybe Text -> [(CRef, Hyperlink)]
maybeCType _ _ Nothing = []
maybeCType n api (Just ctype) = [(TypeRef ctype,
                                  fullyQualifiedType n api (upperName n))]

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

-- | Extract the C references in an object.
objectRefs :: Name -> Object -> [(CRef, Hyperlink)]
objectRefs n o = maybeCType n (APIObject o) (objCType o)
                 <> methodRefs n (APIObject o) (objMethods o)
                 <> signalRefs n (APIObject o) (objCType o) (objSignals o)
