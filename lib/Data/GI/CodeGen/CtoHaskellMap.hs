-- | Construct a map from C identifiers to the corresponding Haskell
-- elements in the bindings.
module Data.GI.CodeGen.CtoHaskellMap
  ( cToHaskellMap
  ) where

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)

import Data.GI.CodeGen.GtkDoc (CRef(..))
import Data.GI.CodeGen.API (API(..), Name(..), Callback(..),
                            Constant(..), Flags(..),
                            Enumeration(..), EnumerationMember(..),
                            Interface(..), Object(..),
                            Function(..), Method(..), Struct(..), Union(..))
import Data.GI.CodeGen.ModulePath (ModulePath, dotModulePath, (/.))
import Data.GI.CodeGen.SymbolNaming (submoduleLocation, lowerName, upperName)
import Data.GI.CodeGen.Util (ucFirst)

-- | Given a set of APIs, build a `Map` that given a Text
-- corresponding to a certain C identifier returns the corresponding
-- Haskell element in the bindings. For instance, `gtk_widget_show`
-- will get mapped to `GI.Gtk.Objects.Widget.show`.
cToHaskellMap :: [(Name, API)] -> M.Map CRef Text
cToHaskellMap apis = M.union (M.fromList builtins)
                     (M.fromList $ concatMap extractRefs apis)
  where extractRefs :: (Name, API) -> [(CRef, Text)]
        extractRefs (n, APIConst c) = constRefs n c
        extractRefs (n, APIFunction f) = funcRefs n f
        extractRefs (n, api@(APIEnum e)) = enumRefs api n e
        extractRefs (n, api@(APIFlags (Flags e))) = enumRefs api n e
        extractRefs (n, APICallback c) = callbackRefs n c
        extractRefs (n, APIStruct s) = structRefs n s
        extractRefs (n, APIUnion u) = unionRefs n u
        extractRefs (n, APIInterface i) = ifaceRefs n i
        extractRefs (n, APIObject o) = objectRefs n o

        builtins :: [(CRef, Text)]
        builtins = [(TypeRef "gboolean", "Bool"),
                    (ConstantRef "TRUE", "True"),
                    (ConstantRef "FALSE", "False"),
                    (TypeRef "GError", "GError"),
                    (TypeRef "GType", "GType"),
                    (TypeRef "GVariant", "GVariant"),
                    (ConstantRef "NULL", "Nothing")]

-- | Obtain the absolute location of the module where the given `API`
-- lives.
location :: Name -> API -> ModulePath
location n api = ("GI" /. ucFirst (namespace n)) <> submoduleLocation n api

-- | Obtain the fully qualified symbol.
fullyQualified :: Name -> API -> Text -> Text
fullyQualified n api symbol = dotModulePath (location n api) <> "." <> symbol

-- | Extract the C name of a constant. These are often referred to as
-- types, so we allow that too.
constRefs :: Name -> Constant -> [(CRef, Text)]
constRefs n c = [(ConstantRef (constantCType c),
                  fullyQualified n (APIConst c) $ name n),
                 (TypeRef (constantCType c),
                  fullyQualified n (APIConst c) $ name n)]

-- | Extract the C name of a function.
funcRefs :: Name -> Function -> [(CRef, Text)]
funcRefs n f = [(FunctionRef (fnSymbol f),
                 fullyQualified n (APIFunction f) $ lowerName n)]

-- | Extract the C names of the fields in an enumeration/flags, and
-- the name of the type itself.
enumRefs :: API -> Name -> Enumeration -> [(CRef, Text)]
enumRefs api n e = (TypeRef (enumCType e), fullyQualified n api $ upperName n) :
                   map memberToRef (enumMembers e)
  where memberToRef :: EnumerationMember -> (CRef, Text)
        memberToRef em = (ConstantRef (enumMemberCId em),
                          fullyQualified n api $ upperName $
                          n {name = name n <> "_" <> enumMemberName em})

-- | Given an optional C type and the API constructor construct the
-- list of associated refs.
maybeCType :: Name -> API -> Maybe Text -> [(CRef, Text)]
maybeCType _ _ Nothing = []
maybeCType n api (Just ctype) = [(TypeRef ctype,
                                  fullyQualified n api (upperName n))]

-- | Refs to the methods for a given owner.
methodRefs :: Name -> API -> [Method] -> [(CRef, Text)]
methodRefs n api methods = map methodRef methods
  where methodRef :: Method -> (CRef, Text)
        methodRef m@(Method {methodName = mn}) =
          -- Method name namespaced by the owner.
          let mn' = mn {name = name n <> "_" <> name mn}
          in (FunctionRef (methodSymbol m),
              fullyQualified n api $ lowerName mn')

-- | Extract the C name of a callback.
callbackRefs :: Name -> Callback -> [(CRef, Text)]
callbackRefs n cb = maybeCType n (APICallback cb) (cbCType cb)

-- | Extract the C references in a struct.
structRefs :: Name -> Struct -> [(CRef, Text)]
structRefs n s = maybeCType n (APIStruct s) (structCType s)
                 <> methodRefs n (APIStruct s) (structMethods s)

-- | Extract the C references in a union.
unionRefs :: Name -> Union -> [(CRef, Text)]
unionRefs n u = maybeCType n (APIUnion u) (unionCType u)
                 <> methodRefs n (APIUnion u) (unionMethods u)

-- | Extract the C references in an interface.
ifaceRefs :: Name -> Interface -> [(CRef, Text)]
ifaceRefs n i = maybeCType n (APIInterface i) (ifCType i)
                 <> methodRefs n (APIInterface i) (ifMethods i)

-- | Extract the C references in an object.
objectRefs :: Name -> Object -> [(CRef, Text)]
objectRefs n o = maybeCType n (APIObject o) (objCType o)
                 <> methodRefs n (APIObject o) (objMethods o)
