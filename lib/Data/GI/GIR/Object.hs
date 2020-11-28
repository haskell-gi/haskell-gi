-- | Parsing of objects.
module Data.GI.GIR.Object
    ( Object(..)
    , parseObject
    ) where

import Data.Text (Text)

import Data.GI.GIR.Method (Method, parseMethod, MethodType(..))
import Data.GI.GIR.Property (Property, parseProperty)
import Data.GI.GIR.Signal (Signal, parseSignal)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (queryCType)

data Object = Object {
    objParent :: Maybe Name,
    objTypeInit :: Text,
    objTypeName :: Text,
    objCType :: Maybe Text,
    objRefFunc :: Maybe Text,
    objUnrefFunc :: Maybe Text,
    objSetValueFunc :: Maybe Text,
    objGetValueFunc :: Maybe Text,
    objInterfaces :: [Name],
    objDeprecated :: Maybe DeprecationInfo,
    objDocumentation :: Documentation,
    objMethods :: [Method],
    objProperties :: [Property],
    objSignals :: [Signal]
    } deriving Show

parseObject :: Parser (Name, Object)
parseObject = do
  name <- parseName
  deprecated <- parseDeprecation
  doc <- parseDocumentation
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  functions <- parseChildrenWithLocalName "function" (parseMethod MemberFunction)
  parent <- optionalAttr "parent" Nothing (fmap Just . qualifyName)
  interfaces <- parseChildrenWithLocalName "implements" parseName
  props <- parseChildrenWithLocalName "property" parseProperty
  typeInitFn <- getAttrWithNamespace GLibGIRNS "get-type"
  typeInit <- case typeInitFn of
                "intern" -> resolveInternalType name
                fn -> return fn
  typeName <- getAttrWithNamespace GLibGIRNS "type-name"
  signals <- parseChildrenWithNSName GLibGIRNS "signal" parseSignal
  refFunc <- queryAttrWithNamespace GLibGIRNS "ref-func"
  unrefFunc <- queryAttrWithNamespace GLibGIRNS "unref-func"
  setValueFunc <- queryAttrWithNamespace GLibGIRNS "set-value-func"
  getValueFunc <- queryAttrWithNamespace GLibGIRNS "get-value-func"

  ctype <- queryCType
  return (name,
         Object {
            objParent = parent
          , objTypeInit = typeInit
          , objCType = ctype
          , objRefFunc = refFunc
          , objUnrefFunc = unrefFunc
          , objSetValueFunc = setValueFunc
          , objGetValueFunc = getValueFunc
          , objTypeName = typeName
          , objInterfaces = interfaces
          , objDeprecated = deprecated
          , objDocumentation = doc
          , objMethods = constructors ++ methods ++ functions
          , objProperties = props
          , objSignals = signals
          })

-- | Some basic types do not list a type init function, and instead
-- mention "intern". Provide the explicit numerical value of the GType
-- in these cases.
resolveInternalType :: Name -> Parser Text
resolveInternalType (Name "GObject" p@"ParamSpec") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecBoolean") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecBoxed") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecChar") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecDouble") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecEnum") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecFlags") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecFloat") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecGType") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecInt") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecInt64") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecLong") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecObject") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecOverride") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecParam") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecPointer") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecString") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecUChar") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecUInt") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecUInt64") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecULong") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecUnichar") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecVariant") = pspec_type_init p
resolveInternalType (Name "GObject" p@"ParamSpecValueArray") = pspec_type_init p
resolveInternalType (Name ns n) =
  parseError $ "Unknown internal type: " <> ns <> "." <> n <> "\n"
                <> "This is a bug, please report at https://github.com/haskell-gi/haskell-gi/issues"

-- | The name of the function we provide for querying ParamSpec types
-- at runtime.
pspec_type_init :: Text -> Parser Text
pspec_type_init p = return $ "haskell_gi_pspec_type_init_" <> p
