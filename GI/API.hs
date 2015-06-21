
module GI.API
    ( API(..)
    , Name(..)
    , Constant(..)
    , Arg(..)
    , Callable(..)
    , Function(..)
    , Signal(..)
    , Property(..)
    , Field(..)
    , Struct(..)
    , Callback(..)
    , Interface(..)
    , Object(..)
    , Enumeration(..)
    , Flags (..)
    , Union (..)

    , Scope(..) -- from GI.Internal.ArgInfo, for convenience

    , loadAPI
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Int
import Data.Maybe (isJust)

import GI.Internal.Types
import GI.Internal.ArgInfo
import GI.Internal.BaseInfo
import GI.Internal.CallableInfo
import GI.Internal.ConstantInfo
import GI.Internal.EnumInfo
import GI.Internal.FieldInfo
import GI.Internal.FunctionInfo
import GI.Internal.InterfaceInfo
import GI.Internal.ObjectInfo
import GI.Internal.PropertyInfo
import GI.Internal.RegisteredTypeInfo
import GI.Internal.StructInfo
import GI.Internal.TypeInfo
import GI.Internal.Typelib (getInfos, load)
import GI.Internal.UnionInfo
import GI.GType
import GI.Type

data Name = Name { namespace :: String, name :: String }
    deriving (Eq, Ord, Show)

getName :: InfoClass info => info -> Name
getName i = Name namespace name
    where namespace = infoNamespace i
          name = infoName i

withName :: InfoClass info => (info -> a) -> (info -> (Name, a))
withName f x = (getName x, f x)

data Constant = Constant {
      constantType      :: Type,
      constantValue     :: Argument,
      constDeprecated   :: Maybe String }
    deriving Show

toConstant :: ConstantInfo -> Constant
toConstant ci = Constant
    (typeFromTypeInfo $ constantInfoType ci)
    (constantInfoValue ci)
    (infoDeprecated ci)

data Enumeration = Enumeration {
    enumValues :: [(String, Int64)],
    errorDomain :: Maybe String,
    enumTypeInit :: Maybe String,
    enumStorageType :: TypeTag,
    enumDeprecated :: Maybe String }
    deriving Show

toEnumeration :: EnumInfo -> Enumeration
toEnumeration ei = Enumeration
    (map viToV $ enumInfoValues ei)
    (enumInfoErrorDomain ei)
    (registeredTypeInfoTypeInit ei)
    (enumInfoStorageType ei)
    (infoDeprecated ei)
    where viToV vi = (infoName vi, valueInfoValue vi)

data Flags = Flags Enumeration
    deriving Show

toFlags :: EnumInfo -> Flags
toFlags ei = Flags $ toEnumeration ei

data Arg = Arg {
    argName :: String,
    argType :: Type,
    direction :: Direction,
    mayBeNull :: Bool,
    argScope :: Scope,
    argClosure :: Int,
    argDestroy :: Int,
    transfer :: Transfer }
    deriving (Show, Eq, Ord)

toArg :: ArgInfo -> Arg
toArg ai = Arg
    (infoName ai)
    (typeFromTypeInfo . argInfoType $ ai)
    (argInfoDirection ai)
    (argInfoMayBeNull ai)
    (argInfoScope ai)
    (argInfoClosure ai)
    (argInfoDestroy ai)
    (argInfoOwnershipTransfer ai)

data Callable = Callable {
    returnType :: Type,
    returnMayBeNull :: Bool,
    returnTransfer :: Transfer,
    returnAttributes :: [(String, String)],
    args :: [Arg],
    skipReturn :: Bool,
    callableDeprecated :: Maybe String }
    deriving (Show, Eq)

toCallable :: CallableInfo -> Callable
toCallable ci = Callable
    (typeFromTypeInfo $ callableInfoReturnType ci)
    (callableInfoMayReturnNull ci)
    (callableInfoCallerOwns ci)
    (callableInfoReturnAttributes ci)
    (map toArg $ callableInfoArgs ci)
    (callableInfoSkipReturn ci)
    (infoDeprecated ci)

data Function = Function {
    fnSymbol :: String,
    fnCallable :: Callable,
    fnFlags :: [FunctionInfoFlag] }
    deriving Show

toFunction :: FunctionInfo -> Function
toFunction fi = Function
    (functionInfoSymbol fi)
    (toCallable ci)
    (functionInfoFlags fi)
    where ci = fromBaseInfo (baseInfo fi) :: CallableInfo

data Signal = Signal {
    sigName :: String,
    sigCallable :: Callable,
    sigDeprecated :: Maybe String }
    deriving (Show, Eq)

toSignal :: SignalInfo -> Signal
toSignal si = Signal
    (infoName si)
    (toCallable $ callableInfo si)
    (infoDeprecated si)

data Property = Property {
    propName :: String,
    propType :: Type,
    propFlags :: [ParamFlag],
    propTransfer :: Transfer,
    propDeprecated :: Maybe String }
    deriving (Show, Eq)

toProperty :: PropertyInfo -> Property
toProperty pi = Property
    (infoName pi)
    (typeFromTypeInfo $ propertyInfoType pi)
    (propertyInfoFlags pi)
    (propertyInfoTransfer pi)
    (infoDeprecated pi)

data Field = Field {
    fieldName :: String,
    fieldType :: Type,
    fieldCallback :: Maybe Callback,
    fieldOffset :: Int,
    fieldFlags :: [FieldInfoFlag],
    fieldDeprecated :: Maybe String }
    deriving Show

toField :: FieldInfo -> Field
toField fi = Field
    (infoName fi)
    (typeFromTypeInfo $ fieldInfoType fi)
     -- Fields with embedded "anonymous" callback interfaces.
    (case typeFromTypeInfo (fieldInfoType fi) of
       TInterface _ n ->
         if n /= infoName fi
         then Nothing
         else let iface = baseInfo . typeInfoInterface . fieldInfoType $ fi
              in case infoType iface of
                   InfoTypeCallback -> Just . toCallback . fromBaseInfo $ iface
                   _                -> Nothing
       _ -> Nothing)
    (fieldInfoOffset fi)
    (fieldInfoFlags fi)
    (infoDeprecated fi)

data Struct = Struct {
    structIsBoxed :: Bool,
    structTypeInit :: Maybe String,
    structSize :: Int,
    structIsForeign :: Bool,
    isGTypeStruct :: Bool,
    structFields :: [Field],
    structMethods :: [(Name, Function)],
    structDeprecated :: Maybe String }
    deriving Show

toStruct :: StructInfo -> Struct
toStruct si = Struct
    (isJust (registeredTypeInfoTypeInit si) &&
        gtypeIsBoxed (registeredTypeInfoGType si))
    (registeredTypeInfoTypeInit si)
    (structInfoSize si)
    (structInfoIsForeign si)
    (structInfoIsGTypeStruct si)
    (map toField $ structInfoFields si)
    (map (withName toFunction) (structInfoMethods si))
    (infoDeprecated si)

-- XXX: Capture alignment and method info.

data Union = Union {
    unionIsBoxed :: Bool,
    unionSize :: Int,
    unionTypeInit :: Maybe String,
    unionFields :: [Field],
    unionMethods :: [(Name, Function)],
    unionDeprecated :: Maybe String }
    deriving Show

toUnion :: UnionInfo -> Union
toUnion ui = Union
    (isJust (registeredTypeInfoTypeInit ui) &&
        gtypeIsBoxed (registeredTypeInfoGType ui))
    (unionInfoSize ui)
    (registeredTypeInfoTypeInit ui)
    (map toField $ unionInfoFields ui)
    (map (withName toFunction) (unionInfoMethods ui))
    (infoDeprecated ui)

-- XXX
data Callback = Callback Callable
    deriving Show

toCallback = Callback . toCallable

data Interface = Interface {
    ifConstants :: [(Name, Constant)],
    ifProperties :: [Property],
    ifSignals :: [Signal],
    ifPrerequisites :: [Name],
    ifTypeInit :: Maybe String,
    ifMethods :: [(Name, Function)],
    ifDeprecated :: Maybe String }
    deriving Show

toInterface :: InterfaceInfo -> Interface
toInterface ii = Interface
    (map (withName toConstant) (interfaceInfoConstants ii))
    (map toProperty (interfaceInfoProperties ii))
    (map toSignal (interfaceInfoSignals ii))
    (map (fst . toAPI) (interfaceInfoPrerequisites ii))
    (registeredTypeInfoTypeInit ii)
    (map (withName toFunction) (interfaceInfoMethods ii))
    (infoDeprecated ii)

data Object = Object {
    objFields :: [Field],
    objMethods :: [(Name, Function)],
    objProperties :: [Property],
    objSignals :: [Signal],
    objInterfaces :: [Name],
    objConstants :: [Constant],
    objParent :: Maybe Name,
    objTypeInit :: String,
    objTypeName :: String,
    objRefFunction :: Maybe String,
    objUnrefFunction :: Maybe String,
    objDeprecated :: Maybe String }
    deriving Show

toObject :: ObjectInfo -> Object
toObject oi = Object
    (map toField $ objectInfoFields oi)
    (map (withName toFunction) (objectInfoMethods oi))
    (map toProperty $ objectInfoProperties oi)
    (map toSignal (objectInfoSignals oi))
    (map getName $ objectInfoInterfaces oi)
    (map toConstant $ objectInfoConstants oi)
    (getName <$> objectInfoParent oi)
    (objectInfoTypeInit oi)
    (objectInfoTypeName oi)
    (objectInfoRefFunction oi)
    (objectInfoUnrefFunction oi)
    (infoDeprecated oi)

-- XXX: Work out what to do with boxed types.
data Boxed = Boxed
    deriving Show

toBoxed :: BaseInfo -> Boxed
toBoxed _ = Boxed

data API
    = APIConst Constant
    | APIFunction Function
    | APICallback Callback
    | APIEnum Enumeration
    | APIFlags Flags
    | APIInterface Interface
    | APIObject Object
    | APIStruct Struct
    | APIUnion Union
    | APIBoxed Boxed
    deriving Show

toAPI :: BaseInfo -> (Name, API)
toAPI bi = (getName bi, toAPI' (infoType bi) bi)
    where

    toAPI' InfoTypeConstant = convert APIConst toConstant
    toAPI' InfoTypeEnum = convert APIEnum toEnumeration
    toAPI' InfoTypeFlags = convert APIFlags toFlags
    toAPI' InfoTypeFunction = convert APIFunction toFunction
    toAPI' InfoTypeCallback = convert APICallback toCallback
    toAPI' InfoTypeStruct = convert APIStruct toStruct
    toAPI' InfoTypeUnion = convert APIUnion toUnion
    toAPI' InfoTypeObject = convert APIObject toObject
    toAPI' InfoTypeInterface = convert APIInterface toInterface
    toAPI' InfoTypeBoxed = convert APIBoxed toBoxed
    toAPI' it = error $ "not expecting a " ++ show it

    convert fa fb bi = fa $ fb $ fromBaseInfo bi

-- | Load the APIs in the given namespace.
loadAPI :: Bool -> String -> Maybe String -> IO [(Name, API)]
loadAPI verbose name version = do
    lib <- load name version verbose
    infos <- getInfos lib
    return $ map toAPI infos
