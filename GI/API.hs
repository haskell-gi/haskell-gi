
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

import Data.Word
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
import GI.Value

import Control.Applicative ((<$>))

data Name = Name { namespace :: String, name :: String }
    deriving (Eq, Ord, Show)

getName :: BaseInfoClass bi => bi -> Name
getName bi =
   let namespace = baseInfoNamespace $ baseInfo bi
       name = baseInfoName $ baseInfo bi
    in Name namespace name

withName :: BaseInfoClass bi => (bi -> a) -> (bi -> (Name, a))
withName f x = (getName x, f x)

data Constant = Constant {
    constValue :: Value }
    deriving Show

toConstant :: ConstantInfo -> Constant
toConstant ci =
    let typeInfo = constantInfoType ci
        arg = constantInfoValue ci
        value = fromArgument typeInfo arg
     in Constant value

data Enumeration = Enumeration {
    enumValues :: [(String, Word64)],
    errorDomain :: Maybe String,
    enumTypeInit :: Maybe String,
    enumStorageType :: TypeTag }
    deriving Show

toEnumeration :: EnumInfo -> Enumeration
toEnumeration ei = Enumeration
    (map (\vi -> (baseInfoName . baseInfo $ vi, valueInfoValue vi))
             (enumInfoValues ei))
    (enumInfoErrorDomain ei)
    (registeredTypeInfoTypeInit ei)
    (enumInfoStorageType ei)

data Flags = Flags Enumeration
    deriving Show

toFlags :: EnumInfo -> Flags
toFlags ei = let enum = toEnumeration ei
              in Flags enum

data Arg = Arg {
    argName :: String,
    argType :: Type,
    direction :: Direction,
    mayBeNull :: Bool,
    argScope :: Scope,
    argClosure :: Int,
    argDestroy :: Int,
    transfer :: Transfer }
    deriving (Show, Eq)

toArg :: ArgInfo -> Arg
toArg ai =
   Arg (baseInfoName . baseInfo $ ai)
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
    skipReturn :: Bool }
    deriving Show

toCallable :: CallableInfo -> Callable
toCallable ci =
    let returnType = callableInfoReturnType ci
        argType = typeFromTypeInfo returnType
        ais = callableInfoArgs ci
        in Callable argType
               (callableInfoMayReturnNull ci)
               (callableInfoCallerOwns ci)
               (callableInfoReturnAttributes ci)
               (map toArg ais)
               (callableInfoSkipReturn ci)

data Function = Function {
    fnSymbol :: String,
    fnCallable :: Callable,
    fnFlags :: [FunctionInfoFlag] }
    deriving Show

toFunction :: FunctionInfo -> Function
toFunction fi =
     let ci = fromBaseInfo (baseInfo fi) :: CallableInfo
      in Function
         (functionInfoSymbol fi)
         (toCallable ci)
         (functionInfoFlags fi)

data Signal = Signal {
    sigCallable :: Callable }
    deriving Show

toSignal :: SignalInfo -> Signal
toSignal si = Signal {
    sigCallable = toCallable $ callableInfo si }

data Property = Property {
    propName :: String,
    propType :: Type,
    propFlags :: [ParamFlag],
    propTransfer :: Transfer }
    deriving (Show, Eq)

toProperty :: PropertyInfo -> Property
toProperty pi =
    Property (baseInfoName $ baseInfo pi)
        (typeFromTypeInfo $ propertyInfoType pi)
        (propertyInfoFlags pi)
        (propertyInfoTransfer pi)

data Field = Field {
    fieldName :: String,
    fieldType :: Type,
    fieldFlags :: [FieldInfoFlag] }
    deriving Show

toField :: FieldInfo -> Field
toField fi =
    Field (baseInfoName . baseInfo $ fi)
        (typeFromTypeInfo $ fieldInfoType fi)
        (fieldInfoFlags fi)

data Struct = Struct {
    structIsBoxed :: Bool,
    structTypeInit :: Maybe String,
    structSize :: Int,
    structIsForeign :: Bool,
    isGTypeStruct :: Bool,
    structFields :: [Field],
    structMethods :: [(Name, Function)]
    }
    deriving Show

toStruct :: StructInfo -> Struct
toStruct si = Struct {
                structIsBoxed = isJust (registeredTypeInfoTypeInit si) &&
                                gtypeIsBoxed (registeredTypeInfoGType si),
                structTypeInit = registeredTypeInfoTypeInit si,
                structFields = map toField $ structInfoFields si,
                structMethods = map (withName toFunction)
                                (structInfoMethods si),
                structSize = structInfoSize si,
                structIsForeign = structInfoIsForeign si,
                isGTypeStruct = structInfoIsGTypeStruct si }

-- XXX: Capture alignment and method info.

data Union = Union {
    unionIsBoxed :: Bool,
    unionSize :: Int,
    unionTypeInit :: Maybe String,
    unionFields :: [Field],
    unionMethods :: [(Name, Function)] }
    deriving Show

toUnion :: UnionInfo -> Union
toUnion ui = Union {
               unionIsBoxed = isJust (registeredTypeInfoTypeInit ui) &&
                              gtypeIsBoxed (registeredTypeInfoGType ui),
               unionSize = unionInfoSize ui,
               unionTypeInit = registeredTypeInfoTypeInit ui,
               unionFields = map toField $ unionInfoFields ui,
               unionMethods = map (withName toFunction)
                                (unionInfoMethods ui) }

-- XXX
data Callback = Callback Callable
    deriving Show

toCallback = Callback . toCallable

data Interface = Interface {
    ifConstants :: [(Name, Constant)],
    ifProperties :: [Property],
    ifPrerequisites :: [Name],
    ifTypeInit :: Maybe String,
    ifMethods :: [(Name, Function)] }
    deriving Show

toInterface :: InterfaceInfo -> Interface
toInterface ii = Interface {
    ifConstants = map (withName toConstant) (interfaceInfoConstants ii),
    ifProperties = map toProperty (interfaceInfoProperties ii),
    ifPrerequisites = map (fst . toAPI) (interfaceInfoPrerequisites ii),
    ifTypeInit = registeredTypeInfoTypeInit ii,
    ifMethods = map (withName toFunction) (interfaceInfoMethods ii) }

data Object = Object {
    objFields :: [Field],
    objMethods :: [(Name, Function)],
    objProperties :: [Property],
    objSignals :: [(Name, Signal)],
    objInterfaces :: [Name],
    objConstants :: [Constant],
    objParent :: Maybe Name,
    objTypeInit :: String,
    objTypeName :: String,
    objRefFunction :: Maybe String,
    objUnrefFunction :: Maybe String }
    deriving Show

toObject :: ObjectInfo -> Object
toObject oi = Object {
    objFields = map toField $ objectInfoFields oi,
    objMethods = map (withName toFunction) (objectInfoMethods oi),
    objProperties = map toProperty $ objectInfoProperties oi,
    objSignals = map (withName toSignal) (objectInfoSignals oi),
    objInterfaces = map getName $ objectInfoInterfaces oi,
    objConstants = map toConstant $ objectInfoConstants oi,
    objParent = getName <$> objectInfoParent oi,
    objTypeInit = objectInfoTypeInit oi,
    objTypeName = objectInfoTypeName oi,
    objRefFunction = objectInfoRefFunction oi,
    objUnrefFunction = objectInfoUnrefFunction oi }

-- XXX: Work out what to do with boxed types.
data Boxed = Boxed
    deriving Show

toBoxed :: BaseInfo -> Boxed
toBoxed _ = Boxed

data API
    = APIConst Constant
    | APIFunction Function
    | APICallback Callback
    -- XXX: These plus APIUnion should have their gTypes exposed (via a
    -- binding of GIRegisteredTypeInfo.
    | APIEnum Enumeration
    | APIFlags Flags
    | APIInterface Interface
    | APIObject Object
    | APIStruct Struct
    | APIUnion Union
    | APIBoxed Boxed
    deriving Show

toAPI :: BaseInfoClass bi => bi -> (Name, API)
toAPI i = (getName bi, toAPI' (baseInfoType i) bi)
    where

    bi = baseInfo i

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

loadAPI :: String -> IO [(Name, API)]
loadAPI name = do
    lib <- load name Nothing
    infos <- getInfos lib
    return $ map toAPI infos
