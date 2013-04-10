module GI.Internal.Types
  (
    BaseInfo(..)
  , BaseInfoClass(..)
  , CallableInfo(..)
  , CallableInfoClass(..)
  , FunctionInfo(..)
  , FunctionInfoClass(..)
  , SignalInfo(..)
  , SignalInfoClass(..)
  , VFuncInfo(..)
  , VFuncInfoClass(..)
  , RegisteredTypeInfo(..)
  , RegisteredTypeInfoClass(..)
  , EnumInfo(..)
  , EnumInfoClass(..)
  , InterfaceInfo(..)
  , InterfaceInfoClass(..)
  , ObjectInfo(..)
  , ObjectInfoClass(..)
  , StructInfo(..)
  , StructInfoClass(..)
  , UnionInfo(..)
  , UnionInfoClass(..)
  , ArgInfo(..)
  , ArgInfoClass(..)
  , ConstantInfo(..)
  , ConstantInfoClass(..)
  , FieldInfo(..)
  , FieldInfoClass(..)
  , PropertyInfo(..)
  , PropertyInfoClass(..)
  , TypeInfo(..)
  , TypeInfoClass(..)
  , ValueInfo(..)
  , ValueInfoClass(..)
  , Argument(..)
  , AttributeIter(..)
  )
where

import Foreign

#include <girepository.h>

{# pointer *GIArgument as Argument newtype #}
{# pointer *GIAttributeIter as AttributeIter newtype #}

{# pointer *GIBaseInfo as BaseInfo newtype #}
{# class BaseInfoClass BaseInfo #}

{# pointer *GICallableInfo as CallableInfo newtype #}
{# class CallableInfoClass CallableInfo #}
instance BaseInfoClass CallableInfo where
  baseInfo (CallableInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = CallableInfo (castPtr p)

{# pointer *GIFunctionInfo as FunctionInfo newtype #}
{# class FunctionInfoClass FunctionInfo #}
instance BaseInfoClass FunctionInfo where
  baseInfo (FunctionInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = FunctionInfo (castPtr p)
instance CallableInfoClass FunctionInfo where
  callableInfo (FunctionInfo p) = CallableInfo (castPtr p)
  fromCallableInfo (CallableInfo p) = FunctionInfo (castPtr p)

{# pointer *GISignalInfo as SignalInfo newtype #}
{# class SignalInfoClass SignalInfo #}
instance BaseInfoClass SignalInfo where
  baseInfo (SignalInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = SignalInfo (castPtr p)
instance CallableInfoClass SignalInfo where
  callableInfo (SignalInfo p) = CallableInfo (castPtr p)
  fromCallableInfo (CallableInfo p) = SignalInfo (castPtr p)

{# pointer *GIVFuncInfo as VFuncInfo newtype #}
{# class VFuncInfoClass VFuncInfo #}
instance BaseInfoClass VFuncInfo where
  baseInfo (VFuncInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = VFuncInfo (castPtr p)
instance CallableInfoClass VFuncInfo where
  callableInfo (VFuncInfo p) = CallableInfo (castPtr p)
  fromCallableInfo (CallableInfo p) = VFuncInfo (castPtr p)

{# pointer *GIRegisteredTypeInfo as RegisteredTypeInfo newtype #}
{# class RegisteredTypeInfoClass RegisteredTypeInfo #}
instance BaseInfoClass RegisteredTypeInfo where
  baseInfo (RegisteredTypeInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = RegisteredTypeInfo (castPtr p)

{# pointer *GIEnumInfo as EnumInfo newtype #}
{# class EnumInfoClass EnumInfo #}
instance BaseInfoClass EnumInfo where
  baseInfo (EnumInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = EnumInfo (castPtr p)
instance RegisteredTypeInfoClass EnumInfo where
  registeredTypeInfo (EnumInfo p) = RegisteredTypeInfo (castPtr p)
  fromRegisteredTypeInfo (RegisteredTypeInfo p) = EnumInfo (castPtr p)

{# pointer *GIInterfaceInfo as InterfaceInfo newtype #}
{# class InterfaceInfoClass InterfaceInfo #}
instance BaseInfoClass InterfaceInfo where
  baseInfo (InterfaceInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = InterfaceInfo (castPtr p)
instance RegisteredTypeInfoClass InterfaceInfo where
  registeredTypeInfo (InterfaceInfo p) = RegisteredTypeInfo (castPtr p)
  fromRegisteredTypeInfo (RegisteredTypeInfo p) = InterfaceInfo (castPtr p)

{# pointer *GIObjectInfo as ObjectInfo newtype #}
{# class ObjectInfoClass ObjectInfo #}
instance BaseInfoClass ObjectInfo where
  baseInfo (ObjectInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = ObjectInfo (castPtr p)
instance RegisteredTypeInfoClass ObjectInfo where
  registeredTypeInfo (ObjectInfo p) = RegisteredTypeInfo (castPtr p)
  fromRegisteredTypeInfo (RegisteredTypeInfo p) = ObjectInfo (castPtr p)

{# pointer *GIStructInfo as StructInfo newtype #}
{# class StructInfoClass StructInfo #}
instance BaseInfoClass StructInfo where
  baseInfo (StructInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = StructInfo (castPtr p)
instance RegisteredTypeInfoClass StructInfo where
  registeredTypeInfo (StructInfo p) = RegisteredTypeInfo (castPtr p)
  fromRegisteredTypeInfo (RegisteredTypeInfo p) = StructInfo (castPtr p)

{# pointer *GIUnionInfo as UnionInfo newtype #}
{# class UnionInfoClass UnionInfo #}
instance BaseInfoClass UnionInfo where
  baseInfo (UnionInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = UnionInfo (castPtr p)
instance RegisteredTypeInfoClass UnionInfo where
  registeredTypeInfo (UnionInfo p) = RegisteredTypeInfo (castPtr p)
  fromRegisteredTypeInfo (RegisteredTypeInfo p) = UnionInfo (castPtr p)

{# pointer *GIArgInfo as ArgInfo newtype #}
{# class ArgInfoClass ArgInfo #}
instance BaseInfoClass ArgInfo where
  baseInfo (ArgInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = ArgInfo (castPtr p)

{# pointer *GIConstantInfo as ConstantInfo newtype #}
{# class ConstantInfoClass ConstantInfo #}
instance BaseInfoClass ConstantInfo where
  baseInfo (ConstantInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = ConstantInfo (castPtr p)

{# pointer *GIFieldInfo as FieldInfo newtype #}
{# class FieldInfoClass FieldInfo #}
instance BaseInfoClass FieldInfo where
  baseInfo (FieldInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = FieldInfo (castPtr p)

{# pointer *GIPropertyInfo as PropertyInfo newtype #}
{# class PropertyInfoClass PropertyInfo #}
instance BaseInfoClass PropertyInfo where
  baseInfo (PropertyInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = PropertyInfo (castPtr p)

{# pointer *GITypeInfo as TypeInfo newtype #}
{# class TypeInfoClass TypeInfo #}
instance BaseInfoClass TypeInfo where
  baseInfo (TypeInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = TypeInfo (castPtr p)

{# pointer *GIValueInfo as ValueInfo newtype #}
{# class ValueInfoClass ValueInfo #}
instance BaseInfoClass ValueInfo where
  baseInfo (ValueInfo p) = BaseInfo (castPtr p)
  fromBaseInfo (BaseInfo p) = ValueInfo (castPtr p)

