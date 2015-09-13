module GI.GIR.Property
    ( Property(..)
    , ParamFlag(..)
    ) where

import GI.Type (Type)
import GI.GIR.Deprecation (DeprecationInfo)

import GI.GIR.BasicTypes (Transfer)

data ParamFlag = ParamReadable
               | ParamWritable
               | ParamConstruct
               | ParamConstructOnly
               | ParamLaxValidation
               | ParamStaticName
               | ParamStaticNick
               | ParamStaticBlurb
               | ParamDeprecated
               deriving (Show,Eq)

instance Enum ParamFlag where
  fromEnum ParamReadable = 1
  fromEnum ParamWritable = 2
  fromEnum ParamConstruct = 4
  fromEnum ParamConstructOnly = 8
  fromEnum ParamLaxValidation = 16
  fromEnum ParamStaticName = 32
  fromEnum ParamStaticNick = 64
  fromEnum ParamStaticBlurb = 128
  fromEnum ParamDeprecated = 2147483648

  toEnum 1 = ParamReadable
  toEnum 2 = ParamWritable
  toEnum 4 = ParamConstruct
  toEnum 8 = ParamConstructOnly
  toEnum 16 = ParamLaxValidation
  toEnum 32 = ParamStaticName
  toEnum 64 = ParamStaticNick
  toEnum 128 = ParamStaticBlurb
  toEnum 2147483648 = ParamDeprecated
  toEnum n = error $ "Unknown param flag: " ++ show n

data Property = Property {
        propName :: String,
        propType :: Type,
        propFlags :: [ParamFlag],
        propTransfer :: Transfer,
        propDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)
