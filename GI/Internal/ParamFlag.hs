
module GI.Internal.ParamFlag (ParamFlag(..)) where

{-
This is a patched version of the code that c2hs generates for GParamFlags.
It works around the fact that G_PARAM_PRIVATE is an alias for
G_PARAM_STATIC_NAME.
-}

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
  toEnum unmatched = error ("ParamFlag.toEnum: Cannot match " ++ show unmatched)

