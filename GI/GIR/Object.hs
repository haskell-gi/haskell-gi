{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of objects.
module GI.GIR.Object
    ( Object(..)
    , parseObject
    ) where

import Data.Text (Text)
import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext, Name)
import GI.GIR.Deprecation (DeprecationInfo)

import GI.GIR.Constant (Constant)
import GI.GIR.Field (Field)
import GI.GIR.Function (Function)
import GI.GIR.Property (Property)
import GI.GIR.Signal (Signal)

data Object = Object {
    objFields :: [Field],
    objMethods :: [(Name, Function)],
    objProperties :: [Property],
    objSignals :: [Signal],
    objInterfaces :: [Name],
    objConstants :: [Constant],
    objParent :: Maybe Name,
    objTypeInit :: Text,
    objTypeName :: Text,
    objRefFunction :: Maybe Text,
    objUnrefFunction :: Maybe Text,
    objDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
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
-}

parseObject :: ParseContext -> Element -> Maybe (Name, Object)
parseObject _ _ = Nothing

