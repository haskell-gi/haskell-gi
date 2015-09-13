{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of struct/union fields.
module GI.GIR.Field
    ( Field(..)
    , FieldInfoFlag
    , parseField
    ) where

import Data.Text (Text)
import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext, Name)
import GI.GIR.Deprecation (DeprecationInfo)
import GI.Type (Type)

import GI.GIR.Callback (Callback)

data Field = Field {
    fieldName :: Text,
    fieldType :: Type,
    fieldCallback :: Maybe Callback,
    fieldOffset :: Int,
    fieldFlags :: [FieldInfoFlag],
    fieldDeprecated :: Maybe DeprecationInfo }
    deriving Show

data FieldInfoFlag = FieldIsReadable | FieldIsWritable
                   deriving Show

{-
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
-}

parseField :: ParseContext -> Element -> Maybe (Name, Field)
parseField _ _ = Nothing
