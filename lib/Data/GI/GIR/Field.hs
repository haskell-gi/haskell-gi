-- | Parsing of object/struct/union fields.
module Data.GI.GIR.Field
    ( Field(..)
    , FieldInfoFlag
    , parseFields
    ) where

import Data.Monoid ((<>))
import Data.Text (Text, isSuffixOf)

import Data.GI.GIR.BasicTypes (Type(..))
import Data.GI.GIR.Callback (Callback, parseCallback)
import Data.GI.GIR.Type (parseType, parseCType)
import Data.GI.GIR.Parser

data Field = Field {
      fieldName :: Text,
      fieldVisible :: Bool,
      fieldType :: Type,
      fieldIsPointer :: Bool,
      fieldCallback :: Maybe Callback,
      fieldOffset :: Int,
      fieldFlags :: [FieldInfoFlag],
      fieldDeprecated :: Maybe DeprecationInfo }
    deriving Show

data FieldInfoFlag = FieldIsReadable | FieldIsWritable
                   deriving Show

-- | Parse a single field in a struct or union. We parse
-- non-introspectable fields too (but set fieldVisible = False for
-- them), this is necessary since they affect the computation of
-- offsets of fields and sizes of containing structs.
parseField :: Parser Field
parseField = do
  name <- getAttr "name"
  deprecated <- parseDeprecation
  readable <- optionalAttr "readable" True parseBool
  writable <- optionalAttr "writable" False parseBool
  let flags = if readable then [FieldIsReadable] else []
             <> if writable then [FieldIsWritable] else []
  introspectable <- optionalAttr "introspectable" True parseBool
  private <- optionalAttr "private" False parseBool
  (t, ctype, callback) <-
      if introspectable
      then do
        callbacks <- parseChildrenWithLocalName "callback" parseCallback
        (cbn, callback) <- case callbacks of
                             [] -> return (Nothing, Nothing)
                             [(n, cb)] -> return (Just n, Just cb)
                             _ -> parseError "Multiple callbacks in field"
        (t, ct) <- case cbn of
               Nothing -> do
                 t <- parseType
                 ct <- parseCType
                 return (t, Just ct)
               Just n -> return (TInterface n, Nothing)
        return (t, ct, callback)
      else do
        callbacks <- parseAllChildrenWithLocalName "callback" parseName
        case callbacks of
          [] -> do
               t <- parseType
               ct <- parseCType
               return (t, Just ct, Nothing)
          [n] -> return (TInterface n, Nothing, Nothing)
          _ -> parseError "Multiple callbacks in field"
  return $ Field {
               fieldName = name
             , fieldVisible = introspectable && not private
             , fieldType = t
             , fieldIsPointer = maybe True ("*" `isSuffixOf`) ctype
             , fieldCallback = callback
             , fieldOffset = error ("unfixed field offset " ++ show name)
             , fieldFlags = flags
             , fieldDeprecated = deprecated
          }

parseFields :: Parser [Field]
parseFields = parseAllChildrenWithLocalName "field" parseField
