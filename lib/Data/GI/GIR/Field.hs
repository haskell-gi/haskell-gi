-- | Parsing of object/struct/union fields.
module Data.GI.GIR.Field
    ( Field(..)
    , FieldInfoFlag
    , parseFields
    ) where

import Control.Monad.Except (catchError, throwError)

import Data.Maybe (isJust, catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text, isSuffixOf)

import Data.GI.GIR.BasicTypes (Type(..))
import Data.GI.GIR.Callback (Callback, parseCallback)
import Data.GI.GIR.Type (parseType, queryElementCType)
import Data.GI.GIR.Parser

data Field = Field {
      fieldName :: Text,
      fieldVisible :: Bool,
      fieldType :: Type,
      fieldIsPointer :: Maybe Bool, -- ^ `Nothing` if not known.
      fieldCallback :: Maybe Callback,
      fieldOffset :: Int,
      fieldFlags :: [FieldInfoFlag],
      fieldDocumentation :: Documentation,
      fieldDeprecated :: Maybe DeprecationInfo }
    deriving Show

data FieldInfoFlag = FieldIsReadable | FieldIsWritable
                   deriving Show

-- | Parse a single field in a struct or union. We parse
-- non-introspectable fields too (but set fieldVisible = False for
-- them), this is necessary since they affect the computation of
-- offsets of fields and sizes of containing structs.
parseField :: Parser (Maybe Field)
parseField = do
  name <- getAttr "name"
  deprecated <- parseDeprecation
  readable <- optionalAttr "readable" True parseBool
  writable <- optionalAttr "writable" False parseBool
  let flags = if readable then [FieldIsReadable] else []
             <> if writable then [FieldIsWritable] else []
  introspectable <- optionalAttr "introspectable" True parseBool
  private <- optionalAttr "private" False parseBool
  doc <- parseDocumentation
  -- Sometimes fields marked as not introspectable contain invalid
  -- introspection info. We are lenient in these cases with parsing
  -- errors, and simply ignore the fields.
  flip catchError (\e -> if (not introspectable) && private
                         then return Nothing
                         else throwError e) $ do
    (t, isPtr, callback) <-
      if introspectable
      then do
        callbacks <- parseChildrenWithLocalName "callback" parseCallback
        (cbn, callback) <- case callbacks of
                             [] -> return (Nothing, Nothing)
                             [(n, cb)] -> return (Just n, Just cb)
                             _ -> parseError "Multiple callbacks in field"
        (t, isPtr) <- case cbn of
               Nothing -> do
                 t <- parseType
                 ct <- queryElementCType
                 return (t, fmap ("*" `isSuffixOf`) ct)
               Just n -> return (TInterface n, Nothing)
        return (t, isPtr, callback)
      else do
        callbacks <- parseAllChildrenWithLocalName "callback" parseName
        case callbacks of
          [] -> do
               t <- parseType
               ct <- queryElementCType
               return (t, fmap ("*" `isSuffixOf`) ct, Nothing)
          [n] -> return (TInterface n, Just True, Nothing)
          _ -> parseError "Multiple callbacks in field"

    return $ Just $ Field {
               fieldName = name
             , fieldVisible = introspectable && not private
             , fieldType = t
             , fieldIsPointer = if isJust callback
                                then Just True
                                else isPtr
             , fieldCallback = callback
             , fieldOffset = error ("unfixed field offset " ++ show name)
             , fieldFlags = flags
             , fieldDocumentation = doc
             , fieldDeprecated = deprecated
          }

parseFields :: Parser [Field]
parseFields = catMaybes <$> parseAllChildrenWithLocalName "field" parseField
