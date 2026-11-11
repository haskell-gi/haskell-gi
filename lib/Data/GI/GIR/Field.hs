-- | Parsing of object/struct/union fields.
module Data.GI.GIR.Field
    ( Field(..)
    , AnonymousMember(..)
    , FieldInfoFlag
    , parseFields
    ) where

import Control.Monad.Except (catchError, throwError)

import Data.Maybe (isJust, catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text, isSuffixOf)

import Data.GI.GIR.BasicTypes (Type(..), BasicType(..))
import Data.GI.GIR.Callback (Callback, parseCallback)
import Data.GI.GIR.Type (parseType, queryElementCType)
import Data.GI.GIR.Parser

data Field = Field {
      fieldName :: Text,
      fieldVisible :: Bool,
      fieldType :: Type,
      fieldIsPointer :: Maybe Bool, -- ^ `Nothing` if not known.
      fieldArrayOfPointers :: Bool,
      -- ^ For C arrays stored inline in the struct (that is, with a
      -- fixed size), whether the elements are pointers.
      fieldCallback :: Maybe Callback,
      fieldOffset :: Int,
      -- ^ For bitfields, this is the offset of the storage unit
      -- containing the bitfield.
      fieldBits :: Maybe Int,
      -- ^ The width in bits, if the field is a bitfield.
      fieldBitShift :: Maybe Int,
      -- ^ For bitfields, the position of the least significant bit
      -- of the bitfield inside its storage unit (a value of the
      -- declared type of the field, stored at `fieldOffset`). This
      -- is `Nothing` if the layout of the bitfield could not be
      -- determined.
      fieldAnonymous :: Maybe AnonymousMember,
      -- ^ If this member is an (anonymous type) union or struct
      -- declared inline, its contents. In this case `fieldType`
      -- is meaningless, and the field is never visible. We keep
      -- track of these members since they affect the layout of
      -- the containing struct or union.
      fieldFlags :: [FieldInfoFlag],
      fieldDocumentation :: Documentation,
      fieldDeprecated :: Maybe DeprecationInfo }
    deriving Show

data FieldInfoFlag = FieldIsReadable | FieldIsWritable
                   deriving Show

-- | Contents of a union or struct declared inline in a struct or
-- union.
data AnonymousMember = AnonymousUnion [Field]
                     | AnonymousStruct [Field]
                     deriving Show

-- | Parse a single field in a struct or union. We parse
-- non-introspectable fields too (but set fieldVisible = False for
-- them), this is necessary since they affect the computation of
-- offsets of fields and sizes of containing structs.
parseField :: Parser (Maybe Field)
parseField = do
  name <- getAttr "name"
  deprecated <- parseDeprecation
  bits <- optionalAttr "bits" Nothing (fmap Just . parseIntegral)
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
  flip catchError (\e -> if not introspectable
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

    elementCTypes <- parseAllChildrenWithLocalName "array" queryElementCType
    let arrayOfPointers = case elementCTypes of
          [Just ct] -> "*" `isSuffixOf` ct
          _ -> False

    return $ Just $ Field {
               fieldName = name
             , fieldVisible = introspectable && not private
             , fieldType = t
             , fieldIsPointer = if isJust callback
                                then Just True
                                else isPtr
             , fieldArrayOfPointers = arrayOfPointers
             , fieldCallback = callback
             , fieldOffset = error ("unfixed field offset " ++ show name)
             , fieldBits = if bits == Just 0 then Nothing else bits
             , fieldBitShift = Nothing
             , fieldAnonymous = Nothing
             , fieldFlags = flags
             , fieldDocumentation = doc
             , fieldDeprecated = deprecated
          }

-- | Parse an union or struct declared inline in a struct or union.
parseAnonymousMember :: Bool -> Parser Field
parseAnonymousMember isUnion = do
  name <- optionalAttr "name" (if isUnion
                               then "<anonymous union>"
                               else "<anonymous struct>") return
  -- We do not keep track of the offsets of the members of the
  -- anonymous union or struct, only of its total size.
  members <- map (\f -> f {fieldOffset = 0}) <$> parseFields
  doc <- parseDocumentation
  return $ Field {
               fieldName = name
             , fieldVisible = False
             , fieldType = TBasicType TPtr -- Not meaningful
             , fieldIsPointer = Just False
             , fieldArrayOfPointers = False
             , fieldCallback = Nothing
             , fieldOffset = 0
             , fieldBits = Nothing
             , fieldBitShift = Nothing
             , fieldAnonymous = Just (if isUnion
                                      then AnonymousUnion members
                                      else AnonymousStruct members)
             , fieldFlags = []
             , fieldDocumentation = doc
             , fieldDeprecated = Nothing
          }

-- | Parse the members of a struct or union, in order.
parseFields :: Parser [Field]
parseFields = catMaybes <$>
  parseAllChildrenWithLocalNames ["field", "union", "record"] parseMember
  where parseMember :: Text -> Parser (Maybe Field)
        parseMember "union" = Just <$> parseAnonymousMember True
        parseMember "record" = Just <$> parseAnonymousMember False
        parseMember _ = parseField
