{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of object/struct/union fields.
module GI.GIR.Field
    ( Field(..)
    , FieldInfoFlag
    , parseFields
    , computeFieldOffsets
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Monoid ((<>))
import Data.Text (Text)

import GI.Type (Type(..), typeSize, typeAlign)

import GI.GIR.Callback (Callback, parseCallback)
import GI.GIR.Type (parseType)
import GI.GIR.Parser

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

--- XXX We should also include the non-introspectable fields in the
--- computation of the sizes.
-- | Compute the offsets for the fields.
computeFieldOffsets :: [Field] -> ([Field], Int)
computeFieldOffsets fs =
    let fields = go 0 fs
        size = case fields of
                 [] -> 0
                 fs -> fieldOffset f + typeSize (fieldType f)
                     where f = last fs
    in (fields, size)

    where go :: Int -> [Field] -> [Field]
          go _ [] = []
          go off (f:fs) = let fadrs = align off (fieldType f)
                              next = fadrs + typeSize (fieldType f)
                          in f {fieldOffset = fadrs} : go next fs
          -- Align to the smallest multiple of the alignment for the C
          -- type larger or equal to the given offset.
          align :: Int -> Type -> Int
          align off t = let a = typeAlign t
                        in ((off + a - 1) `div` a) * a

parseField :: Parser Field
parseField = do
  name <- getAttr "name"
  deprecated <- parseDeprecation
  readable <- optionalAttr "readable" True parseBool
  writable <- optionalAttr "writable" False parseBool
  let flags = if readable then [FieldIsReadable] else []
             <> if writable then [FieldIsWritable] else []
  callbacks <- parseChildrenWithLocalName "callback" parseCallback
  (cbn, callback) <- case callbacks of
                       [] -> return (Nothing, Nothing)
                       [(n, cb)] -> return (Just n, Just cb)
                       _ -> parseError "Multiple callbacks in field"
  t <- case cbn of
         Nothing -> parseType
         Just (Name ns n) -> return (TInterface ns n)
  return $ Field {
               fieldName = name
             , fieldType = t
             , fieldCallback = callback
             , fieldOffset = 0          -- Fixed by computeOffsets
             , fieldFlags = flags
             , fieldDeprecated = deprecated
          }

parseFields :: Parser [Field]
parseFields = parseChildrenWithLocalName "field" parseField
