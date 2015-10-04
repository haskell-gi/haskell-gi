{-# LANGUAGE RecordWildCards, PatternGuards #-}
-- | Parsing type information from GIR files.
module GI.GIR.Type
    ( parseType
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Storable (sizeOf)
import Foreign.C (CShort, CUShort, CInt, CUInt, CLong, CULong, CSize)
import System.Posix.Types (CSsize)

import GI.Type (Type(..), BasicType(..))
import GI.GIR.Parser

-- | Map the given type name to a BasicType (in GI.Type), if possible.
nameToBasicType :: Text -> Maybe BasicType
nameToBasicType "none"     = Just TVoid
-- XXX Should we try to distinguish TPtr from TVoid?
nameToBasicType "gpointer" = Just TVoid
nameToBasicType "gboolean" = Just TBoolean
nameToBasicType "gchar"    = Just TInt8
nameToBasicType "gint8"    = Just TInt8
nameToBasicType "guint8"   = Just TUInt8
nameToBasicType "gint16"   = Just TInt16
nameToBasicType "guint16"  = Just TUInt16
nameToBasicType "gint32"   = Just TInt32
nameToBasicType "guint32"  = Just TUInt32
nameToBasicType "gint64"   = Just TInt64
nameToBasicType "guint64"  = Just TUInt64
nameToBasicType "gfloat"   = Just TFloat
nameToBasicType "gdouble"  = Just TDouble
nameToBasicType "gunichar" = Just TUniChar
nameToBasicType "GType"    = Just TGType
nameToBasicType "utf8"     = Just TUTF8
nameToBasicType "filename" = Just TFileName
nameToBasicType "gshort"   = case sizeOf (0 :: CShort) of
                               2 -> Just TInt16
                               4 -> Just TInt32
                               8 -> Just TInt64
                               n -> error $ "Unexpected short size: " ++ show n
nameToBasicType "gushort"  = case sizeOf (0 :: CUShort) of
                               2 -> Just TUInt16
                               4 -> Just TUInt32
                               8 -> Just TUInt64
                               n -> error $ "Unexpected ushort size: " ++ show n
nameToBasicType "gint"     = case sizeOf (0 :: CInt) of
                               4 -> Just TInt32
                               8 -> Just TInt64
                               n -> error $ "Unexpected int length: " ++ show n
nameToBasicType "guint"    = case sizeOf (0 :: CUInt) of
                               4 -> Just TUInt32
                               8 -> Just TUInt64
                               n -> error $ "Unexpected uint length: " ++ show n
nameToBasicType "glong"    = case sizeOf (0 :: CLong) of
                               4 -> Just TInt32
                               8 -> Just TInt64
                               n -> error $ "Unexpected long length: " ++ show n
nameToBasicType "gulong"   = case sizeOf (0 :: CULong) of
                               4 -> Just TUInt32
                               8 -> Just TUInt64
                               n -> error $ "Unexpected ulong length: " ++ show n
nameToBasicType "gssize"   = case sizeOf (0 :: CSsize) of
                               4 -> Just TInt32
                               8 -> Just TInt64
                               n -> error $ "Unexpected ssize length: " ++ show n
nameToBasicType "gsize"    = case sizeOf (0 :: CSize) of
                               4 -> Just TUInt32
                               8 -> Just TUInt64
                               n -> error $ "Unexpected size length: " ++ show n
nameToBasicType _          = Nothing

-- | The different array types.
parseArrayInfo :: Parser Type
parseArrayInfo = queryAttr "name" >>= \case
      Just "GLib.Array" -> TGArray <$> parseType
      Just "GLib.PtrArray" -> TPtrArray <$> parseType
      Just "GLib.ByteArray" -> return TByteArray
      Just other -> parseError $ "Unsupported array type: \"" <> other <> "\""
      Nothing -> parseCArrayType

-- | A C array
parseCArrayType :: Parser Type
parseCArrayType = do
  zeroTerminated <- queryAttr "zero-terminated" >>= \case
                    Just b -> parseBool b
                    Nothing -> return True
  length <- queryAttr "length" >>= \case
            Just l -> parseIntegral l
            Nothing -> return (-1)
  fixedSize <- queryAttr "fixed-size" >>= \case
               Just s -> parseIntegral s
               Nothing -> return (-1)
  elementType <- parseType
  return $ TCArray zeroTerminated fixedSize length elementType

-- | A hash table.
parseHashTable :: Parser Type
parseHashTable = parseTypeElements >>= \case
                 [key, value] -> return $ TGHash key value
                 other -> parseError $ "Unsupported hash type: "
                                       <> T.pack (show other)

-- | For GLists and GSLists there is sometimes no information about
-- the type of the elements. In these cases we report them as
-- pointers.
parseListType :: Parser Type
parseListType = queryType >>= \case
                Just t -> return t
                Nothing -> return (TBasicType TVoid)

-- | A type which is not a BasicType or array.
parseFundamentalType :: Text -> Text -> Parser Type
parseFundamentalType "GLib" "List" = TGList <$> parseListType
parseFundamentalType "GLib" "SList" = TGSList <$> parseListType
parseFundamentalType "GLib" "HashTable" = parseHashTable
parseFundamentalType "GLib" "Error" = return TError
parseFundamentalType "GLib" "Variant" = return TVariant
parseFundamentalType "GObject" "ParamSpec" = return TParamSpec
-- A TInterface type (basically, everything that is not of a known type).
parseFundamentalType ns n = resolveQualifiedTypeName ns n

-- | Parse information on a "type" element.
parseTypeInfo :: Parser Type
parseTypeInfo = do
  typeName <- getAttr "name"
  case nameToBasicType typeName of
    Just b -> return (TBasicType b)
    Nothing -> case T.split ('.' ==) typeName of
                 [ns, n] -> parseFundamentalType ns n
                 [n] -> do
                   ns <- currentNamespace
                   parseFundamentalType ns n
                 _ -> parseError $ "Unsupported type form: \""
                                   <> typeName <> "\""

-- | Find the children giving the type of the given element.
parseTypeElements :: Parser [Type]
parseTypeElements = (++) <$> parseChildrenWithLocalName "type" parseTypeInfo <*>
                    parseChildrenWithLocalName "array" parseArrayInfo

-- | Try to find a type node, but do not error out if it is not
-- found. This _does_ give an error if more than one type node is
-- found.
queryType :: Parser (Maybe Type)
queryType = parseTypeElements >>= \case
            [e] -> return (Just e)
            [] -> return Nothing
            _ -> parseError $ "Found more than one type for the element."

-- | Parse the type of a node (which will be described by a child node
-- named "type" or "array").
parseType :: Parser Type
parseType = parseTypeElements >>= \case
            [e] -> return e
            [] -> parseError $ "Did not find a type for the element."
            _ -> parseError $ "Found more than one type for the element."
