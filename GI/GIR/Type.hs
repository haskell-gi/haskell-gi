{-# LANGUAGE RecordWildCards, OverloadedStrings, PatternGuards #-}
-- | Parsing type information from GIR files.
module GI.GIR.Type
    ( parseType
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative ((<|>))

import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Storable (sizeOf)
import Foreign.C (CInt, CUInt, CLong, CULong, CSize)
import Text.XML (Element(elementAttributes))

import GI.Type (Type(..), BasicType(..))
import GI.GIR.BasicTypes (Alias(..), ParseContext(ParseContext, knownAliases,
                                                  currentNamespace))
import GI.GIR.XMLUtils (subelements, localName, parseIntegral)

data TypeElement = TypeElement Element | ArrayElement Element

-- | Find the children giving the type of the given element.
findTypeElements :: Element -> [TypeElement]
findTypeElements = mapMaybe toTypeElement . subelements
    where toTypeElement :: Element -> Maybe TypeElement
          toTypeElement elem
              | "type" <- localName elem = Just (TypeElement elem)
              | "array" <- localName elem = Just (ArrayElement elem)
          toTypeElement _ = Nothing

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
nameToBasicType "gtype"    = Just TGType
nameToBasicType "utf8"     = Just TUTF8
nameToBasicType "filename" = Just TFileName
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
nameToBasicType "gsize"    = case sizeOf (0 :: CSize) of
                               4 -> Just TUInt32
                               8 -> Just TUInt64
                               n -> error $ "Unexpected size length: " ++ show n
nameToBasicType _          = Nothing

-- | A boolean value given by a numerical constant.
parseBool :: Text -> Maybe Bool
parseBool "0" = Just False
parseBool "1" = Just True
parseBool _   = Nothing

-- | The different array types.
parseArrayType :: ParseContext -> Element -> Maybe Type
parseArrayType ctx elem =
    case M.lookup "name" (elementAttributes elem) of
      Just "GLib.Array" -> TGArray <$> parseType ctx elem
      Just "GLib.PtrArray" -> TPtrArray <$> parseType ctx elem
      Just "GLib.ByteArray" -> Just TByteArray
      Just _ -> Nothing
      Nothing -> parseCArrayType ctx elem

-- | A C array
parseCArrayType :: ParseContext -> Element -> Maybe Type
parseCArrayType ctx element = do
  let attrs = elementAttributes element
      length = fromMaybe (-1) (M.lookup "length" attrs >>= parseIntegral)
      zeroTerminated = fromMaybe True (M.lookup "zero-terminated" attrs >>= parseBool)
      fixedSize = fromMaybe (-1) (M.lookup "fixed-size" attrs >>= parseIntegral)
  elementType <- parseType ctx element
  return $ TCArray zeroTerminated fixedSize length elementType

-- | A hash table.
parseHashTable :: ParseContext -> Element -> Maybe Type
parseHashTable ctx elem =
    case findTypeElements elem of
      [key, value] -> TGHash <$> parseTypeElement ctx key <*> parseTypeElement ctx value
      _ -> Nothing

-- | A type which is not a BasicType or array.
parseFundamentalType :: Text -> ParseContext -> Element -> Maybe Type
parseFundamentalType "GLib.List" ctx elem = TGList <$> parseType ctx elem
parseFundamentalType "GLib.SList" ctx elem = TGSList <$> parseType ctx elem
parseFundamentalType "GLib.HashTable" ctx elem = parseHashTable ctx elem
parseFundamentalType "GLib.Error" _ _ = Just TError
parseFundamentalType "GLib.Variant" _ _ = Just TVariant
parseFundamentalType "GObject.ParamSpec" _ _ = Just TParamSpec
parseFundamentalType iface ctx _ = parseInterfaceType iface ctx

-- | An interface type (basically, everything that is not of a known type).
parseInterfaceType :: Text -> ParseContext -> Maybe Type
parseInterfaceType iface ParseContext{..} =
    case T.split (== '.') iface of
      [ns, n] -> Just $ checkAliases ns n
      [n]     -> Just $ checkAliases currentNamespace n
      _       -> Nothing
    where checkAliases :: Text -> Text -> Type
          checkAliases ns name =
              case M.lookup (Alias (ns, name)) knownAliases of
                Just t -> t
                Nothing -> TInterface (T.unpack ns) (T.unpack name)

-- | Parse the type of a node (which will be described by a child node
-- named "type" or "array").
parseType :: ParseContext -> Element -> Maybe Type
parseType ctx element =
    case findTypeElements element of
      [e] -> parseTypeElement ctx e
      _ -> Nothing -- If there is not precisely one type element it is
                   -- not clear what to do.

-- | Parse a single type element (the "type" or "array" element itself).
parseTypeElement :: ParseContext -> TypeElement -> Maybe Type
parseTypeElement ctx (TypeElement e) = do
  typeName <- M.lookup "name" (elementAttributes e)
  (TBasicType <$> nameToBasicType typeName) <|> parseFundamentalType typeName ctx e
parseTypeElement ctx (ArrayElement e) = parseArrayType ctx e
