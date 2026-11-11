-- | Fix the memory layout of structs and unions.
--
-- When compiling the typelib, the typelib compiler (both
-- @gi-compile-repository@ in GLib and @g-ir-compiler@ in
-- gobject-introspection) makes two simplifications when computing
-- the layout of structs and unions:
--
-- * It ignores the width of bitfields: each bitfield is laid out as
--   if it was a full member of its declared type.
--
-- * It ignores unions and structs declared inline (such as
--   @union { ... } u;@, or anonymous unions).
--
-- This leads to wrong struct sizes, and wrong offsets for the fields
-- following the affected members. This affects the struct itself,
-- and any struct or union embedding it. See
-- <https://github.com/haskell-gi/haskell-gi/issues/485>.
--
-- The GIR files do contain the required information, so here we
-- recompute the layout ourselves, following the rules of the C
-- ABI. To make sure that we are not missing any information (fields
-- that we could not parse, types of unknown size, ...), we first
-- check that by laying out the struct in the same way that the
-- typelib compiler does we reproduce the typelib data exactly. If
-- this is not the case we keep the data from the typelib.
module Data.GI.CodeGen.StructLayout
  ( Aggregate(..)
  , fixupStructLayouts
  ) where

#include <glib-object.h>

#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif

import Control.Monad (forM, unless, when)
import qualified Data.Map.Lazy as ML
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CTime)
import Foreign.Storable (alignment, sizeOf)
import qualified System.Info

import Data.GI.GIR.BasicTypes (BasicType(..), Name(..), Type(..))
import Data.GI.GIR.Field (Field(..), AnonymousMember(..))
import Data.GI.GIR.Struct (Struct(..))
import Data.GI.GIR.Union (Union(..))

-- | The information about a named type that we need in order to
-- compute layouts.
data Aggregate = AggStruct Struct
               | AggUnion Union
               | AggScalar Int  -- ^ Enums and flags, with their
                                -- storage size in bytes.
               | AggPointer     -- ^ Types which are always stored
                                -- as pointers in structs (objects,
                                -- interfaces, callbacks).

-- | How to lay out the struct.
data Mode = TypelibMode -- ^ Mimic the typelib compiler.
          | ABIMode     -- ^ Follow the platform C ABI.
  deriving (Eq)

-- | Position of a field in a struct.
data FieldLayout = FieldLayout {
      flOffset   :: Int       -- ^ Offset in bytes.
    , flBitShift :: Maybe Int -- ^ Bit shift, for bitfields.
    }

-- | Layout of a struct or union.
data Layout = Layout {
      lSize   :: Int
    , lAlign  :: Int
    , lFields :: [FieldLayout]
    }

-- | Size and alignment of a C type.
type SizeAlign = (Int, Int)

-- | Given the structs, unions, and other named types in scope,
-- return the structs and unions whose layout has been
-- corrected. Bitfields in the corrected structs and unions will
-- also have their `fieldBitShift` set.
fixupStructLayouts :: ML.Map Name Aggregate -> ML.Map Name Aggregate
fixupStructLayouts aggs = ML.mapMaybeWithKey fix aggs
  where
    fix :: Name -> Aggregate -> Maybe Aggregate
    fix n (AggStruct s) | needsFixing (structFields s) = do
      layout <- either (const Nothing) Just (abiLayout n)
      return $ AggStruct (s { structSize = lSize layout
                            , structFields = applyLayout layout
                                                         (structFields s) })
    fix n (AggUnion u) | needsFixing (unionFields u) = do
      layout <- either (const Nothing) Just (abiLayout n)
      return $ AggUnion (u { unionSize = lSize layout
                           , unionFields = applyLayout layout
                                                       (unionFields u) })
    fix _ _ = Nothing

    -- Structs whose layout may be wrong in the typelib: those
    -- containing bitfields or inline unions or structs, and those
    -- embedding other structs or unions (which may be wrong).
    needsFixing :: [Field] -> Bool
    needsFixing = any (\f -> isJust (fieldBits f) || embedsAggregate f)

    embedsAggregate :: Field -> Bool
    embedsAggregate f
      | isJust (fieldAnonymous f) = True
      | isPointerField f = False
      | otherwise = case elementType (fieldType f) of
          TInterface n -> case ML.lookup n aggs of
                            Just (AggStruct _) -> True
                            Just (AggUnion _) -> True
                            _ -> False
          TGValue -> True
          TGClosure _ -> True
          _ -> False

    elementType :: Type -> Type
    elementType (TCArray _ fixed _ t) | fixed > 0 = elementType t
    elementType t = t

    applyLayout :: Layout -> [Field] -> [Field]
    applyLayout layout =
      zipWith (\fl f -> f { fieldOffset = flOffset fl
                          , fieldBitShift = flBitShift fl })
              (lFields layout)

    -- Layouts computed by mimicking the typelib compiler, keeping
    -- only those that reproduce exactly the data in the typelib. The
    -- map is lazy, so we only compute what we need, and each layout
    -- is computed at most once.
    typelibLayouts :: ML.Map Name (Either Text Layout)
    typelibLayouts = ML.mapMaybe verified aggs
      where verified (AggStruct s) =
              Just (verify (structSize s) (structFields s)
                    =<< structLayout TypelibMode (structFields s))
            verified (AggUnion u) =
              Just (verify (unionSize u) (unionFields u)
                    =<< unionLayout TypelibMode (unionFields u))
            verified _ = Nothing

            verify :: Int -> [Field] -> Layout -> Either Text Layout
            verify size fields layout = do
              when (lSize layout /= size) $
                Left $ "Computed size " <> tshow (lSize layout)
                       <> " does not match typelib size " <> tshow size
              let offsets = [(flOffset fl, fieldOffset f)
                            | (fl, f) <- zip (lFields layout) fields
                            , not (isJust (fieldAnonymous f))]
              unless (all (uncurry (==)) offsets) $
                Left "Computed offsets do not match the typelib offsets"
              return layout

    -- Layouts following the C ABI, for those types whose typelib
    -- layout we can reproduce.
    abiLayouts :: ML.Map Name (Either Text Layout)
    abiLayouts = ML.mapMaybeWithKey abi aggs
      where abi n (AggStruct s) =
              Just (typelibLayout n >> structLayout ABIMode (structFields s))
            abi n (AggUnion u) =
              Just (typelibLayout n >> unionLayout ABIMode (unionFields u))
            abi _ _ = Nothing

    typelibLayout :: Name -> Either Text Layout
    typelibLayout n = case ML.lookup n typelibLayouts of
      Just l -> l
      Nothing -> Left ("No layout for " <> tshow n)

    abiLayout :: Name -> Either Text Layout
    abiLayout n = case ML.lookup n abiLayouts of
      Just l -> l
      Nothing -> Left ("No layout for " <> tshow n)

    -- Size and alignment of an embedded struct or union.
    embeddedSizeAlign :: Mode -> Name -> Either Text SizeAlign
    embeddedSizeAlign mode n = do
      layout <- case mode of
                  TypelibMode -> typelibLayout n
                  ABIMode -> abiLayout n
      return (lSize layout, lAlign layout)

    structLayout :: Mode -> [Field] -> Either Text Layout
    structLayout mode fields = do
      sizes <- forM fields (fieldSizeAlign mode)
      let bitfields = case mode of
                        TypelibMode -> map (const Nothing) fields
                        ABIMode -> map fieldBits fields
          placed = zip3 fields sizes bitfields
      if msBitfields
        then msStructLayout placed
        else sysvStructLayout placed

    unionLayout :: Mode -> [Field] -> Either Text Layout
    unionLayout mode fields = do
      sizes <- forM fields (fieldSizeAlign mode)
      let align = maximum (1 : map snd sizes)
          size = roundUp (maximum (0 : map fst sizes)) align
          fl (f, (s, _)) = case (mode, fieldBits f) of
            (ABIMode, Just w) -> FieldLayout 0 (Just (bitShift s 0 w))
            _ -> FieldLayout 0 Nothing
      return $ Layout size align (map fl (zip fields sizes))

    fieldSizeAlign :: Mode -> Field -> Either Text SizeAlign
    fieldSizeAlign mode f = case fieldAnonymous f of
      -- These are ignored by the typelib compiler.
      Just _ | mode == TypelibMode -> Right (0, 1)
      Just (AnonymousUnion members) -> do
        layout <- unionLayout mode members
        return (lSize layout, lAlign layout)
      Just (AnonymousStruct members) -> do
        layout <- structLayout mode members
        return (lSize layout, lAlign layout)
      Nothing
        | isPointerField f -> Right pointerSizeAlign
        | TCArray _ fixed _ _ <- fieldType f, fixed > 0, fieldArrayOfPointers f ->
            let (size, align) = pointerSizeAlign
            in Right (fixed * size, align)
        | otherwise -> case typeSizeAlign mode (fieldType f) of
            Left err -> Left ("Field “" <> fieldName f <> "”: " <> err)
            r -> r

    -- Size and alignment of a type stored inline.
    typeSizeAlign :: Mode -> Type -> Either Text SizeAlign
    typeSizeAlign mode t = case t of
      TBasicType b -> basicSizeAlign b
      TInterface n -> case ML.lookup n aggs of
        Just (AggStruct _) -> embeddedSizeAlign mode n
        Just (AggUnion _) -> embeddedSizeAlign mode n
        Just (AggScalar size) -> Right (size, size)
        Just AggPointer -> Right pointerSizeAlign
        Nothing -> Left ("Unknown type " <> tshow n)
      TGValue -> embeddedSizeAlign mode (Name "GObject" "Value")
      TGClosure _ -> embeddedSizeAlign mode (Name "GObject" "Closure")
      TCArray _ fixed _ elemType
        | fixed > 0 -> do
            (size, align) <- typeSizeAlign mode elemType
            return (fixed * size, align)
        | otherwise -> Right pointerSizeAlign
      _ -> Right pointerSizeAlign

-- | Whether the field is a pointer (as opposed to being stored inline
-- in the struct).
isPointerField :: Field -> Bool
isPointerField f = isJust (fieldCallback f) || fieldIsPointer f == Just True

-- | Lay out the fields in a struct following the rules of the System
-- V (and Itanium) ABIs: a bitfield is placed at the next available
-- bit, unless it would straddle a storage unit of its declared type,
-- in which case it is placed at the start of the next such unit.
sysvStructLayout :: [(Field, SizeAlign, Maybe Int)] -> Either Text Layout
sysvStructLayout = go 0 1 []
  where
    -- We keep track of the position in bits.
    go :: Int -> Int -> [FieldLayout] -> [(Field, SizeAlign, Maybe Int)]
       -> Either Text Layout
    go pos align acc [] =
      Right $ Layout (roundUp (roundUp pos 8 `div` 8) align) align (reverse acc)
    go pos align acc ((f, (size, falign), bits) : rest) = case bits of
      Nothing ->
        let offset = roundUp (roundUp pos 8 `div` 8) falign
        in go ((offset + size) * 8) (max align falign)
              (FieldLayout offset Nothing : acc) rest
      Just w -> do
        let unitBits = size * 8
        when (w > unitBits) $
          Left ("Bitfield “" <> fieldName f <> "” is wider than its type")
        let start = if pos `mod` unitBits + w > unitBits
                    then roundUp pos unitBits
                    else pos
            unit = (start `div` unitBits) * size
        go (start + w) (max align falign)
           (FieldLayout unit (Just (bitShift size (start - unit * 8) w)) : acc)
           rest

-- | Lay out the fields in a struct following the rules used by MSVC
-- (and by GCC with @-mms-bitfields@, the default on Windows): a
-- bitfield shares the storage unit of the previous bitfield only if
-- their declared types have the same size and there is enough space
-- left, otherwise a new storage unit is allocated.
msStructLayout :: [(Field, SizeAlign, Maybe Int)] -> Either Text Layout
msStructLayout = go 0 Nothing 1 []
  where
    -- We keep track of the end of the struct so far in bytes, and of
    -- the current storage unit (offset, size, bits used), if any.
    go :: Int -> Maybe (Int, Int, Int) -> Int -> [FieldLayout]
       -> [(Field, SizeAlign, Maybe Int)] -> Either Text Layout
    go end _ align acc [] =
      Right $ Layout (roundUp end align) align (reverse acc)
    go end unit align acc ((f, (size, falign), bits) : rest) = case bits of
      Nothing ->
        let offset = roundUp end falign
        in go (offset + size) Nothing (max align falign)
              (FieldLayout offset Nothing : acc) rest
      Just w -> do
        when (w > size * 8) $
          Left ("Bitfield “" <> fieldName f <> "” is wider than its type")
        case unit of
          Just (uOffset, uSize, used) | uSize == size && used + w <= size * 8 ->
            go end (Just (uOffset, uSize, used + w)) (max align falign)
               (FieldLayout uOffset (Just (bitShift size used w)) : acc) rest
          _ -> do
            let offset = roundUp end falign
            go (offset + size) (Just (offset, size, w)) (max align falign)
               (FieldLayout offset (Just (bitShift size 0 w)) : acc) rest

-- | Shift of the least significant bit of a bitfield of the given
-- width, starting at the given bit position in a storage unit of
-- the given size in bytes. On big endian machines bitfields are
-- allocated starting from the most significant bit.
bitShift :: Int -> Int -> Int -> Int
bitShift unitSize pos width
  | bigEndian = unitSize * 8 - pos - width
  | otherwise = pos

-- | Whether we are running on a big endian machine.
bigEndian :: Bool
bigEndian = (#{const G_BYTE_ORDER} :: Int) == #{const G_BIG_ENDIAN}

-- | Whether bitfields are laid out following the MSVC rules.
msBitfields :: Bool
msBitfields = System.Info.os == "mingw32"

pointerSizeAlign :: SizeAlign
pointerSizeAlign = (#{size gpointer}, #{alignment gpointer})

-- | Size and alignment of the basic types.
basicSizeAlign :: BasicType -> Either Text SizeAlign
basicSizeAlign t = case t of
  TBoolean -> Right (#{size gboolean}, #{alignment gboolean})
  TInt -> Right (#{size gint}, #{alignment gint})
  TUInt -> Right (#{size guint}, #{alignment guint})
  TLong -> Right (#{size glong}, #{alignment glong})
  TULong -> Right (#{size gulong}, #{alignment gulong})
  TInt8 -> Right (#{size gint8}, #{alignment gint8})
  TUInt8 -> Right (#{size guint8}, #{alignment guint8})
  TInt16 -> Right (#{size gint16}, #{alignment gint16})
  TUInt16 -> Right (#{size guint16}, #{alignment guint16})
  TInt32 -> Right (#{size gint32}, #{alignment gint32})
  TUInt32 -> Right (#{size guint32}, #{alignment guint32})
  TInt64 -> Right (#{size gint64}, #{alignment gint64})
  TUInt64 -> Right (#{size guint64}, #{alignment guint64})
  TFloat -> Right (#{size gfloat}, #{alignment gfloat})
  TDouble -> Right (#{size gdouble}, #{alignment gdouble})
  TUniChar -> Right (#{size gunichar}, #{alignment gunichar})
  TGType -> Right (#{size GType}, #{alignment GType})
  TUTF8 -> Right pointerSizeAlign
  TFileName -> Right pointerSizeAlign
  TPtr -> Right pointerSizeAlign
  TIntPtr -> Right (#{size gintptr}, #{alignment gintptr})
  TUIntPtr -> Right (#{size guintptr}, #{alignment guintptr})
  TShort -> Right (#{size gshort}, #{alignment gshort})
  TUShort -> Right (#{size gushort}, #{alignment gushort})
  TSize -> Right (#{size gsize}, #{alignment gsize})
  TSSize -> Right (#{size gssize}, #{alignment gssize})
  Ttime_t -> Right (sizeOf (0 :: CTime), alignment (0 :: CTime))
  _ -> Left ("Unsupported basic type " <> tshow t)

roundUp :: Int -> Int -> Int
roundUp n a = ((n + a - 1) `div` a) * a

tshow :: Show a => a -> Text
tshow = T.pack . show
