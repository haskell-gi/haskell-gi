{-# LANGUAGE PatternGuards, DeriveFunctor #-}

module Data.GI.CodeGen.Conversions
    ( convert
    , genConversion
    , unpackCArray
    , computeArrayLength

    , callableHasClosures

    , hToF
    , fToH
    , transientToH
    , haskellType
    , isoHaskellType
    , foreignType

    , argumentType
    , ExposeClosures(..)
    , elementType
    , elementMap
    , elementTypeAndMap

    , isManaged
    , typeIsNullable
    , typeIsPtr
    , typeIsCallback
    , maybeNullConvert
    , nullPtrForType

    , typeAllocInfo
    , TypeAllocInfo(..)

    , apply
    , mapC
    , literal
    , Constructor(..)
    ) where

#include <glib-object.h>

import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))

import Foreign.C.Types (CInt, CUInt)
import Foreign.Storable (sizeOf)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.GObject
import Data.GI.CodeGen.SymbolNaming
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

-- | The free monad.
data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance (Functor f) => Applicative (Free f) where
    pure = Pure
    Pure a <*> Pure b = Pure $ a b
    Pure a <*> Free mb = Free $ fmap a <$> mb
    Free ma <*> b = Free $ (<*> b) <$> ma

instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r

-- | Lift some command to the Free monad.
liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

-- String identifying a constructor in the generated code, which is
-- either (by default) a pure function (indicated by the P
-- constructor) or a function returning values on a monad (M
-- constructor). 'Id' denotes the identity function.
data Constructor = P Text | M Text | Id
                   deriving (Eq,Show)
instance IsString Constructor where
    fromString = P . T.pack

data FExpr next = Apply Constructor next
                | LambdaConvert Text next
                | MapC Map Constructor next
                | Literal Constructor next
                  deriving (Show, Functor)

type Converter = Free FExpr ()

-- Different available maps.
data Map = Map | MapFirst | MapSecond
         deriving (Show)

-- Naming for the maps.
mapName :: Map -> Text
mapName Map = "map"
mapName MapFirst = "mapFirst"
mapName MapSecond = "mapSecond"

-- Naming for the monadic versions of the maps that we use
monadicMapName :: Map -> Text
monadicMapName Map = "mapM"
monadicMapName MapFirst = "mapFirstA"
monadicMapName MapSecond = "mapSecondA"

apply :: Constructor -> Converter
apply f = liftF $ Apply f ()

mapC :: Constructor -> Converter
mapC f = liftF $ MapC Map f ()

mapFirst :: Constructor -> Converter
mapFirst f = liftF $ MapC MapFirst f ()

mapSecond :: Constructor -> Converter
mapSecond f = liftF $ MapC MapSecond f ()

literal :: Constructor -> Converter
literal f = liftF $ Literal f ()

lambdaConvert :: Text -> Converter
lambdaConvert c = liftF $ LambdaConvert c ()

genConversion :: Text -> Converter -> CodeGen Text
genConversion l (Pure ()) = return l
genConversion l (Free k) = do
  let l' = prime l
  case k of
    Apply (P f) next ->
        do line $ "let " <> l' <> " = " <> f <> " " <> l
           genConversion l' next
    Apply (M f) next ->
        do line $ l' <> " <- " <> f <> " " <> l
           genConversion l' next
    Apply Id next -> genConversion l next

    MapC m (P f) next ->
        do line $ "let " <> l' <> " = " <> mapName m <> " " <> f <> " " <> l
           genConversion l' next
    MapC m (M f) next ->
        do line $ l' <> " <- " <> monadicMapName m <> " " <> f <> " " <> l
           genConversion l' next
    MapC _ Id next -> genConversion l next

    LambdaConvert conv next ->
        do line $ conv <> " " <> l <> " $ \\" <> l' <> " -> do"
           increaseIndent
           genConversion l' next

    Literal (P f) next ->
        do line $ "let " <> l <> " = " <> f
           genConversion l next
    Literal (M f) next ->
        do line $ l <> " <- " <> f
           genConversion l next
    Literal Id next -> genConversion l next

-- | Given an array, together with its type, return the code for reading
-- its length.
computeArrayLength :: Text -> Type -> ExcCodeGen Text
computeArrayLength array (TCArray _ _ _ t) = do
  reader <- findReader
  return $ "fromIntegral $ " <> reader <> " " <> array
    where findReader = case t of
                     TBasicType TUInt8 -> return "B.length"
                     _ -> return "P.length"
computeArrayLength _ t =
    notImplementedError $ "computeArrayLength called on non-CArray type "
                            <> tshow t

convert :: Text -> BaseCodeGen e Converter -> BaseCodeGen e Text
convert l c = do
  c' <- c
  genConversion l c'

hObjectToF :: Type -> Transfer -> ExcCodeGen Constructor
hObjectToF t transfer =
  if transfer == TransferEverything
  then do
    isGO <- isGObject t
    if isGO
    then return $ M "B.ManagedPtr.disownObject"
    else return $ M "B.ManagedPtr.disownManagedPtr"
  -- castPtr since we accept any instance of the class associated with
  -- the GObject, not just the precise type of the GObject, while the
  -- foreign function declaration requires a pointer of the precise
  -- type.
  else return $ M "unsafeManagedPtrCastPtr"

hVariantToF :: Transfer -> CodeGen Constructor
hVariantToF transfer =
  if transfer == TransferEverything
  then return $ M "B.GVariant.disownGVariant"
  else return $ M "unsafeManagedPtrGetPtr"

hValueToF :: Transfer -> CodeGen Constructor
hValueToF transfer =
  if transfer == TransferEverything
  then return $ M "B.GValue.disownGValue"
  else return $ M "unsafeManagedPtrGetPtr"

hParamSpecToF :: Transfer -> CodeGen Constructor
hParamSpecToF transfer =
  if transfer == TransferEverything
  then return $ M "B.GParamSpec.disownGParamSpec"
  else return $ M "unsafeManagedPtrGetPtr"

hClosureToF :: Transfer -> Maybe Type -> CodeGen Constructor
-- Untyped closures
hClosureToF transfer Nothing =
  if transfer == TransferEverything
  then return $ M "B.GClosure.disownGClosure"
  -- We cast the point here because the foreign type for untyped
  -- closures is always represented as Ptr (GClosure ()), while the
  -- corresponding Haskell type is the parametric "GClosure a".
  else return $ M "unsafeManagedPtrCastPtr"
-- Typed closures
hClosureToF transfer (Just _) =
  if transfer == TransferEverything
  then return $ M "B.GClosure.disownGClosure"
  else return $ M "unsafeManagedPtrGetPtr"

hBoxedToF :: Transfer -> CodeGen Constructor
hBoxedToF transfer =
  if transfer == TransferEverything
  then return $ M "B.ManagedPtr.disownBoxed"
  else return $ M "unsafeManagedPtrGetPtr"

hStructToF :: Struct -> Transfer -> ExcCodeGen Constructor
hStructToF s transfer =
    if transfer /= TransferEverything || structIsBoxed s then
        hBoxedToF transfer
    else do
        when (structSize s == 0) $
             badIntroError "Transferring a non-boxed struct with unknown size!"
        return $ M "unsafeManagedPtrGetPtr"

hUnionToF :: Union -> Transfer -> ExcCodeGen Constructor
hUnionToF u transfer =
    if transfer /= TransferEverything || unionIsBoxed u then
        hBoxedToF transfer
    else do
        when (unionSize u == 0) $
             badIntroError "Transferring a non-boxed union with unknown size!"
        return $ M "unsafeManagedPtrGetPtr"

-- Given the Haskell and Foreign types, returns the name of the
-- function marshalling between both.
hToF' :: Type -> Maybe API -> TypeRep -> TypeRep -> Transfer
            -> ExcCodeGen Constructor
hToF' t a hType fType transfer
    | ( hType == fType ) = return Id
    | TError <- t = hBoxedToF transfer
    | TVariant <- t = hVariantToF transfer
    | TGValue <- t = hValueToF transfer
    | TParamSpec <- t = hParamSpecToF transfer
    | TGClosure c <- t = hClosureToF transfer c
    | Just (APIEnum _) <- a = return "(fromIntegral . fromEnum)"
    | Just (APIFlags _) <- a = return "gflagsToWord"
    | Just (APIObject _) <- a = hObjectToF t transfer
    | Just (APIInterface _) <- a = hObjectToF t transfer
    | Just (APIStruct s) <- a = hStructToF s transfer
    | Just (APIUnion u) <- a = hUnionToF u transfer
    -- Converting callback types requires more context, we leave that
    -- as a special case to be implemented by the caller.
    | Just (APICallback _) <- a = error "Cannot handle callback type here!! "
    | TByteArray <- t = return $ M "packGByteArray"
    | TCArray True _ _ (TBasicType TUTF8) <- t =
        return $ M "packZeroTerminatedUTF8CArray"
    | TCArray True _ _ (TBasicType TFileName) <- t =
        return $ M "packZeroTerminatedFileNameArray"
    | TCArray True _ _ (TBasicType TPtr) <- t =
        return $ M "packZeroTerminatedPtrArray"
    | TCArray True _ _ (TBasicType TUInt8) <- t =
        return $ M "packZeroTerminatedByteString"
    | TCArray True _ _ (TBasicType TBoolean) <- t =
        return $ M "(packMapZeroTerminatedStorableArray (fromIntegral . fromEnum))"
    | TCArray True _ _ (TBasicType TGType) <- t =
        return $ M "(packMapZeroTerminatedStorableArray gtypeToCGtype)"
    | TCArray True _ _ (TBasicType _) <- t =
        return $ M "packZeroTerminatedStorableArray"
    | TCArray False _ _ (TBasicType TUTF8) <- t =
        return $ M "packUTF8CArray"
    | TCArray False _ _ (TBasicType TFileName) <- t =
        return $ M "packFileNameArray"
    | TCArray False _ _ (TBasicType TPtr) <- t =
        return $ M "packPtrArray"
    | TCArray False _ _ (TBasicType TUInt8) <- t =
        return $ M "packByteString"
    | TCArray False _ _ (TBasicType TBoolean) <- t =
        return $ M "(packMapStorableArray (fromIntegral . fromEnum))"
    | TCArray False _ _ (TBasicType TGType) <- t =
        return $ M "(packMapStorableArray gtypeToCGType)"
    | TCArray False _ _ (TBasicType TFloat) <- t =
        return $ M "(packMapStorableArray realToFrac)"
    | TCArray False _ _ (TBasicType TDouble) <- t =
        return $ M "(packMapStorableArray realToFrac)"
    | TCArray False _ _ (TBasicType _) <- t =
        return $ M "packStorableArray"
    | TCArray False _ _ TGValue <- t =
        return $ M "B.GValue.packGValueArray"
    | TCArray{}  <- t = notImplementedError $
                   "Don't know how to pack C array of type " <> tshow t
    | otherwise = case (typeShow hType, typeShow fType) of
               ("T.Text", "CString") -> return $ M "textToCString"
               ("[Char]", "CString") -> return $ M "stringToCString"
               ("Char", "CInt")      -> return "(fromIntegral . ord)"
               ("Bool", "CInt")      -> return "(fromIntegral . fromEnum)"
               ("Float", "CFloat")   -> return "realToFrac"
               ("Double", "CDouble") -> return "realToFrac"
               ("GType", "CGType")   -> return "gtypeToCGType"
               _                     -> notImplementedError $
                                        "Don't know how to convert "
                                        <> typeShow hType <> " into "
                                        <> typeShow fType <> ".\n"
                                        <> "Internal type: "
                                        <> tshow t

getForeignConstructor :: Type -> Transfer -> ExcCodeGen Constructor
getForeignConstructor t transfer = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  hToF' t a hType fType transfer

hToF_PackedType :: Type -> Text -> Transfer -> ExcCodeGen Converter
hToF_PackedType t packer transfer = do
  innerConstructor <- getForeignConstructor t transfer
  return $ do
    mapC innerConstructor
    apply (M packer)

-- | Try to find the `hash` and `equal` functions appropriate for the
-- given type, when used as a key in a GHashTable.
hashTableKeyMappings :: Type -> ExcCodeGen (Text, Text)
hashTableKeyMappings (TBasicType TPtr) = return ("gDirectHash", "gDirectEqual")
hashTableKeyMappings (TBasicType TUTF8) = return ("gStrHash", "gStrEqual")
hashTableKeyMappings t =
    notImplementedError $ "GHashTable key of type " <> tshow t <> " unsupported."

-- | `GHashTable` tries to fit every type into a pointer, the
-- following function tries to find the appropriate
-- (destroy,packer,unpacker) for the given type.
hashTablePtrPackers :: Type -> ExcCodeGen (Text, Text, Text)
hashTablePtrPackers (TBasicType TPtr) =
    return ("Nothing", "ptrPackPtr", "ptrUnpackPtr")
hashTablePtrPackers (TBasicType TUTF8) =
    return ("(Just ptr_to_g_free)", "cstringPackPtr", "cstringUnpackPtr")
hashTablePtrPackers t =
    notImplementedError $ "GHashTable element of type " <> tshow t <> " unsupported."

hToF_PackGHashTable :: Type -> Type -> ExcCodeGen Converter
hToF_PackGHashTable keys elems = do
  -- We will be adding elements to the Hash list with appropriate
  -- destructors, so we always want a fresh copy.
  keysConstructor <- getForeignConstructor keys TransferEverything
  elemsConstructor <- getForeignConstructor elems TransferEverything
  (keyHash, keyEqual) <- hashTableKeyMappings keys
  (keyDestroy, keyPack, _) <- hashTablePtrPackers keys
  (elemDestroy, elemPack, _) <- hashTablePtrPackers elems
  return $ do
    apply (P "Map.toList")
    mapFirst keysConstructor
    mapSecond elemsConstructor
    mapFirst (P keyPack)
    mapSecond (P elemPack)
    apply (M (T.intercalate " " ["packGHashTable", keyHash, keyEqual,
                                 keyDestroy, elemDestroy]))

hToF :: Type -> Transfer -> ExcCodeGen Converter
hToF (TGList t) transfer = do
  isPtr <- typeIsPtr t
  when (not isPtr) $
       badIntroError ("'" <> tshow t <>
                      "' is not a pointer type, cannot pack into a GList.")
  hToF_PackedType t "packGList" transfer
hToF (TGSList t) transfer = do
  isPtr <- typeIsPtr t
  when (not isPtr) $
       badIntroError ("'" <> tshow t <>
                      "' is not a pointer type, cannot pack into a GSList.")
  hToF_PackedType t "packGSList" transfer
hToF (TGArray t) transfer = hToF_PackedType t "packGArray" transfer
hToF (TPtrArray t) transfer = hToF_PackedType t "packGPtrArray" transfer
hToF (TGHash ta tb) _ = hToF_PackGHashTable ta tb
hToF (TCArray zt _ _ t@(TCArray{})) transfer = do
  let packer = if zt
               then "packZeroTerminated"
               else "pack"
  hToF_PackedType t (packer <> "PtrArray") transfer
hToF (TCArray zt _ _ t@(TInterface _)) transfer = do
  isScalar <- typeIsEnumOrFlag t
  let packer = if zt
               then "packZeroTerminated"
               else "pack"
  if isScalar
  then hToF_PackedType t (packer <> "StorableArray") transfer
  else do
    api <- findAPI t
    let size = case api of
                 Just (APIStruct s) -> structSize s
                 Just (APIUnion u) -> unionSize u
                 _ -> 0
    if size == 0 || zt
    then hToF_PackedType t (packer <> "PtrArray") transfer
    else hToF_PackedType t (packer <> "BlockArray " <> tshow size) transfer

hToF t transfer = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  constructor <- hToF' t a hType fType transfer
  return $ apply constructor

boxedForeignPtr :: Text -> Transfer -> CodeGen Constructor
boxedForeignPtr constructor transfer = return $
   case transfer of
     TransferEverything -> M $ parenthesize $ "wrapBoxed " <> constructor
     _ -> M $ parenthesize $ "newBoxed " <> constructor

suForeignPtr :: Bool -> TypeRep -> Transfer -> CodeGen Constructor
suForeignPtr isBoxed hType transfer = do
  let constructor = typeConName hType
  if isBoxed then
      boxedForeignPtr constructor transfer
  else return $ M $ parenthesize $
       case transfer of
         TransferEverything -> "wrapPtr " <> constructor
         _ -> "newPtr " <> constructor

structForeignPtr :: Struct -> TypeRep -> Transfer -> CodeGen Constructor
structForeignPtr s =
    suForeignPtr (structIsBoxed s)

unionForeignPtr :: Union -> TypeRep -> Transfer -> CodeGen Constructor
unionForeignPtr u =
    suForeignPtr (unionIsBoxed u)

fObjectToH :: Type -> TypeRep -> Transfer -> ExcCodeGen Constructor
fObjectToH t hType transfer = do
  let constructor = typeConName hType
  isGO <- isGObject t
  return $ M $ parenthesize $
    case transfer of
    TransferEverything ->
        if isGO
        then "wrapObject " <> constructor
        else "wrapPtr " <> constructor
    _ ->
        if isGO
        then "newObject " <> constructor
        else "newPtr " <> constructor

fCallbackToH :: TypeRep -> Transfer -> ExcCodeGen Constructor
fCallbackToH hType TransferNothing = do
  let constructor = typeConName hType
  return (P (callbackDynamicWrapper constructor))
fCallbackToH _ transfer =
  notImplementedError ("ForeignCallback with unsupported transfer type `"
                       <> tshow transfer <> "'")

fVariantToH :: Transfer -> CodeGen Constructor
fVariantToH transfer =
  return $ M $ case transfer of
                  TransferEverything -> "B.GVariant.wrapGVariantPtr"
                  _ -> "B.GVariant.newGVariantFromPtr"

fValueToH :: Transfer -> CodeGen Constructor
fValueToH transfer =
  return $ M $ case transfer of
                  TransferEverything -> "B.GValue.wrapGValuePtr"
                  _ -> "B.GValue.newGValueFromPtr"

fParamSpecToH :: Transfer -> CodeGen Constructor
fParamSpecToH transfer =
  return $ M $ case transfer of
                  TransferEverything -> "B.GParamSpec.wrapGParamSpecPtr"
                  _ -> "B.GParamSpec.newGParamSpecFromPtr"

fClosureToH :: Transfer -> Maybe Type -> CodeGen Constructor
-- Untyped closures
fClosureToH transfer Nothing =
  return $ M $ case transfer of
                  TransferEverything ->
                    parenthesize $ "B.GClosure.wrapGClosurePtr . FP.castPtr"
                  _ -> parenthesize $ "B.GClosure.newGClosureFromPtr . FP.castPtr"
-- Typed closures
fClosureToH transfer (Just _) =
  return $ M $ case transfer of
                  TransferEverything -> "B.GClosure.wrapGClosurePtr"
                  _ -> "B.GClosure.newGClosureFromPtr"

fToH' :: Type -> Maybe API -> TypeRep -> TypeRep -> Transfer
         -> ExcCodeGen Constructor
fToH' t a hType fType transfer
    | ( hType == fType ) = return Id
    | Just (APIEnum _) <- a = return "(toEnum . fromIntegral)"
    | Just (APIFlags _) <- a = return "wordToGFlags"
    | TError <- t = boxedForeignPtr "GError" transfer
    | TVariant <- t = fVariantToH transfer
    | TGValue <- t = fValueToH transfer
    | TParamSpec <- t = fParamSpecToH transfer
    | TGClosure c <- t = fClosureToH transfer c
    | Just (APIStruct s) <- a = structForeignPtr s hType transfer
    | Just (APIUnion u) <- a = unionForeignPtr u hType transfer
    | Just (APIObject _) <- a = fObjectToH t hType transfer
    | Just (APIInterface _) <- a = fObjectToH t hType transfer
    | Just (APICallback _) <- a = fCallbackToH hType transfer
    | TCArray True _ _ (TBasicType TUTF8) <- t =
        return $ M "unpackZeroTerminatedUTF8CArray"
    | TCArray True _ _ (TBasicType TFileName) <- t =
        return $ M "unpackZeroTerminatedFileNameArray"
    | TCArray True _ _ (TBasicType TUInt8) <- t =
        return $ M "unpackZeroTerminatedByteString"
    | TCArray True _ _ (TBasicType TPtr) <- t =
        return $ M "unpackZeroTerminatedPtrArray"
    | TCArray True _ _ (TBasicType TBoolean) <- t =
        return $ M "(unpackMapZeroTerminatedStorableArray (/= 0))"
    | TCArray True _ _ (TBasicType TGType) <- t =
        return $ M "(unpackMapZeroTerminatedStorableArray GType)"
    | TCArray True _ _ (TBasicType TFloat) <- t =
        return $ M "(unpackMapZeroTerminatedStorableArray realToFrac)"
    | TCArray True _ _ (TBasicType TDouble) <- t =
        return $ M "(unpackMapZeroTerminatedStorableArray realToFrac)"
    | TCArray True _ _ (TBasicType _) <- t =
        return $ M "unpackZeroTerminatedStorableArray"
    | TCArray{}  <- t = notImplementedError $
                   "Don't know how to unpack C array of type " <> tshow t
    | TByteArray <- t = return $ M "unpackGByteArray"
    | TGHash _ _ <- t = notImplementedError "Foreign Hashes not supported yet"
    | otherwise = case (typeShow fType, typeShow hType) of
               ("CString", "T.Text") -> return $ M "cstringToText"
               ("CString", "[Char]") -> return $ M "cstringToString"
               ("CInt", "Char")      -> return "(chr . fromIntegral)"
               ("CInt", "Bool")      -> return "(/= 0)"
               ("CFloat", "Float")   -> return "realToFrac"
               ("CDouble", "Double") -> return "realToFrac"
               ("CGType", "GType")   -> return "GType"
               _                     ->
                   notImplementedError $ "Don't know how to convert "
                                           <> typeShow fType <> " into "
                                           <> typeShow hType <> ".\n"
                                           <> "Internal type: "
                                           <> tshow t

getHaskellConstructor :: Type -> Transfer -> ExcCodeGen Constructor
getHaskellConstructor t transfer = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  fToH' t a hType fType transfer

fToH_PackedType :: Type -> Text -> Transfer -> ExcCodeGen Converter
fToH_PackedType t unpacker transfer = do
  innerConstructor <- getHaskellConstructor t transfer
  return $ do
    apply (M unpacker)
    mapC innerConstructor

fToH_UnpackGHashTable :: Type -> Type -> Transfer -> ExcCodeGen Converter
fToH_UnpackGHashTable keys elems transfer = do
  keysConstructor <- getHaskellConstructor keys transfer
  (_,_,keysUnpack) <- hashTablePtrPackers keys
  elemsConstructor <- getHaskellConstructor elems transfer
  (_,_,elemsUnpack) <- hashTablePtrPackers elems
  return $ do
    apply (M "unpackGHashTable")
    mapFirst (P keysUnpack)
    mapFirst keysConstructor
    mapSecond (P elemsUnpack)
    mapSecond elemsConstructor
    apply (P "Map.fromList")

fToH :: Type -> Transfer -> ExcCodeGen Converter
fToH (TGList t) transfer = do
  isPtr <- typeIsPtr t
  when (not isPtr) $
       badIntroError ("`" <> tshow t <>
                      "' is not a pointer type, cannot unpack from a GList.")
  fToH_PackedType t "unpackGList" transfer
fToH (TGSList t) transfer = do
  isPtr <- typeIsPtr t
  when (not isPtr) $
       badIntroError ("`" <> tshow t <>
                      "' is not a pointer type, cannot unpack from a GSList.")
  fToH_PackedType t "unpackGSList" transfer
fToH (TGArray t) transfer = fToH_PackedType t "unpackGArray" transfer
fToH (TPtrArray t) transfer = fToH_PackedType t "unpackGPtrArray" transfer
fToH (TGHash a b) transfer = fToH_UnpackGHashTable a b transfer
-- We cannot unpack arrays without any kind of length info.
fToH t@(TCArray False (-1) (-1) _) _ =
  badIntroError ("`" <> tshow t <>
                  "' is an array type, but contains no length information.")
fToH (TCArray True _ _ t@(TCArray{})) transfer =
  fToH_PackedType t "unpackZeroTerminatedPtrArray" transfer
fToH (TCArray True _ _ t@(TInterface _)) transfer = do
  isScalar <- typeIsEnumOrFlag t
  if isScalar
  then fToH_PackedType t "unpackZeroTerminatedStorableArray" transfer
  else fToH_PackedType t "unpackZeroTerminatedPtrArray" transfer

fToH t transfer = do
  a <- findAPI t
  hType <- haskellType t
  fType <- foreignType t
  constructor <- fToH' t a hType fType transfer
  return $ apply constructor

-- | Somewhat like `fToH`, but with slightly different borrowing
-- semantics: in the case of `TransferNothing` we wrap incoming
-- pointers to boxed structs into transient `ManagedPtr`s (every other
-- case behaves as `fToH`). These are `ManagedPtr`s for which we do
-- not make a copy, and which will be disowned when the function
-- exists, instead of making a copy that the GC will collect
-- eventually.
--
-- This is necessary in order to get the semantics of callbacks and
-- signals right: in some cases making a copy of the object does not
-- simply increase the refcount, but rather makes a full copy. In this
-- cases modification of the original object is not possible, but this
-- is sometimes useful, see for example
--
-- https://github.com/haskell-gi/haskell-gi/issues/97
--
-- Another situation where making a copy of incoming arguments is
-- problematic is when the underlying library is not thread-safe. When
-- running under the threaded GHC runtime it can happen that the GC
-- runs on a different OS thread than the thread where the object was
-- created, and this leads to rather mysterious bugs, see for example
--
-- https://github.com/haskell-gi/haskell-gi/issues/96
--
-- This case is particularly nasty, since it affects `onWidgetDraw`,
-- which is very common.
transientToH :: Type -> Transfer -> ExcCodeGen Converter
transientToH t@(TInterface _) TransferNothing = do
  a <- findAPI t
  case a of
    Just (APIStruct s) -> if structIsBoxed s
                          then wrapTransient t
                          else fToH t TransferNothing
    Just (APIUnion u) -> if unionIsBoxed u
                         then wrapTransient t
                         else fToH t TransferNothing
    _ -> fToH t TransferNothing
transientToH t transfer = fToH t transfer

-- | Wrap the given transient.
wrapTransient :: Type -> CodeGen Converter
wrapTransient t = do
  hCon <- typeConName <$> haskellType t
  return $ lambdaConvert $ "B.ManagedPtr.withTransient " <> hCon

unpackCArray :: Text -> Type -> Transfer -> ExcCodeGen Converter
unpackCArray length (TCArray False _ _ t) transfer =
  case t of
    TBasicType TUTF8 -> return $ apply $ M $ parenthesize $
                        "unpackUTF8CArrayWithLength " <> length
    TBasicType TFileName -> return $ apply $ M $ parenthesize $
                            "unpackFileNameArrayWithLength " <> length
    TBasicType TUInt8 -> return $ apply $ M $ parenthesize $
                         "unpackByteStringWithLength " <> length
    TBasicType TPtr -> return $ apply $ M $ parenthesize $
                         "unpackPtrArrayWithLength " <> length
    TBasicType TBoolean -> return $ apply $ M $ parenthesize $
                         "unpackMapStorableArrayWithLength (/= 0) " <> length
    TBasicType TGType -> return $ apply $ M $ parenthesize $
                         "unpackMapStorableArrayWithLength GType " <> length
    TBasicType TFloat -> return $ apply $ M $ parenthesize $
                         "unpackMapStorableArrayWithLength realToFrac " <> length
    TBasicType TDouble -> return $ apply $ M $ parenthesize $
                         "unpackMapStorableArrayWithLength realToFrac " <> length
    TBasicType _ -> return $ apply $ M $ parenthesize $
                         "unpackStorableArrayWithLength " <> length
    TGValue -> return $ apply $ M $ parenthesize $
               "B.GValue.unpackGValueArrayWithLength " <> length
    TInterface _ -> do
           a <- findAPI t
           isScalar <- typeIsEnumOrFlag t
           hType <- haskellType t
           fType <- foreignType t
           let (boxed, size) = case a of
                        Just (APIStruct s) -> (structIsBoxed s, structSize s)
                        Just (APIUnion u) -> (unionIsBoxed u, unionSize u)
                        _ -> (False, 0)
           let unpacker | isScalar    = "unpackStorableArrayWithLength"
                        | (size == 0) = "unpackPtrArrayWithLength"
                        | boxed       = "unpackBoxedArrayWithLength " <> tshow size
                        | otherwise   = "unpackBlockArrayWithLength " <> tshow size
               -- We always make a copy of the elements when unpacking
               -- boxed types.
           let transfer' | boxed      = if transfer == TransferContainer
                                        then TransferEverything
                                        else transfer
                         | otherwise  = transfer
           innerConstructor <- fToH' t a hType fType transfer'
           return $ do
             apply $ M $ parenthesize $ unpacker <> " " <> length
             mapC innerConstructor
    _ -> notImplementedError $
         "unpackCArray : Don't know how to unpack C Array of type " <> tshow t

unpackCArray _ _ _ = notImplementedError "unpackCArray : unexpected array type."

-- | Whether to expose closures and the associated destroy notify
-- handlers in the Haskell wrapper.
data ExposeClosures = WithClosures
                    | WithoutClosures
  deriving (Eq)

-- | Given a type find the typeclasses the type belongs to, and return
-- the representation of the type in the function signature and the
-- list of typeclass constraints for the type.
argumentType :: Type -> ExposeClosures -> CodeGen (Text, [Text])
argumentType (TGList a) expose = do
  (name, constraints) <- argumentType a expose
  return ("[" <> name <> "]", constraints)
argumentType (TGSList a) expose = do
  (name, constraints) <- argumentType a expose
  return ("[" <> name <> "]", constraints)
argumentType t expose = do
  api <- findAPI t
  s <- typeShow <$> haskellType t
  case api of
    -- Instead of restricting to the actual class,
    -- we allow for any object descending from it.
    Just (APIInterface _) -> do
      cls <- typeConstraint t
      l <- getFreshTypeVariable
      return (l, [cls <> " " <> l])
    Just (APIObject _) -> do
      cls <- typeConstraint t
      l <- getFreshTypeVariable
      return (l, [cls <> " " <> l])
    Just (APICallback cb) ->
      -- See [Note: Callables that throw]
      if callableThrows (cbCallable cb)
      then do
        ft <- typeShow <$> foreignType t
        return (ft, [])
      else
        case expose of
          WithClosures -> do
            s_withClosures <- typeShow <$> isoHaskellType t
            return (s_withClosures, [])
          WithoutClosures ->
            return (s, [])
    _ -> return (s, [])

haskellBasicType :: BasicType -> TypeRep
haskellBasicType TPtr      = ptr $ con0 "()"
haskellBasicType TBoolean  = con0 "Bool"
-- For all the platforms that we support (and those supported by glib)
-- we have gint == gint32. Encoding this assumption in the types saves
-- conversions.
haskellBasicType TInt      = case sizeOf (0 :: CInt) of
                               4 -> con0 "Int32"
                               n -> error ("Unsupported `gint' length: " ++
                                           show n)
haskellBasicType TUInt     = case sizeOf (0 :: CUInt) of
                               4 -> con0 "Word32"
                               n -> error ("Unsupported `guint' length: " ++
                                           show n)
haskellBasicType TLong     = con0 "CLong"
haskellBasicType TULong    = con0 "CULong"
haskellBasicType TInt8     = con0 "Int8"
haskellBasicType TUInt8    = con0 "Word8"
haskellBasicType TInt16    = con0 "Int16"
haskellBasicType TUInt16   = con0 "Word16"
haskellBasicType TInt32    = con0 "Int32"
haskellBasicType TUInt32   = con0 "Word32"
haskellBasicType TInt64    = con0 "Int64"
haskellBasicType TUInt64   = con0 "Word64"
haskellBasicType TGType    = con0 "GType"
haskellBasicType TUTF8     = con0 "T.Text"
haskellBasicType TFloat    = con0 "Float"
haskellBasicType TDouble   = con0 "Double"
haskellBasicType TUniChar  = con0 "Char"
haskellBasicType TFileName = con0 "[Char]"
haskellBasicType TIntPtr   = con0 "CIntPtr"
haskellBasicType TUIntPtr  = con0 "CUIntPtr"

-- | This translates GI types to the types used for generated Haskell code.
haskellType :: Type -> CodeGen TypeRep
haskellType (TBasicType bt) = return $ haskellBasicType bt
-- There is no great choice in this case, so we simply pass the
-- pointer along. This is useful for GdkPixbufNotify, for example.
haskellType t@(TCArray False (-1) (-1) (TBasicType TUInt8)) =
  foreignType t
haskellType (TCArray _ _ _ (TBasicType TUInt8)) =
  return $ "ByteString" `con` []
haskellType (TCArray _ _ _ a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGArray a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TPtrArray a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TByteArray) = return $ "ByteString" `con` []
haskellType (TGList a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGSList a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGHash a b) = do
  innerA <- haskellType a
  innerB <- haskellType b
  return $ "Map.Map" `con` [innerA, innerB]
haskellType TError = return $ "GError" `con` []
haskellType TVariant = return $ "GVariant" `con` []
haskellType TParamSpec = return $ "GParamSpec" `con` []
haskellType (TGClosure (Just inner@(TInterface n))) = do
  innerAPI <- getAPI inner
  case innerAPI of
    APICallback _ -> do
      let n' = normalizedAPIName innerAPI n
      tname <- qualifiedSymbol (callbackCType $ name n') n
      return $ "GClosure" `con` [con0 tname]
    -- The given inner type does not make sense, so we treat it as an
    -- untyped closure.
    _ -> haskellType (TGClosure Nothing)
haskellType (TGClosure _) = do
  tyvar <- getFreshTypeVariable
  return $ "GClosure" `con` [con0 tyvar]
haskellType TGValue = return $ "GValue" `con` []
haskellType t@(TInterface n) = do
  api <- getAPI t
  tname <- qualifiedAPI api n
  return $ case api of
             (APIFlags _) -> "[]" `con` [tname `con` []]
             _ -> tname `con` []

-- | Whether the callable has closure arguments (i.e. "user_data"
-- style arguments).
callableHasClosures :: Callable -> Bool
callableHasClosures = any (/= -1) . map argClosure . args

-- | Check whether the given type corresponds to a callback.
typeIsCallback :: Type -> CodeGen Bool
typeIsCallback t@(TInterface _) = do
  api <- findAPI t
  case api of
    Just (APICallback _) -> return True
    _ -> return False
typeIsCallback _ = return False

-- | Basically like `haskellType`, but for types which admit a
-- "isomorphic" version of the Haskell type distinct from the usual
-- Haskell type.  Generally the Haskell type we expose is isomorphic
-- to the foreign type, but in some cases, such as callbacks with
-- closure arguments, this does not hold, as we omit the closure
-- arguments. This function returns a type which is actually
-- isomorphic. There is another case this function deals with: for
-- convenience untyped `TGClosure` types have a type variable on the
-- Haskell side when they are arguments to functions, but we do not
-- want this when they appear as arguments to callbacks/signals, or
-- return types of properties, as it would force the type synonym/type
-- family to depend on the type variable.
isoHaskellType :: Type -> CodeGen TypeRep
isoHaskellType (TGClosure Nothing) =
  return $ "GClosure" `con` [con0 "()"]
isoHaskellType t@(TInterface n) = do
  api <- findAPI t
  case api of
    Just apiCB@(APICallback cb) -> do
        tname <- qualifiedAPI apiCB n
        if callableHasClosures (cbCallable cb)
        then return ((callbackHTypeWithClosures tname) `con` [])
        else return (tname `con` [])
    _ -> haskellType t
isoHaskellType t = haskellType t

-- | Foreign (C) type associated to one of the basic types.
foreignBasicType :: BasicType -> TypeRep
foreignBasicType TBoolean  = "CInt" `con` []
foreignBasicType TUTF8     = "CString" `con` []
foreignBasicType TFileName = "CString" `con` []
foreignBasicType TUniChar  = "CInt" `con` []
foreignBasicType TFloat    = "CFloat" `con` []
foreignBasicType TDouble   = "CDouble" `con` []
foreignBasicType TGType    = "CGType" `con` []
foreignBasicType t         = haskellBasicType t

-- This translates GI types to the types used in foreign function calls.
foreignType :: Type -> CodeGen TypeRep
foreignType (TBasicType t) = return $ foreignBasicType t
foreignType (TCArray _ _ _ TGValue) = return $ ptr ("B.GValue.GValue" `con` [])
foreignType (TCArray zt _ _ t) = do
  api <- findAPI t
  let size = case api of
               Just (APIStruct s) -> structSize s
               Just (APIUnion u) -> unionSize u
               _ -> 0
  if size == 0 || zt
  then ptr <$> foreignType t
  else foreignType t
foreignType (TGArray a) = do
  inner <- foreignType a
  return $ ptr ("GArray" `con` [inner])
foreignType (TPtrArray a) = do
  inner <- foreignType a
  return $ ptr ("GPtrArray" `con` [inner])
foreignType (TByteArray) = return $ ptr ("GByteArray" `con` [])
foreignType (TGList a) = do
  inner <- foreignType a
  return $ ptr ("GList" `con` [inner])
foreignType (TGSList a) = do
  inner <- foreignType a
  return $ ptr ("GSList" `con` [inner])
foreignType (TGHash a b) = do
  innerA <- foreignType a
  innerB <- foreignType b
  return $ ptr ("GHashTable" `con` [innerA, innerB])
foreignType t@TError = ptr <$> haskellType t
foreignType t@TVariant = ptr <$> haskellType t
foreignType t@TParamSpec = ptr <$> haskellType t
foreignType (TGClosure Nothing) = return $ ptr ("GClosure" `con` [con0 "()"])
foreignType t@(TGClosure (Just _)) = ptr <$> haskellType t
foreignType t@(TGValue) = ptr <$> haskellType t
foreignType t@(TInterface n) = do
  api <- getAPI t
  let enumIsSigned e = any (< 0) (map enumMemberValue (enumMembers e))
      ctypeForEnum e = if enumIsSigned e
                       then "CInt"
                       else "CUInt"
  case api of
    APIEnum e -> return $ (ctypeForEnum e) `con` []
    APIFlags (Flags e) -> return $ (ctypeForEnum e) `con` []
    APICallback _ -> do
      let n' = normalizedAPIName api n
      tname <- qualifiedSymbol (callbackCType $ name n') n
      return (funptr $ tname `con` [])
    _ -> do
      tname <- qualifiedAPI api n
      return (ptr $ tname `con` [])

-- | Whether the give type corresponds to an enum or flag.
typeIsEnumOrFlag :: Type -> CodeGen Bool
typeIsEnumOrFlag t = do
  a <- findAPI t
  case a of
    Nothing -> return False
    (Just (APIEnum _)) -> return True
    (Just (APIFlags _)) -> return True
    _ -> return False

-- | Information on how to allocate a type: allocator function and
-- size of the struct.
data TypeAllocInfo = TypeAlloc Text Int

-- | Information on how to allocate the given type, if known.
typeAllocInfo :: Type -> CodeGen (Maybe TypeAllocInfo)
typeAllocInfo TGValue =
  let n = #{size GValue}
  in return $ Just $ TypeAlloc ("SP.callocBytes " <> tshow n) n
typeAllocInfo (TGArray t) = do
  api <- findAPI t
  case api of
    Just (APIStruct s) -> case structSize s of
                            0 -> return Nothing
                            n -> let allocator = "B.GArray.allocGArray " <> tshow n
                                 in return $ Just $ TypeAlloc allocator n
    _ -> return Nothing
typeAllocInfo t = do
  api <- findAPI t
  case api of
    Just (APIStruct s) ->
      case structSize s of
        0 -> return Nothing
        n -> let allocator = if structIsBoxed s
                             then "SP.callocBoxedBytes"
                             else "SP.callocBytes"
             in return $ Just $ TypeAlloc (allocator <> " " <> tshow n) n
    _ -> return Nothing

-- | Returns whether the given type corresponds to a `ManagedPtr`
-- instance (a thin wrapper over a `ForeignPtr`).
isManaged   :: Type -> CodeGen Bool
isManaged TError = return True
isManaged TVariant = return True
isManaged TGValue = return True
isManaged TParamSpec = return True
isManaged (TGClosure _) = return True
isManaged t@(TInterface _) = do
  a <- findAPI t
  case a of
    Just (APIObject _)    -> return True
    Just (APIInterface _) -> return True
    Just (APIStruct _)    -> return True
    Just (APIUnion _)     -> return True
    _                     -> return False
isManaged _ = return False

-- | Returns whether the given type is represented by a pointer on the
-- C side.
typeIsPtr :: Type -> CodeGen Bool
typeIsPtr t = isJust <$> typePtrType t

-- | Distinct types of foreign pointers.
data FFIPtrType = FFIPtr    -- ^ Ordinary `Ptr`.
                | FFIFunPtr -- ^ `FunPtr`.

-- | For those types represented by pointers on the C side, return the
-- type of pointer which represents them on the Haskell FFI.
typePtrType :: Type -> CodeGen (Maybe FFIPtrType)
typePtrType (TBasicType TPtr) = return (Just FFIPtr)
typePtrType (TBasicType TUTF8) = return (Just FFIPtr)
typePtrType (TBasicType TFileName) = return (Just FFIPtr)
typePtrType t = do
  ft <- foreignType t
  case typeConName ft of
    "Ptr"    -> return (Just FFIPtr)
    "FunPtr" -> return (Just FFIFunPtr)
    _        -> return Nothing

-- | If the passed in type is nullable, return the conversion function
-- between the FFI pointer type (may be a `Ptr` or a `FunPtr`) and the
-- corresponding `Maybe` type.
maybeNullConvert :: Type -> CodeGen (Maybe Text)
maybeNullConvert (TBasicType TPtr) = return Nothing
maybeNullConvert (TGList _) = return Nothing
maybeNullConvert (TGSList _) = return Nothing
maybeNullConvert t = do
  pt <- typePtrType t
  case pt of
    Just FFIPtr -> return (Just "SP.convertIfNonNull")
    Just FFIFunPtr -> return (Just "SP.convertFunPtrIfNonNull")
    Nothing -> return Nothing

-- | An appropriate NULL value for the given type, for types which are
-- represented by pointers on the C side.
nullPtrForType :: Type -> CodeGen (Maybe Text)
nullPtrForType t = do
  pt <- typePtrType t
  case pt of
    Just FFIPtr -> return (Just "FP.nullPtr")
    Just FFIFunPtr -> return (Just "FP.nullFunPtr")
    Nothing -> return Nothing

-- | Returns whether the given type should be represented by a
-- `Maybe` type on the Haskell side. This applies to all properties
-- which have a C representation in terms of pointers, except for
-- G(S)Lists, for which NULL is a valid G(S)List, and raw pointers,
-- which we just pass through to the Haskell side. Notice that
-- introspection annotations can override this.
typeIsNullable :: Type -> CodeGen Bool
typeIsNullable t = isJust <$> maybeNullConvert t

-- | If the given type maps to a list in Haskell, return the type of the
-- elements, and the function that maps over them.
elementTypeAndMap :: Type -> Text -> Maybe (Type, Text)
-- ByteString
elementTypeAndMap (TCArray _ _ _ (TBasicType TUInt8)) _ = Nothing
elementTypeAndMap (TCArray True _ _ t) _ = Just (t, "mapZeroTerminatedCArray")
elementTypeAndMap (TCArray _ _ _ TGValue) len =
  Just (TGValue, parenthesize $ "B.GValue.mapGValueArrayWithLength " <> len)
elementTypeAndMap (TCArray False (-1) _ t) len =
  Just (t, parenthesize $ "mapCArrayWithLength " <> len)
elementTypeAndMap (TCArray False fixed _ t) _ =
  Just (t, parenthesize $ "mapCArrayWithLength " <> tshow fixed)
elementTypeAndMap (TGArray t) _ = Just (t, "mapGArray")
elementTypeAndMap (TPtrArray t) _ = Just (t, "mapPtrArray")
elementTypeAndMap (TGList t) _ = Just (t, "mapGList")
elementTypeAndMap (TGSList t) _ = Just (t, "mapGSList")
-- GHashTable is treated separately, see Transfer.hs
elementTypeAndMap _ _ = Nothing

-- Return just the element type.
elementType :: Type -> Maybe Type
elementType t = fst <$> elementTypeAndMap t undefined

-- Return just the map.
elementMap :: Type -> Text -> Maybe Text
elementMap t len = snd <$> elementTypeAndMap t len
