module GI.Value
    ( Value(..)
    , fromArgument
    , valueType
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Data.Int
import Data.Word

import Foreign (peekByteOff,Ptr)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Type
import GI.Internal.Types

#include <girepository.h>

data Value
    = VVoid
    | VBoolean Bool
    | VInt8 Int8
    | VUInt8 Word8
    | VInt16 Int16
    | VUInt16 Word16
    | VInt32 Int32
    | VUInt32 Word32
    | VInt64 Int64
    | VUInt64 Word64
    | VFloat Float
    | VDouble Double
    | VGType Word32
    | VUTF8 String
    | VFileName String
    deriving (Eq, Show)

valueType :: Value -> Type
valueType VVoid           = TBasicType TVoid
valueType (VBoolean _)    = TBasicType TBoolean
valueType (VInt8 _)       = TBasicType TInt8
valueType (VUInt8 _)      = TBasicType TUInt8
valueType (VInt16 _)      = TBasicType TInt16
valueType (VUInt16 _)     = TBasicType TUInt16
valueType (VInt32 _)      = TBasicType TInt32
valueType (VUInt32 _)     = TBasicType TUInt32
valueType (VInt64 _)      = TBasicType TInt64
valueType (VUInt64 _)     = TBasicType TUInt64
valueType (VFloat _)      = TBasicType TFloat
valueType (VDouble _)     = TBasicType TDouble
valueType (VGType _)      = TBasicType TGType
valueType (VUTF8 _)       = TBasicType TUTF8
valueType (VFileName _)   = TBasicType TFileName

fromArgument :: TypeInfo -> Argument -> Value
fromArgument ti (Argument arg) =
    case typeFromTypeInfo ti of
        TBasicType t -> unsafePerformIO $ basic t
        t -> error $ "don't know how to decode argument of type " ++
                show t

    where

    basic TInt8 = VInt8 <$> fromIntegral <$> {# get GIArgument->v_int8 #} arg
    basic TUInt8 = VUInt8 <$> fromIntegral <$> {# get GIArgument->v_uint8 #} arg
    basic TInt16 = VInt16 <$> fromIntegral <$> {# get GIArgument->v_int16 #} arg
    basic TUInt16 = VUInt16 <$> fromIntegral <$> {# get GIArgument->v_uint16 #} arg
    basic TInt32 = VInt32 <$> fromIntegral <$> {# get GIArgument->v_int32 #} arg
    basic TUInt32 = VUInt32 <$> fromIntegral <$> {# get GIArgument->v_uint32 #} arg
    basic TInt64 = VInt64 <$> fromIntegral <$> {# get GIArgument->v_int64 #} arg
    basic TUInt64 = VUInt64 <$> fromIntegral <$> {# get GIArgument->v_uint64 #} arg
    basic TBoolean = VBoolean <$> (/= 0) <$> {# get GIArgument->v_boolean #} arg
    -- XXX: Loss of precision?
    basic TFloat = VFloat <$> fromRational <$> toRational <$>  {# get GIArgument->v_float #} arg
    -- XXX: Loss of precision?
    basic TDouble = VDouble <$> fromRational <$> toRational <$>  {# get GIArgument->v_double #} arg
    basic TUTF8 = VUTF8 <$> (peekCString =<< {# get GIArgument->v_string #} arg)
    basic t = error $ "a: implement me: " ++ show t
