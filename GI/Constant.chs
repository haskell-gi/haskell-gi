module GI.Constant
    ( genConstant
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


import Control.Monad.IO.Class (liftIO)

import Data.Char (chr)
import Foreign (peekByteOff, Ptr)
import Foreign.C

import GI.API
import GI.Code
import GI.Conversions
import GI.Type
import GI.Internal.ArgInfo (Transfer(TransferNothing))
import GI.Internal.Types (Argument(..))

#include <girepository.h>

genConstant :: Name -> Constant -> CodeGen ()
genConstant (Name _ name) (Constant t value) = do
  line $ "-- constant " ++ name

  handleCGExc (\e -> line $ "-- XXX: Could not generate constant: " ++ describeCGError e)
              (assignValue name t value)

-- | Assign to the given name the given constant value, in a way that
-- can be assigned to the corresponding Haskell type.
assignValue :: String -> Type -> Argument -> ExcCodeGen ()
assignValue name t@(TBasicType b) (Argument arg) = do
  ht <- haskellType t
  line $ name ++ " :: " ++ show ht
  value <- showBasicType b arg
  line $ name ++ " = " ++ value
assignValue name t@(TInterface _ _) (Argument arg) = do
  ht <- haskellType t
  api <- findAPI t
  case api of
    Just (APIEnum _) -> do
             value <- liftIO $ {# get GIArgument->v_int64 #} arg
             line $ name ++ " :: " ++ show ht
             line $ name ++ " = toEnum " ++ show value
    Just (APIFlags _) -> do
             value <- liftIO $ {# get GIArgument->v_int64 #} arg
             line $ name ++ " :: " ++ show ht
             line $ name ++ " = wordToGFlags " ++ show value
    Just (APIStruct s) | structIsBoxed s == False -> do
             line $ name ++ " :: IO " ++ show ht
             ptr <- liftIO $ {# get GIArgument->v_pointer #} arg
             line $ name ++ " = do"
             indent $ do
               line $ "let ptr = intPtrToPtr " ++ show ptr
               wrapped <- convert "ptr" (fToH t TransferNothing)
               line $ "return " ++ wrapped
    _ -> notImplementedError $ "Don't know how to treat constants of type " ++ show t
assignValue _ t _ = notImplementedError $ "Don't know how to treat constants of type " ++ show t

-- | Show the result of an IO action in the CodeGen monad.
showIO :: Show a => IO a -> CodeGen String
showIO x = liftIO (show <$> x)

-- | Show a basic type, in a way that can be assigned to the
-- corresponding Haskell type.
showBasicType :: BasicType -> Ptr Argument -> CodeGen String
showBasicType TInt8 arg = showIO $ {# get GIArgument->v_int8 #} arg
showBasicType TUInt8 arg = showIO $ {# get GIArgument->v_uint8 #} arg
showBasicType TInt16 arg = showIO $ {# get GIArgument->v_int16 #} arg
showBasicType TUInt16 arg = showIO $ {# get GIArgument->v_uint16 #} arg
showBasicType TInt32 arg = showIO $ {# get GIArgument->v_int32 #} arg
showBasicType TUInt32 arg = showIO $ {# get GIArgument->v_uint32 #} arg
showBasicType TInt64 arg = showIO $ {# get GIArgument->v_int64 #} arg
showBasicType TUInt64 arg = showIO $ {# get GIArgument->v_uint64 #} arg
showBasicType TBoolean arg = showIO ((/= 0) <$> {# get GIArgument->v_boolean #} arg)
showBasicType TFloat arg = showIO $ {# get GIArgument->v_float #} arg
showBasicType TDouble arg = showIO $ {# get GIArgument->v_double #} arg
showBasicType TUTF8 arg = showIO (peekCString =<< {# get GIArgument->v_string #} arg)
showBasicType TFileName arg = showIO $ (peekCString =<< {# get GIArgument->v_string #} arg)
showBasicType TUniChar arg = showIO ((chr . fromIntegral) <$> {# get GIArgument->v_int32 #} arg)
showBasicType TVoid arg = liftIO $ do
  ptr <- {# get GIArgument->v_pointer #} arg
  return $ "intPtrToPtr " ++ show ptr
showBasicType TGType arg = liftIO $ do
  gtype <- {# get GIArgument->v_uint32 #} arg
  return $ "GType " ++ show gtype
