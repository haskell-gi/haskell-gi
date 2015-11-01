module GI.Constant
    ( genConstant
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import qualified Data.Text as T
import Data.Text (Text)

import GI.API
import GI.Code
import GI.Conversions
import GI.Type

genConstant :: Name -> Constant -> CodeGen ()
genConstant (Name _ name) (Constant t value deprecated) = do
  line $ "-- constant " ++ name
  line $ deprecatedPragma name deprecated

  handleCGExc (\e -> line $ "-- XXX: Could not generate constant: " ++ describeCGError e)
              (assignValue name t value)

-- | Assign to the given name the given constant value, in a way that
-- can be assigned to the corresponding Haskell type.
assignValue :: String -> Type -> Text -> ExcCodeGen ()
assignValue name t@(TBasicType b) value = do
  ht <- haskellType t
  line $ name ++ " :: " ++ show ht
  hv <- showBasicType b value
  line $ name ++ " = " ++ hv
assignValue name t@(TInterface _ _) value = do
  ht <- haskellType t
  api <- findAPI t
  case api of
    Just (APIEnum _) -> do
             line $ name ++ " :: " ++ show ht
             line $ name ++ " = toEnum " ++ T.unpack value
    Just (APIFlags _) -> do
             line $ name ++ " :: " ++ show ht
             line $ name ++ " = wordToGFlags " ++ T.unpack value
    Just (APIStruct s) | structIsBoxed s == False -> do
             line $ name ++ " :: IO " ++ show ht
             line $ name ++ " = do"
             indent $ do
               line $ "let ptr = intPtrToPtr " ++ T.unpack value
               wrapped <- convert "ptr" (fToH t TransferNothing)
               line $ "return " ++ wrapped
    _ -> notImplementedError $ "Don't know how to treat constants of type " ++ show t
assignValue _ t _ = notImplementedError $ "Don't know how to treat constants of type " ++ show t

-- | Show a basic type, in a way that can be assigned to the
-- corresponding Haskell type.
showBasicType                  :: BasicType -> Text -> ExcCodeGen String
showBasicType TInt8    i       = return $ T.unpack i
showBasicType TUInt8   i       = return $ T.unpack i
showBasicType TInt16   i       = return $ T.unpack i
showBasicType TUInt16  i       = return $ T.unpack i
showBasicType TInt32   i       = return $ T.unpack i
showBasicType TUInt32  i       = return $ T.unpack i
showBasicType TInt64   i       = return $ T.unpack i
showBasicType TUInt64  i       = return $ T.unpack i
showBasicType TBoolean "0"     = return "False"
showBasicType TBoolean "false" = return "False"
showBasicType TBoolean "1"     = return "True"
showBasicType TBoolean "true"  = return "True"
showBasicType TBoolean b       = notImplementedError $ "Could not parse boolean \"" ++ T.unpack b ++ "\""
showBasicType TFloat   f       = return $ T.unpack f
showBasicType TDouble  d       = return $ T.unpack d
showBasicType TUTF8    s       = return $ show s
showBasicType TFileName fn     = return $ show fn
showBasicType TUniChar c       = return $ "'" ++ T.unpack c ++ "'"
showBasicType TVoid    ptr     = return $ "intPtrToPtr " ++ T.unpack ptr
showBasicType TGType   gtype   = return $ "GType " ++ T.unpack gtype
