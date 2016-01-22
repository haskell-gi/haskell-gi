module GI.Struct ( genStructOrUnionFields
                 , extractCallbacksInStruct
                 , fixAPIStructs
                 , ignoreStruct)
    where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, unless, when)

import Data.Maybe (mapMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import GI.API
import GI.Conversions
import GI.Code
import GI.SymbolNaming
import GI.Type
import GI.Util

-- | Whether (not) to generate bindings for the given struct.
ignoreStruct :: Name -> Struct -> Bool
ignoreStruct (Name _ name) s = isJust (gtypeStructFor s) ||
                               "Private" `T.isSuffixOf` name

-- | Canonical name for the type of a callback type embedded in a
-- struct field.
fieldCallbackType :: Text -> Field -> Text
fieldCallbackType structName field =
    structName <> (underscoresToCamelCase . fieldName) field <> "FieldCallback"

-- | Fix the interface names of callback fields in the struct to
-- correspond to the ones that we are going to generate.
fixCallbackStructFields :: Name -> Struct -> Struct
fixCallbackStructFields (Name ns structName) s = s {structFields = fixedFields}
    where fixedFields :: [Field]
          fixedFields = map fixField (structFields s)

          fixField :: Field -> Field
          fixField field =
              case fieldCallback field of
                Nothing -> field
                Just _ -> let n' = fieldCallbackType structName field
                          in field {fieldType = TInterface ns n'}

-- | Fix the interface names of callback fields in an APIStruct to
-- correspond to the ones that we are going to generate. If something
-- other than an APIStruct is passed in we don't touch it.
fixAPIStructs :: (Name, API) -> (Name, API)
fixAPIStructs (n, APIStruct s) = (n, APIStruct $ fixCallbackStructFields n s)
fixAPIStructs api = api

-- | Extract the callback types embedded in the fields of structs, and
-- at the same time fix the type of the corresponding fields. Returns
-- the list of APIs associated to this struct, not including the
-- struct itself.
extractCallbacksInStruct :: (Name, API) -> [(Name, API)]
extractCallbacksInStruct (n@(Name ns structName), APIStruct s)
    | ignoreStruct n s = []
    | otherwise =
        mapMaybe callbackInField (structFields s)
            where callbackInField :: Field -> Maybe (Name, API)
                  callbackInField field = do
                    callback <- fieldCallback field
                    let n' = fieldCallbackType structName field
                    return (Name ns n', APICallback callback)
extractCallbacksInStruct _ = []

buildFieldGetter :: Name -> Field -> ExcCodeGen ()
buildFieldGetter n@(Name ns _) field = do
  name' <- upperName n

  hType <- tshow <$> haskellType (fieldType field)
  fType <- tshow <$> foreignType (fieldType field)
  unless ("Private" `T.isSuffixOf` hType) $ do
     fName <- upperName $ Name ns (fieldName field)
     let getter = lcFirst name' <> "Read" <> fName
     line $ getter <> " :: " <> name' <> " -> IO " <>
                 if T.any (== ' ') hType
                 then parenthesize hType
                 else hType
     line $ getter <> " s = withManagedPtr s $ \\ptr -> do"
     indent $ do
       line $ "val <- peek (ptr `plusPtr` " <> tshow (fieldOffset field)
            <> ") :: IO " <> if T.any (== ' ') fType
                            then parenthesize fType
                            else fType
       result <- convert "val" $ fToH (fieldType field) TransferNothing
       line $ "return " <> result

     export getter

genStructOrUnionFields :: Name -> [Field] -> CodeGen ()
genStructOrUnionFields n fields = do
  name' <- upperName n

  forM_ fields $ \field -> when (fieldVisible field) $ group $
      handleCGExc (\e -> line ("-- XXX Skipped getter for \"" <> name' <>
                               ":" <> fieldName field <> "\" :: " <>
                               describeCGError e))
                  (buildFieldGetter n field)
