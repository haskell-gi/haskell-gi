module Data.GI.CodeGen.OverloadedLabels
    ( genOverloadedLabels
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Maybe (isNothing)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Control.Monad (forM_)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.SymbolNaming
import Data.GI.CodeGen.Util (lcFirst)

-- | A list of all overloadable identifiers in the set of APIs (current
-- properties and methods).
findOverloaded :: [(Name, API)] -> CodeGen [Text]
findOverloaded apis = S.toList <$> go apis S.empty
    where
      go :: [(Name, API)] -> S.Set Text -> CodeGen (S.Set Text)
      go [] set = return set
      go ((_, api):apis) set =
        case api of
          APIInterface iface -> go apis (scanInterface iface set)
          APIObject object -> go apis (scanObject object set)
          APIStruct s -> go apis (scanStruct s set)
          APIUnion u -> go apis (scanUnion u set)
          _ -> go apis set

      scanObject :: Object -> S.Set Text -> S.Set Text
      scanObject o set =
          let props = (map propToLabel . objProperties) o
              methods = (map methodToLabel . filterMethods . objMethods) o
          in S.unions [set, S.fromList props, S.fromList methods]

      scanInterface :: Interface -> S.Set Text -> S.Set Text
      scanInterface i set =
          let props = (map propToLabel . ifProperties) i
              methods = (map methodToLabel . filterMethods . ifMethods) i
          in S.unions [set, S.fromList props, S.fromList methods]

      scanStruct :: Struct -> S.Set Text -> S.Set Text
      scanStruct s set =
          let attrs = (map fieldToLabel . filterFields . structFields) s
              methods = (map methodToLabel . filterMethods . structMethods) s
          in S.unions [set, S.fromList attrs, S.fromList methods]

      scanUnion :: Union -> S.Set Text -> S.Set Text
      scanUnion u set =
          let attrs = (map fieldToLabel . filterFields . unionFields) u
              methods = (map methodToLabel . filterMethods . unionMethods) u
          in S.unions [set, S.fromList attrs, S.fromList methods]

      propToLabel :: Property -> Text
      propToLabel = lcFirst . hyphensToCamelCase . propName

      methodToLabel :: Method -> Text
      methodToLabel = lowerName . methodName

      fieldToLabel :: Field -> Text
      fieldToLabel = lcFirst . underscoresToCamelCase . fieldName

      filterMethods :: [Method] -> [Method]
      filterMethods = filter (\m -> (isNothing . methodMovedTo) m &&
                                    methodType m == OrdinaryMethod)

      filterFields :: [Field] -> [Field]
      filterFields = filter (\f -> fieldVisible f &&
                            (not . T.null . fieldName) f)

genOverloadedLabel :: Text -> CodeGen ()
genOverloadedLabel l = group $ do
  line $ "_" <> l <> " :: IsLabelProxy \"" <> l <> "\" a => a"
  line $ "_" <> l <> " = fromLabelProxy (Proxy :: Proxy \""
           <> l <> "\")"
  export ToplevelSection ("_" <> l)

genOverloadedLabels :: [(Name, API)] -> CodeGen ()
genOverloadedLabels allAPIs = do
  setLanguagePragmas ["DataKinds", "FlexibleContexts", "CPP"]
  setModuleFlags [ImplicitPrelude]

  line $ "import Data.Proxy (Proxy(..))"
  line $ "import Data.GI.Base.Overloading (IsLabelProxy(..))"
  blank

  labels <- findOverloaded allAPIs
  forM_ labels $ \l -> do
      genOverloadedLabel l
      blank
