module Data.GI.CodeGen.OverloadedSignals
    ( genObjectSignals
    , genInterfaceSignals
    , genOverloadedSignalConnectors
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Inheritance (fullObjectSignalList, fullInterfaceSignalList)
import Data.GI.CodeGen.GObject (apiIsGObject)
import Data.GI.CodeGen.SymbolNaming (upperName, hyphensToCamelCase,
                                     signalInfoName)
import Data.GI.CodeGen.Util (lcFirst)

-- A list of distinct signal names for all GObjects appearing in the
-- given list of APIs.
findSignalNames :: [(Name, API)] -> CodeGen [Text]
findSignalNames apis = S.toList <$> go apis S.empty
    where
      go :: [(Name, API)] -> S.Set Text -> CodeGen (S.Set Text)
      go [] set = return set
      go ((_, api):apis) set =
          case api of
            APIInterface iface ->
                go apis $ insertSignals (ifSignals iface) set
            APIObject object ->
                go apis $ insertSignals (objSignals object) set
            _ -> go apis set

      insertSignals :: [Signal] -> S.Set Text -> S.Set Text
      insertSignals props set = foldr (S.insert . sigName) set props

-- | Generate the overloaded signal connectors: "Clicked", "ActivateLink", ...
genOverloadedSignalConnectors :: [(Name, API)] -> CodeGen ()
genOverloadedSignalConnectors allAPIs = do
  setLanguagePragmas ["DataKinds", "PatternSynonyms", "CPP",
                      -- For ghc 7.8 support
                      "RankNTypes", "ScopedTypeVariables", "TypeFamilies"]
  setModuleFlags [ImplicitPrelude]

  line "import Data.GI.Base.Signals (SignalProxy(..))"
  line "import Data.GI.Base.Overloading (ResolveSignal)"
  blank
  signalNames <- findSignalNames allAPIs
  forM_ signalNames $ \sn -> group $ do
    let camelName = hyphensToCamelCase sn
    line $ "pattern " <> camelName <>
             " :: SignalProxy object (ResolveSignal \""
             <> lcFirst camelName <> "\" object)"
    line $ "pattern " <> camelName <> " = SignalProxy"
    exportDecl $ "pattern " <> camelName

-- | Signal instances for (GObject-derived) objects.
genObjectSignals :: Name -> Object -> CodeGen ()
genObjectSignals n o = do
  let name = upperName n
  isGO <- apiIsGObject n (APIObject o)
  when isGO $ do
       infos <- fullObjectSignalList n o >>=
                mapM (\(owner, signal) -> do
                      si <- signalInfoName owner signal
                      return $ "'(\"" <> (lcFirst . hyphensToCamelCase . sigName) signal
                                 <> "\", " <> si <> ")")
       group $ do
         let signalListType = name <> "SignalList"
         line $ "type instance O.SignalList " <> name <> " = " <> signalListType
         line $ "type " <> signalListType <> " = ('[ "
                  <> T.intercalate ", " infos <> "] :: [(Symbol, *)])"

-- | Signal instances for interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen ()
genInterfaceSignals n iface = do
  let name = upperName n
  infos <- fullInterfaceSignalList n iface >>=
           mapM (\(owner, signal) -> do
                   si <- signalInfoName owner signal
                   return $ "'(\"" <> (lcFirst . hyphensToCamelCase . sigName) signal
                              <> "\", " <> si <> ")")
  group $ do
    let signalListType = name <> "SignalList"
    line $ "type instance O.SignalList " <> name <> " = " <> signalListType
    line $ "type " <> signalListType <> " = ('[ "
             <> T.intercalate ", " infos <> "] :: [(Symbol, *)])"
