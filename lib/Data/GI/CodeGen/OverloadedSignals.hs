module Data.GI.CodeGen.OverloadedSignals
    ( genObjectSignals
    , genInterfaceSignals
    , genOverloadedSignalConnectors
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Inheritance (fullObjectSignalList, fullInterfaceSignalList)
import Data.GI.CodeGen.GObject (apiIsGObject)
import Data.GI.CodeGen.Signal (signalHaskellName)
import Data.GI.CodeGen.SymbolNaming (upperName, hyphensToCamelCase,
                                     qualifiedSymbol)
import Data.GI.CodeGen.Util (lcFirst, ucFirst)

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
    line $ "#if MIN_VERSION_base(4,8,0)"
    line $ "pattern " <> camelName <>
             " :: SignalProxy object (ResolveSignal \""
             <> lcFirst camelName <> "\" object)"
    line $ "pattern " <> camelName <> " = SignalProxy"
    line $ "#else"
    line $ "pattern " <> camelName <> " = SignalProxy :: forall info object. "
             <> "info ~ ResolveSignal \"" <> lcFirst camelName
             <> "\" object => SignalProxy object info"
    line $ "#endif"
    exportDecl $ "pattern " <> camelName

-- | Qualified name for the "(sigName, info)" tag for a given signal.
signalInfoName :: Name -> Signal -> CodeGen Text
signalInfoName n signal = do
  let infoName = upperName n <> (ucFirst . signalHaskellName . sigName) signal
                 <> "SignalInfo"
  qualifiedSymbol infoName n

-- | Generate the given signal instance for the given API object.
genInstance :: Name -> Signal -> CodeGen ()
genInstance owner signal = group $ do
  let name = upperName owner
  let sn = (ucFirst . signalHaskellName . sigName) signal
  si <- signalInfoName owner signal
  bline $ "data " <> si
  line $ "instance SignalInfo " <> si <> " where"
  indent $ do
      let signalConnectorName = name <> sn
          cbHaskellType = signalConnectorName <> "Callback"
      line $ "type HaskellCallbackType " <> si <> " = " <> cbHaskellType
      line $ "connectSignal _ = " <> "connect" <> name <> sn
  exportSignal sn si

-- | Signal instances for (GObject-derived) objects.
genObjectSignals :: Name -> Object -> CodeGen ()
genObjectSignals n o = do
  let name = upperName n
  isGO <- apiIsGObject n (APIObject o)
  when isGO $ do
       mapM_ (genInstance n) (objSignals o)
       infos <- fullObjectSignalList n o >>=
                mapM (\(owner, signal) -> do
                      si <- signalInfoName owner signal
                      return $ "'(\"" <> (lcFirst . hyphensToCamelCase . sigName) signal
                                 <> "\", " <> si <> ")")
       group $ do
         let signalListType = name <> "SignalList"
         line $ "type instance SignalList " <> name <> " = " <> signalListType
         line $ "type " <> signalListType <> " = ('[ "
                  <> T.intercalate ", " infos <> "] :: [(Symbol, *)])"

-- | Signal instances for interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen ()
genInterfaceSignals n iface = do
  let name = upperName n
  mapM_ (genInstance n) (ifSignals iface)
  infos <- fullInterfaceSignalList n iface >>=
           mapM (\(owner, signal) -> do
                   si <- signalInfoName owner signal
                   return $ "'(\"" <> (lcFirst . hyphensToCamelCase . sigName) signal
                              <> "\", " <> si <> ")")
  group $ do
    let signalListType = name <> "SignalList"
    line $ "type instance SignalList " <> name <> " = " <> signalListType
    line $ "type " <> signalListType <> " = ('[ "
             <> T.intercalate ", " infos <> "] :: [(Symbol, *)])"
