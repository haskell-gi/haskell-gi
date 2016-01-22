module GI.OverloadedSignals
    ( genObjectSignals
    , genInterfaceSignals
    , genOverloadedSignalConnectors
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S

import GI.API
import GI.Code
import GI.Inheritance (fullObjectSignalList, fullInterfaceSignalList)
import GI.GObject (apiIsGObject)
import GI.Signal (signalHaskellName)
import GI.SymbolNaming (upperName)
import GI.Util (padTo, ucFirst)

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
  setLanguagePragmas ["DataKinds", "GADTs", "KindSignatures", "FlexibleInstances"]
  setModuleFlags [ImplicitPrelude, NoTypesImport, NoCallbacksImport]

  line   "import GHC.TypeLits"
  line   "import GHC.Exts (Constraint)"
  blank
  line   "class NoConstraint a"
  line   "instance NoConstraint a"
  blank
  line   "data SignalProxy (a :: Symbol) (b :: Symbol) (c :: * -> Constraint) where"
  indent $ do
    signalNames <- findSignalNames allAPIs
    let maxLength = maximum $ map (T.length . signalHaskellName) signalNames
    forM_ signalNames $ \sn ->
        line $ padTo (maxLength + 1) (ucFirst (signalHaskellName sn)) <>
                 ":: SignalProxy \"" <> sn <> "\" \"\" NoConstraint"
  export "SignalProxy(..)"

-- | Qualified name for the "(sigName, info)" tag for a given signal.
signalInfoName :: Name -> Signal -> CodeGen Text
signalInfoName n signal = do
  n' <- upperName n
  return $ n' <> (ucFirst . signalHaskellName . sigName) signal
             <> "SignalInfo"

-- | Generate the given signal instance for the given API object.
genInstance :: Name -> Signal -> CodeGen ()
genInstance owner signal = group $ do
  name <- upperName owner
  let sn = (ucFirst . signalHaskellName . sigName) signal
  si <- signalInfoName owner signal
  bline $ "data " <> si
  line $ "instance SignalInfo " <> si <> " where"
  indent $ do
      let signalConnectorName = name <> sn
          cbHaskellType = signalConnectorName <> "Callback"
      line $ "type HaskellCallbackType " <> si <> " = " <> cbHaskellType
      line $ "connectSignal _ = " <> "connect" <> name <> sn
  export si

-- | Signal instances for (GObject-derived) objects.
genObjectSignals :: Name -> Object -> CodeGen ()
genObjectSignals n o = do
  name <- upperName n
  isGO <- apiIsGObject n (APIObject o)
  when isGO $ do
       mapM_ (genInstance n) (objSignals o)
       infos <- fullObjectSignalList n o >>=
                mapM (\(owner, signal) -> do
                      si <- signalInfoName owner signal
                      return $ "'(\"" <> sigName signal
                                 <> "\", " <> si <> ")")
       -- The "notify::[property]" signal is a generic signal used for
       -- connecting to property notifications.
       let allSignals = infos <>
                        ["'(\"notify::[property]\", GObjectNotifySignalInfo)"]
       group . line $ "type instance SignalList " <> name <>
             " = '[ " <> T.intercalate ", " allSignals <> "]"

-- | Signal instances for interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen ()
genInterfaceSignals n iface = do
  name <- upperName n
  mapM_ (genInstance n) (ifSignals iface)
  infos <- fullInterfaceSignalList n iface >>=
           mapM (\(owner, signal) -> do
                   si <- signalInfoName owner signal
                   return $ "'(\"" <> sigName signal
                              <> "\", " <> si <> ")")
  isGO <- apiIsGObject n (APIInterface iface)
  -- The "notify::[property]" signal is a generic signal used for
  -- connecting to property notifications of a GObject.
  let allSignals =
          if isGO
          then infos <> ["'(\"notify::[property]\", GObjectNotifySignalInfo)"]
          else infos
  group . line $ "type instance SignalList " <> name <>
            " = '[ " <> T.intercalate ", " allSignals <> "]"
