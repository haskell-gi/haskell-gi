module GI.OverloadedSignals
    ( genSignalInstances
    , genOverloadedSignalConnectors
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)
import Data.Text (Text)
import qualified Data.Text as T

import Data.List (intercalate)
import qualified Data.Set as S

import GI.API
import GI.Code
import GI.Inheritance (fullObjectSignalList, fullInterfaceSignalList)
import GI.GObject (apiIsGObject)
import GI.Signal (signalHaskellName)
import GI.SymbolNaming (ucFirst, upperName)
import GI.Util (padTo)

-- A list of distinct signal names for all GObjects appearing in the
-- given list of APIs.
findSignalNames :: [(Name, API)] -> CodeGen [String]
findSignalNames apis = (map T.unpack . S.toList) <$> go apis S.empty
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
genOverloadedSignalConnectors :: [(Name, API)] -> String -> CodeGen ()
genOverloadedSignalConnectors allAPIs modulePrefix = do
  line   "-- Generated code."
  blank
  line   "{-# LANGUAGE DataKinds, GADTs, KindSignatures, FlexibleInstances #-}"
  blank
  line $ "module " ++ modulePrefix ++ "Signals where"
  blank
  line   "import GHC.TypeLits"
  line   "import GHC.Exts (Constraint)"
  blank
  line   "class NoConstraint a"
  line   "instance NoConstraint a"
  blank
  line   "data SignalProxy (a :: Symbol) (b :: Symbol) (c :: * -> Constraint) where"
  indent $ do
    signalNames <- findSignalNames allAPIs
    let maxLength = maximum $ map (length . signalHaskellName) signalNames
    forM_ signalNames $ \sn ->
        line $ padTo (maxLength + 1) (ucFirst (signalHaskellName sn)) ++
                 ":: SignalProxy \"" ++ sn ++ "\" \"\" NoConstraint"

-- | Qualified name for the "(sigName, info)" tag for a given signal.
signalInfoName :: Name -> Signal -> CodeGen String
signalInfoName n signal = do
  n' <- upperName n
  return $ n' ++ (ucFirst . signalHaskellName . T.unpack . sigName) signal
             ++ "SignalInfo"

-- | Generate the given signal instance for the given API object.
genInstance :: Name -> Signal -> CodeGen ()
genInstance owner signal = group $ do
  name <- upperName owner
  let sn = (ucFirst . signalHaskellName . T.unpack . sigName) signal
  si <- signalInfoName owner signal
  line $ "data " ++ si
  line $ "instance SignalInfo " ++ si ++ " where"
  indent $ do
      let signalConnectorName = name ++ sn
          cbHaskellType = signalConnectorName ++ "Callback"
      line $ "type HaskellCallbackType " ++ si ++ " = " ++ cbHaskellType
      line $ "connectSignal _ = " ++ "connect" ++ name ++ sn

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
                      return $ "'(\"" ++ (T.unpack . sigName) signal
                                 ++ "\", " ++ si ++ ")")
       -- The "notify::[property]" signal is a generic signal used for
       -- connecting to property notifications.
       let allSignals = infos ++
                        ["'(\"notify::[property]\", GObjectNotifySignalInfo)"]
       group . line $ "type instance SignalList " ++ name ++
             " = '[ " ++ intercalate ", " allSignals ++ "]"

-- | Signal instances for interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen ()
genInterfaceSignals n iface = do
  name <- upperName n
  mapM_ (genInstance n) (ifSignals iface)
  infos <- fullInterfaceSignalList n iface >>=
           mapM (\(owner, signal) -> do
                   si <- signalInfoName owner signal
                   return $ "'(\"" ++ T.unpack (sigName signal)
                              ++ "\", " ++ si ++ ")")
  isGO <- apiIsGObject n (APIInterface iface)
  -- The "notify::[property]" signal is a generic signal used for
  -- connecting to property notifications of a GObject.
  let allSignals =
          if isGO
          then infos ++ ["'(\"notify::[property]\", GObjectNotifySignalInfo)"]
          else infos
  group . line $ "type instance SignalList " ++ name ++
            " = '[ " ++ intercalate ", " allSignals ++ "]"

-- | Generate HasSignal instances for a given API element.
genSignals :: (Name, API) -> CodeGen ()
genSignals (n, APIObject o) = genObjectSignals n o
genSignals (n, APIInterface i) = genInterfaceSignals n i
genSignals _ = return ()

-- | Generate the signal information instances, so the generic overloaded
-- signal connectors are available.
genSignalInstances :: String -> [(Name, API)] -> String -> CodeGen ()
genSignalInstances name apis modulePrefix = do
  let mp = (modulePrefix ++)
      nm = ucFirst name

  code <- recurse $ forM_ apis genSignals

  line   "-- Generated code."
  blank

  -- Providing orphan instances is the whole point of these modules,
  -- tell GHC that this is fine.
  line   "{-# OPTIONS_GHC -fno-warn-orphans #-}"
  blank
  line   "{-# LANGUAGE DataKinds,    FlexibleInstances,"
  line   "             TypeFamilies, MultiParamTypeClasses,"
  line "               TypeOperators #-}"
  blank

  line $ "module " ++ mp nm ++ "Signals where"
  blank

  line   "import Data.GI.Base.Properties (GObjectNotifySignalInfo)"
  line   "import Data.GI.Base.Signals"
  line   "import Data.GI.Base.Overloading"
  blank

  -- Import dependencies, including instances for their overloaded
  -- signals, so they are implicitly reexported and they do not need
  -- to be included explicitly from client code.
  deps <- S.toList <$> getDeps
  forM_ deps $ \i -> when (i /= name) $
    line $ "import qualified " ++ mp (ucFirst i) ++ "Signals as " ++ ucFirst i

  line $ "import " ++ modulePrefix ++ nm
  blank

  tellCode code
