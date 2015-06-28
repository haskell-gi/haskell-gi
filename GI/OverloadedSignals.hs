module GI.OverloadedSignals
    ( genSignalInstances
    , genOverloadedSignalConnectors
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)
import Control.Monad.Writer (tell)

import qualified Data.Set as S

import GI.API
import GI.Code
import GI.GObject (apiIsGObject)
import GI.Inheritable (fullInterfaceSignalList, fullObjectSignalList)
import GI.Signal (signalHaskellName)
import GI.SymbolNaming (ucFirst, upperName, qualify)
import GI.Util (padTo)

-- A list of distinct signal names for all GObjects appearing in the
-- given list of APIs.
findSignalNames :: [(Name, API)] -> CodeGen [String]
findSignalNames apis = S.toList <$> go apis S.empty
    where
      go :: [(Name, API)] -> S.Set String -> CodeGen (S.Set String)
      go [] set = return set
      go ((name, api):apis) set = do
        isGO <- apiIsGObject name api
        if isGO
        then case api of
               APIInterface iface ->
                   go apis $ insertSignals (ifSignals iface) set
               APIObject object ->
                   go apis $ insertSignals (objSignals object) set
               _ -> error $ "GObject not an Interface or Object!? " ++ show name
        else go apis set

      insertSignals :: [Signal] -> S.Set String -> S.Set String
      insertSignals props set = foldr (S.insert . sigName) set props

-- | Generate the overloaded signal connectors: "Clicked", "ActivateLink", ...
genOverloadedSignalConnectors :: [(Name, API)] -> String -> CodeGen ()
genOverloadedSignalConnectors allAPIs modulePrefix = do
  line   "-- Generated code."
  blank
  line   "{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}"
  blank
  line $ "module " ++ modulePrefix ++ "Signals where"
  blank
  line   "import GHC.TypeLits"
  blank
  line   "data SignalProxy (a :: Symbol) (b :: Symbol) where"
  indent $ do
    signalNames <- findSignalNames allAPIs
    let maxLength = maximum $ map (length . signalHaskellName) signalNames
    forM_ signalNames $ \sn ->
        line $ padTo (maxLength + 1) (ucFirst (signalHaskellName sn)) ++
                 ":: SignalProxy \"" ++ sn ++ "\" \"" ++ sn ++ "\""

-- | Generate the given signal instance for the given API object.
genInstance :: Name -> (Name, Signal) -> CodeGen ()
genInstance n (owner, signal) = group $ do
    line $ "instance HasSignal \"" ++ sigName signal ++ "\" "
             ++ name n ++ " where"
    indent $ do
      -- This should follow the conventions in genSignal in Signal.hs
      on <- upperName owner
      let sn = signalHaskellName (sigName signal)
          signalConnectorName = on ++ ucFirst sn
          cbHaskellType = signalConnectorName ++ "Callback"
      line $ "type HaskellCallbackType \"" ++ sigName signal ++ "\" "
               ++ name n ++ " = " ++ cbHaskellType
      line $ "type ConnectConstraint \"" ++ sigName signal ++ "\" "
              ++ name n ++ " = (~) \"" ++ sigName signal ++ "\""
      ons <- qualify (namespace owner)
      line $ "connectSignal _ = " ++ ons ++ "connect"
               ++ ucFirst (name owner) ++ ucFirst sn

-- | HasSignal instances for GObject objects.
genObjectSignals :: Name -> Object -> CodeGen ()
genObjectSignals n o = do
  isGO <- apiIsGObject n (APIObject o)
  when isGO $
       fullObjectSignalList n o >>= mapM_ (genInstance n)

-- | HasSignal instances for GObject interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen ()
genInterfaceSignals n o = do
  isGO <- apiIsGObject n (APIInterface o)
  when isGO $
       fullInterfaceSignalList n o >>= mapM_ (genInstance n)

-- | Generate HasSignal instances for a given API element.
genSignals :: (Name, API) -> CodeGen ()
genSignals (n, APIObject o) = genObjectSignals n o
genSignals (n, APIInterface i) = genInterfaceSignals n i
genSignals _ = return ()

-- | Generate the HasSignal instances, so the generic overloaded
-- signal connectors are available.
genSignalInstances :: String -> [(Name, API)] -> String -> CodeGen ()
genSignalInstances name apis modulePrefix = do
  let mp = (modulePrefix ++)
      nm = ucFirst name

  code <- recurse' $ forM_ apis genSignals

  line   "-- Generated code."
  blank

  -- Providing orphan instances is the whole point of these modules,
  -- tell GHC that this is fine.
  line   "{-# OPTIONS_GHC -fno-warn-orphans #-}"
  line   "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
  blank
  line   "{-# LANGUAGE DataKinds,    FlexibleInstances,"
  line   "             TypeFamilies, MultiParamTypeClasses #-}"
  blank

  line $ "module " ++ mp nm ++ "Signals where"
  blank

  line   "import GI.Utils.Signals"
  blank

  deps <- S.toList <$> getDeps
  forM_ deps $ \i -> when (i /= name) $
    line $ "import qualified " ++ mp (ucFirst i) ++ " as " ++ ucFirst i
  blank

  line $ "import " ++ modulePrefix ++ nm
  blank

  tell code
