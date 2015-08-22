module GI.OverloadedSignals
    ( genSignalInstances
    , genOverloadedSignalConnectors
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)
import Control.Monad.Writer (tell)

import Data.List (intercalate)
import qualified Data.Set as S

import GI.API
import GI.Code
import GI.GObject (apiIsGObject)
import GI.Signal (signalHaskellName)
import GI.SymbolNaming (ucFirst, upperName)
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

-- | Generate the given signal instance for the given API object.
genInstance :: Name -> Signal -> CodeGen String
genInstance owner signal = group $ do
  name <- upperName owner
  let sn = (ucFirst . signalHaskellName . sigName) signal
      si = name ++ sn ++ "SignalInfo"
  line $ "data " ++ si
  line $ "instance SignalInfo " ++ si ++ " where"
  indent $ do
      let signalConnectorName = name ++ sn
          cbHaskellType = signalConnectorName ++ "Callback"
      line $ "type HaskellCallbackType " ++ si ++ " = " ++ cbHaskellType
      line $ "connectSignal _ = " ++ "connect" ++ name ++ sn
  return $ "'(\"" ++ sigName signal ++ "\", " ++ si ++ ")"

-- | HasSignal instances for GObject objects.
genObjectSignals :: Name -> Object -> CodeGen ()
genObjectSignals n o = do
  name <- upperName n
  isGO <- apiIsGObject n (APIObject o)
  -- We only generate code for GObject-derived APIObjects.
  when isGO $ do
       infos <- mapM (genInstance n) (objSignals o)
       group . line $ "type instance SignalList " ++ name ++
             " = '[ " ++ intercalate ", " infos ++ "]"

-- | HasSignal instances for GObject interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen ()
genInterfaceSignals n iface = do
  name <- upperName n
  isGO <- apiIsGObject n (APIInterface iface)
  if isGO
  then do
    infos <- mapM (genInstance n) (ifSignals iface)
    group . line $ "type instance SignalList " ++ name ++
              " = '[ " ++ intercalate ", " infos ++ "]"
  else group . line $ "type instance SignalList " ++ name ++ " = '[]"

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
  blank
  line   "{-# LANGUAGE DataKinds,    FlexibleInstances,"
  line   "             TypeFamilies, MultiParamTypeClasses,"
  line "               TypeOperators #-}"
  blank

  line $ "module " ++ mp nm ++ "Signals where"
  blank

  line   "import GI.Utils.Signals"
  line   "import GI.Utils.Overloading"
  blank

  deps <- S.toList <$> getDeps
  forM_ deps $ \i -> when (i /= name) $
    line $ "import qualified " ++ mp (ucFirst i) ++ " as " ++ ucFirst i
  blank

  line $ "import " ++ modulePrefix ++ nm
  blank

  tell code
