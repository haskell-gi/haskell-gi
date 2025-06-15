{-# Language ImplicitParams #-}
{-# Language UndecidableInstances #-}
{-# Language TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language RankNTypes #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

-- For ghc <= 9.2, this warning is not part of -Wall for ghc >= 9.4
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Declarative.Gtk.ModelViewHandler
  ( mvh
  , fullMVH
  , View
  , DynLens(..)
  , Handler
  , modify
  , modify'
  , modifyWith
  , (<~)
  , (!<~)
  , gather
  , watch
  , withCurrent
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (finally)
import Control.Monad (void, forM_, when)
import qualified Data.List as L
import GHC.Records (HasField(..))
import qualified GHC.TypeLits as TL
import qualified Data.IORef as IOR
import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Optics.Internal.Generic as OG
import qualified Optics.Core as O

import qualified GI.GLib as GLib

import Data.GI.Base.DynVal (DVKey(..), ModelProxy(..), DynVal(..),
                            dvRead, dvKeys, modelProxyRegisterHandler,
                            modelProxyCurrentValue)


type View model msg result = (?_haskell_gi_modelProxy :: ModelProxy model)
                      => DynVal model model -> (msg -> IO ()) -> IO result

type Handler msg model = msg -> DynLens model model -> IO ()

-- | A path to a field of type @a@ inside @model@, keeping track of
-- the path being changed.
data DynLens model a = DynLens [T.Text] (O.Lens' model a) (HandlerProxy model)

instance (OG.GFieldImpl fieldName s s a a, TL.KnownSymbol fieldName) =>
  HasField (fieldName :: TL.Symbol) (DynLens model s) (DynLens model a) where
  getField (DynLens parentPath parentLens handlerProxy) =
    let lens = OG.gfieldImpl @fieldName
        fn = T.pack . TL.symbolVal $ (Proxy :: Proxy fieldName)
    in DynLens (parentPath <> [fn]) (parentLens O.% lens) handlerProxy

-- | An identifier for a modification notification handler.
type HandlerID = Integer

-- | A collection of handlers dealing with changes to the model,
-- together with an `IOR.IORef` to a model being modified.
data HandlerProxy model = HandlerProxy {
  -- | Current value of the model.
  hpCurrent :: !(IOR.IORef model),
  -- | A monotonically increasing counter for handlers.
  hpCounter :: !(IOR.IORef HandlerID),
  -- | List of handlers, indexed by an ID.
  hpHandlers :: !(IOR.IORef (M.Map HandlerID (model -> IO ()))),
  -- | Map from modification paths to sets of handlers listening to
  -- the given notification.
  hpActions :: !(IOR.IORef (M.Map [T.Text] (S.Set HandlerID))),
  -- | List of notifications being held in the current thread.
  hpHeld :: !(IOR.IORef (M.Map ThreadId (S.Set HandlerID)))
}

-- | Execute the given action for the current value of the `DynVal`,
-- and invoke it again any time the `DynVal` changes.
watch :: (?_haskell_gi_modelProxy :: ModelProxy model) =>
         DynVal model a -> (a -> IO ()) -> IO ()
watch dynval action = do
  currentModel <- modelProxyCurrentValue ?_haskell_gi_modelProxy
  action (dvRead dynval currentModel)
  modelProxyRegisterHandler ?_haskell_gi_modelProxy (dvKeys dynval)
                  (action . dvRead dynval)

-- | Construct a declarative object, consisting of a model, a view,
-- and a message handler. This returns whatever the view returns,
-- together with a function for emitting messages and a `DynLens` to the
-- model, which you can use to obtain/modify the underlying model. If
-- you don't need to modify the model from outside the handler you can
-- use `mvh` instead.
fullMVH :: forall model msg result.
              model -> View model msg result -> Handler msg model ->
              IO (result, msg -> IO (), DynLens model model)
fullMVH initialModel view handler = do
  modelRef <- IOR.newIORef initialModel
  counterRef <- IOR.newIORef 0
  handlersRef <- IOR.newIORef M.empty
  actionsRef <- IOR.newIORef M.empty
  held <- IOR.newIORef M.empty

  let hp = HandlerProxy modelRef counterRef handlersRef actionsRef held
  let ?_haskell_gi_modelProxy = ModelProxy (IOR.readIORef modelRef)
                                           (register hp)
                                           (update hp)
  let root = DynLens [] (O.lens id (flip const)) hp
  let emit msg = void $ GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
        gather root $ handler msg root
        return False

  v <- view (DynVal (DVKeyDirect []) id) emit
  return (v, emit, root)

  where
    update :: HandlerProxy model -> [T.Text] -> (model -> Maybe model) -> IO ()
    update proxy path doUpdate = do
      (model', modified) <- IOR.atomicModifyIORef' (hpCurrent proxy) $ \model ->
        case doUpdate model of
          Just newModel -> (newModel, (newModel, True))
          Nothing -> (model, (model, False))
      when modified $
        broadcastModification model' proxy path

    register :: HandlerProxy model -> DVKey -> (model -> IO ()) -> IO ()
    register hp keys action = do
      let keySet = case keys of
                     DVKeyDirect k -> S.singleton k
                     DVKeyDerived d -> d
      -- We only need to actually register a handler if it's listening
      -- to some key.
      when (not $ S.null keySet) $ do
        actionID <- IOR.atomicModifyIORef' (hpCounter hp) $ \counter ->
          (counter+1, counter)
        -- These maps are indexed by the actionID, so it's fine to
        -- update them in separate atomic actions.
        IOR.atomicModifyIORef' (hpHandlers hp) $ \handlers ->
          (M.insert actionID action handlers, ())
        IOR.atomicModifyIORef' (hpActions hp) $ \actions ->
          let newActions = S.foldl' (insert actionID) actions keySet
          in (newActions, ())
        putStrLn $ "Handler " <> show actionID <> " : " <> show (S.toList keySet)

    insert :: HandlerID ->
              M.Map [T.Text] (S.Set HandlerID) -> [T.Text] ->
              M.Map [T.Text] (S.Set HandlerID)
    insert action actions key = M.alter (alter action) key actions

    alter :: HandlerID -> Maybe (S.Set HandlerID) ->
             Maybe (S.Set HandlerID)
    alter actionID Nothing = Just (S.singleton actionID)
    alter actionID (Just actions) = Just (S.insert actionID actions)

-- | Construct a declarative object, consisting of a model, a view,
-- and a message handler. This returns whatever the view returns.
mvh :: model -> View model msg result -> Handler msg model -> IO result
mvh m v h = (\(r, _, _) -> r) <$> fullMVH m v h

-- | Run the given action using the current value of the model.
withCurrent :: DynLens model field -> (field -> IO a) -> IO a
withCurrent (DynLens _ lens hp) action = do
  currentModel <- IOR.readIORef (hpCurrent hp)
  action (O.view lens currentModel)

-- | Run the given action, collecting all notifications so that they
-- are all emitted together at the end.
gather :: DynLens model a -> IO b -> IO b
gather dynlens@(DynLens _path _lens proxy) action = do
  tid <- myThreadId
  IOR.atomicModifyIORef' (hpHeld proxy) $ \heldMap ->
    if tid `M.member` heldMap
    then (heldMap, ()) -- Already gathering notifications, nothing to do
    else (M.insert tid S.empty heldMap, ())
  action `finally` release dynlens

-- | If the given `DynLens` is being held, broadcast any pending
-- notifications, and empty the notification queue. If the `DynLens`
-- is not held we do not do anything.
flush :: DynLens model a -> IO ()
flush (DynLens _path _lens proxy) = do
  tid <- myThreadId
  actionSet <- IOR.atomicModifyIORef' (hpHeld proxy) $ \heldMap ->
    case M.lookup tid heldMap of
      Just actions -> (M.insert tid S.empty heldMap, actions)
      Nothing -> (heldMap, S.empty) -- Not gathering notifications, so
                                    -- nothing to do

  when (not $ S.null actionSet) $ do
    putStrLn $ "Flushing notifications: " <> show (S.toList actionSet)
    model' <- IOR.readIORef (hpCurrent proxy)

    -- Make sure that the action is run on the GTK thread
    void $ GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
      handlers <- IOR.readIORef (hpHandlers proxy)
      forM_ (S.toList actionSet) $ \actionID -> do
        case M.lookup actionID handlers of
          Just action -> action model'
          Nothing -> putStrLn $ "Failed to find action key " <> show actionID
      return False

-- | Release a held `DynLens`, flushing any pending notifications. If
-- the `DynLens` is not held this does nothing.
release :: DynLens model a -> IO ()
release dynlens@(DynLens _path _lens proxy) = do
  flush dynlens
  tid <- myThreadId
  IOR.atomicModifyIORef' (hpHeld proxy) $ \heldMap ->
    case M.lookup tid heldMap of
      Just _ -> (M.delete tid heldMap, ())
      Nothing -> (heldMap, ()) -- Not gathering notifications, so
                               -- nothing to do

-- | Act with the given transform on the given lens, notifying any
-- listeners. The notification will only be triggered if the old value
-- is different from the new value, according to the comparison
-- function passed as the first argument, which should return @True@
-- if the values are to be treated as equal, and @False@ otherwise.
modifyWith :: (a -> a -> Bool) -> DynLens model a -> (a -> a) -> IO ()
modifyWith eq (DynLens path lens proxy) transform = do
  (model', oldVal, newVal) <- IOR.atomicModifyIORef' (hpCurrent proxy) $ \model ->
       let model' = O.over lens transform model
       in (model', (model', O.view lens model, O.view lens model'))
  when (not $ eq oldVal newVal) $ do
    broadcastModification model' proxy path

broadcastModification :: model -> HandlerProxy model -> [T.Text] -> IO ()
broadcastModification model' proxy path = do
  actionMap <- IOR.readIORef (hpActions proxy)

  let actionShouldFire tag = path `L.isPrefixOf` tag ||
                             tag `L.isPrefixOf` path
      mergeActions tag actions accum =
        if actionShouldFire tag
        then S.union accum actions
        else accum
      actionSet = M.foldrWithKey' mergeActions S.empty actionMap

  putStrLn $ "Notifying " <> show (S.toList actionSet)
    <> " due to a change in " <> show path

  when (not $ S.null actionSet) $ do
    tid <- myThreadId
    -- Try adding this set of actions to the list of held actions
    -- for the current thread, in case notifications are being
    -- gathered for dispatching at once at the end.
    threadHeld <- IOR.atomicModifyIORef' (hpHeld proxy) $ \heldMap ->
      case M.lookup tid heldMap of
        Just prevActions ->
          (M.insert tid (S.union prevActions actionSet) heldMap, True)
        Nothing -> (heldMap, False)

    when threadHeld $ putStrLn "\t(Notifications held)"

    -- Notifications were not being gathered in the current thread,
    -- so dispatch right away.
    when (not threadHeld) $ do
      -- Make sure that the action is run on the GTK thread
      void $ GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
        handlers <- IOR.readIORef (hpHandlers proxy)
        forM_ (S.toList actionSet) $ \actionID -> do
          case M.lookup actionID handlers of
            Just action -> action model'
            Nothing -> putStrLn $ "Failed to find action key " <> show actionID
        return False


-- | Act with the given transform on the given lens, notifying any
-- listeners. The notification will only be triggered if the new value
-- is different from the old value.
modify :: Eq a => DynLens model a -> (a -> a) -> IO ()
modify = modifyWith (==)

-- | Act with the given transform on the given lens, notifying any
-- listeners. The notification will always be triggered, including in
-- the case where the old and new values coincide.
modify' :: DynLens model a -> (a -> a) -> IO ()
modify' = modifyWith (const . const True)

-- | Set the given lens to the given value, notifying any
-- listeners. The notification will only be triggered if the new value
-- is different from the old value.
infixr 0 <~
(<~) :: Eq a => DynLens model a -> a -> IO ()
lens <~ value = modify lens (const value)

-- | Set the given lens to the given value, notifying any
-- listeners. The notification will always be triggered, including in
-- the case where the old and new values coincide.
infixr 0 !<~
(!<~) :: Eq a => DynLens model a -> a -> IO ()
lens !<~ value = modify' lens (const value)
