{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
-- Type subclassing in haskell-gi involves some type-level
-- programming, enable the necessary features here.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module DemoWindow
    ( demoWindow
    ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text)
import Data.Functor (($>))
import Data.Char
import Data.Maybe (fromJust)
import Prelude
import Data.Coerce (coerce)
import Data.FileEmbed
import Data.GI.Gtk.BuildFn
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Base
import Data.GI.Base.GType
import Data.GI.Base.GObject (registerGType, DerivedGObject(..),
                             GObjectClass(..))
import qualified Data.GI.Base.Overloading as O
import GI.Gtk (AttrOp(..), set, on, get, set, after, new)

-- necessary because of some callback functions, see below. is this fixable with overrides?
import Foreign.Ptr (nullPtr)

import qualified GI.Gtk as Gtk
import qualified GI.Handy as Hdy
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified Data.Text as T
import qualified Data.Text.IO as T

import GHC.OverloadedLabels as OL

data DemoWindow = DemoWindow
    { headerBox :: Hdy.Leaflet
    , contentBox :: Hdy.Leaflet
    , back :: Gtk.Button
    , searchButton :: Gtk.ToggleButton
    , sidebar :: Gtk.StackSidebar
    , stack :: Gtk.Stack
    , boxDialer :: Gtk.Box
    , dialer :: Hdy.Dialer
    , display :: Gtk.Label
    , arrows :: Hdy.Arrows
    , searchBar :: Hdy.SearchBar
    , searchEntry :: Gtk.SearchEntry
    , arrowsListbox :: Gtk.ListBox
    , arrowsDirectionRow :: Hdy.ComboRow
    , columnListbox :: Gtk.ListBox
    , listsListbox :: Gtk.ListBox
    , comboRow :: Hdy.ComboRow
    , enumComboRow :: Hdy.ComboRow
    , headerGroup :: Hdy.HeaderGroup
    , adjArrowsCount :: Gtk.Adjustment
    , adjArrowsDuration :: Gtk.Adjustment
    , presentationDialogButton :: Gtk.Button
    , actionDialogButton :: Gtk.Button
    }

-- | A pointer to the type for the demo window.
newtype HdyDemoWindow = HdyDemoWindow (ManagedPtr HdyDemoWindow)

-- | 'HdyDemoWindow' is a type of 'GObject', which we register below.
instance TypedObject HdyDemoWindow where
  glibType = registerGType HdyDemoWindow

instance GObject HdyDemoWindow

-- | 'GObject' registration data for 'HdyDemoWindow'.
instance DerivedGObject HdyDemoWindow where
  -- | The parent type
  type GObjectParentType  HdyDemoWindow = Gtk.ApplicationWindow
  -- | The type for the private data
  type GObjectPrivateData HdyDemoWindow = DemoWindow

  -- | Object type name to register in the GObject type system.
  objectTypeName      = "HdyDemoWindow"

  -- | Class init function (called when registering the type)
  objectClassInit     = hdyDemoWindowClassInit

  -- | Instance init function (called for each object of this type)
  objectInstanceInit  = hdyDemoWindowInstanceInit

-- | Tell the Haskell type system that we descend from
-- 'Gtk.ApplicationWindow'.
instance O.HasParentTypes HdyDemoWindow
type instance O.ParentTypes HdyDemoWindow = Gtk.ApplicationWindow ': O.ParentTypes Gtk.ApplicationWindow

-- | We inherit all overloaded properties, signals and methods from
-- 'Gtk.ApplicationWindow'.

-- Overloaded properties:
instance O.HasAttributeList HdyDemoWindow
type instance O.AttributeList HdyDemoWindow = O.AttributeList Gtk.ApplicationWindow

-- Overloaded signals:
type instance O.SignalList HdyDemoWindow = O.SignalList Gtk.ApplicationWindow

-- Overloaded methods:
instance (info ~ Gtk.ResolveApplicationWindowMethod t HdyDemoWindow,
          O.MethodInfo info HdyDemoWindow p)
         => OL.IsLabel t (HdyDemoWindow -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

-- | Things to do when registering the 'HdyDemoWindow' type.
hdyDemoWindowClassInit :: GObjectClass -> IO ()
hdyDemoWindowClassInit klass =
  withTransient Gtk.WidgetClass (coerce klass) $ \widgetClass -> do
    #setTemplateFromResource widgetClass "/haskell-gi/examples/handy-demo/hdy-demo-window.ui"
    let bindChild childName =
          #bindTemplateChildFull widgetClass childName False 0
    mapM_ bindChild ["header_box", "content_box", "back", "search_button",
                     "sidebar", "stack", "box_dialer", "dialer",
                     "display", "arrows", "search_bar", "search_entry",
                     "arrows_listbox", "arrows_direction_row", "column_listbox",
                     "lists_listbox", "combo_row", "enum_combo_row",
                     "header_group", "adj_arrows_count", "adj_arrows_duration",
                     "presentation_dialog_button", "action_dialog_button"]

-- | Get a child of the given type from the given widget.
getChild :: forall o. GObject o
         => HdyDemoWindow -> (ManagedPtr o -> o) -> T.Text -> IO o
getChild widget constructor name = do
  gtype <- glibType @HdyDemoWindow
  bareGObject <- #getTemplateChild widget gtype name
  unsafeCastTo constructor bareGObject

-- | Initialization for each instance
hdyDemoWindowInstanceInit :: GObjectClass -> HdyDemoWindow
                          -> IO DemoWindow
hdyDemoWindowInstanceInit klass self = do
  #initTemplate self

  w@DemoWindow {..} <- DemoWindow
         <$> getChild self Hdy.Leaflet "header_box"
         <*> getChild self Hdy.Leaflet "content_box"
         <*> getChild self Gtk.Button "back"
         <*> getChild self Gtk.ToggleButton "search_button"
         <*> getChild self Gtk.StackSidebar "sidebar"
         <*> getChild self Gtk.Stack "stack"
         <*> getChild self Gtk.Box "box_dialer"
         <*> getChild self Hdy.Dialer "dialer"
         <*> getChild self Gtk.Label "display"
         <*> getChild self Hdy.Arrows "arrows"
         <*> getChild self Hdy.SearchBar "search_bar"
         <*> getChild self Gtk.SearchEntry "search_entry"
         <*> getChild self Gtk.ListBox "arrows_listbox"
         <*> getChild self Hdy.ComboRow "arrows_direction_row"
         <*> getChild self Gtk.ListBox "column_listbox"
         <*> getChild self Gtk.ListBox "lists_listbox"
         <*> getChild self Hdy.ComboRow "combo_row"
         <*> getChild self Hdy.ComboRow "enum_combo_row"
         <*> getChild self Hdy.HeaderGroup "header_group"
         <*> getChild self Gtk.Adjustment "adj_arrows_count"
         <*> getChild self Gtk.Adjustment "adj_arrows_duration"
         <*> getChild self Gtk.Button "presentation_dialog_button"
         <*> getChild self Gtk.Button "action_dialog_button"

  arrowsPageInit w
  listsPageInit w
  listBoxSeparate columnListbox
  listBoxSeparate arrowsListbox
  set contentBox [ #visibleChildName := "content" ]
  -- signals
  on self #keyPressEvent (keyPressedCB self)
  on headerBox (Gdk.PropertyNotify #visibleChild) (\_ -> update w)
  on headerBox (Gdk.PropertyNotify #fold) (\_ -> update w)
  on stack
      (Gdk.PropertyNotify #visibleChild)
      (\_ ->
           set contentBox [#visibleChildName := "content"] *>
           updateHeaderBar w)
  on back #clicked $ set contentBox [#visibleChildName := "sidebar"]

  dialerSignals w

  on arrowsDirectionRow (Gdk.PropertyNotify #selectedIndex) $ \k -> do
      set arrows
          [ #direction :=> toEnum . fromIntegral <$>
            get arrowsDirectionRow #selectedIndex
          ]
      Hdy.arrowsAnimate arrows
  on adjArrowsCount #valueChanged $ do
      set arrows [ #count :=> truncate <$> get adjArrowsCount #value ]
      Hdy.arrowsAnimate arrows
  on adjArrowsDuration #valueChanged $ do
      set arrows [ #duration :=> truncate <$> get adjArrowsDuration #value ]
      Hdy.arrowsAnimate arrows

  on presentationDialogButton #clicked (presentationDialog self)
  on actionDialogButton #clicked (actionDialog self)

  pure w

keyPressedCB :: MonadIO m => HdyDemoWindow -> Gdk.EventKey -> m Bool
keyPressedCB win key = do
    kv <- get key #keyval
    st <- get key #state
    if (kv == Gdk.KEY_q || kv == Gdk.KEY_Q) &&
       st == [Gdk.ModifierTypeControlMask]
        then Gtk.widgetDestroy win $> True
        else pure False

update :: MonadIO m => DemoWindow -> m ()
update DemoWindow {..} = do
    headerChild <-
        do w <- get headerBox #visibleChild
           liftIO $ Gdk.castTo Gtk.HeaderBar w
    fold <- get headerBox #fold
    -- don't use the overloaded function here because it is not nullable!
    Hdy.headerGroupSetFocus headerGroup $
        case fold of
            Hdy.FoldFolded -> headerChild
            Hdy.FoldUnfolded -> Nothing

updateHeaderBar :: MonadIO m => DemoWindow -> m ()
updateHeaderBar DemoWindow {..} = do
    visibleChildName <- get stack #visibleChildName
    set searchButton [#visible := visibleChildName == Just "search-bar"]

dialerSignals :: MonadIO m => DemoWindow -> m ()
dialerSignals DemoWindow{..} = do
    after dialer #submitted $ \number -> T.putStrLn ("Submit " <> number)
    after dialer #deleted $ T.putStrLn "Delete btn"
    after dialer #symbolClicked $ \o ->
        T.putStrLn (T.snoc "clicked: " (chr (fromIntegral o)))
    on dialer
        (Gdk.PropertyNotify #number)
        (\_ -> get dialer #number >>= \num -> set display [#label := num])
    
    pure ()

dialogLabelAndShow :: (Gtk.IsWidget w, Gtk.IsDialog w, MonadIO m) => w -> m ()
dialogLabelAndShow dlg = do
    lbl <- new Gtk.Label
        [ #label := "Hello, World!"
        , #vexpand := True
        , #valign := Gtk.AlignCenter
        , #halign := Gtk.AlignCenter
        ]
    Gtk.dialogGetContentArea dlg >>= flip Gtk.containerAdd lbl
    Gtk.widgetShow lbl
    Gtk.widgetShow dlg

presentationDialog :: MonadIO m => HdyDemoWindow -> m ()
presentationDialog appWindow = do
    dlg <- new Hdy.Dialog [#title := "HdyDialog", #transientFor := appWindow ]
    dialogLabelAndShow dlg

actionDialog :: MonadIO m => HdyDemoWindow -> m ()
actionDialog appWindow = do
    dlg <-
        new
            Hdy.Dialog
            [ #title := "HdyDialog"
            , #transientFor := appWindow
            , #useHeaderBar := 1
            ]
    Gtk.dialogAddButton
        dlg
        "Done"
        (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
    Gtk.dialogAddButton
        dlg
        "Cancel"
        (fromIntegral . fromEnum $ Gtk.ResponseTypeCancel)
    Gtk.dialogSetDefaultResponse
        dlg
        (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
    on dlg #response $ const (Gtk.widgetDestroy dlg)
    dialogLabelAndShow dlg

listBoxSeparate :: MonadIO m => Gtk.ListBox -> m ()
listBoxSeparate list = 
    Gtk.listBoxSetHeaderFunc
        list
        (Just (\a b -> Hdy.listBoxSeparatorHeader a b nullPtr))

listsPageInit :: MonadIO m => DemoWindow -> m ()
listsPageInit DemoWindow {..} = do
    listBoxSeparate listsListbox
    setupComboRow
    setupEnumRow
  where
    setupComboRow = do
        obj0 <- new Hdy.ValueObject [#value :=> toGValue (Just @Text "Foo")]
        obj1 <- new Hdy.ValueObject [#value :=> toGValue (Just @Text "Bar")]
        obj2 <- new Hdy.ValueObject [#value :=> toGValue (Just @Text "Baz")]
        voType <- liftIO $ glibType @Hdy.ValueObject
        listStore <- new Gio.ListStore [#itemType := voType]
        Gio.listStoreInsert listStore 0 obj0
        Gio.listStoreInsert listStore 1 obj1
        Gio.listStoreInsert listStore 2 obj2
        Hdy.comboRowBindNameModel
            comboRow
            (Just listStore)
            (Just
                 (Gdk.castTo Hdy.ValueObject >=>
                  Hdy.valueObjectDupString . fromJust))
    setupEnumRow = do
        enumType <- liftIO $ glibType @Gtk.License
        Hdy.comboRowSetForEnum
            enumComboRow
            enumType
            (Just (`Hdy.enumValueRowName` nullPtr))

arrowsPageInit :: MonadIO m => DemoWindow -> m ()
arrowsPageInit DemoWindow{..} = do
    ty <- liftIO $ glibType @Hdy.ArrowsDirection
    Hdy.comboRowSetForEnum arrowsDirectionRow ty 
            (Just (`Hdy.enumValueRowName` nullPtr))

demoWindow :: MonadIO m => Gtk.Application -> m HdyDemoWindow
demoWindow app = new HdyDemoWindow [#application := app]

