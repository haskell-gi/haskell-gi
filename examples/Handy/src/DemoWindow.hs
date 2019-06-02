{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
module DemoWindow
    ( demoWindow
    , appWindow
    ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text)
import Data.Functor (($>))
import Data.Char
import Data.Maybe (fromJust)
import Prelude
import Data.FileEmbed
import Data.GI.Gtk.BuildFn
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Base
import Data.GI.Base.GType
import GI.Gtk (AttrOp(..), set, on, get, set, after, new)

-- necessary because of some callback functions, see below. is this fixable with overrides?
import Foreign.Ptr (nullPtr)

import qualified GI.Gtk as Gtk
import qualified GI.Handy as Hdy
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified Data.Text as T
import qualified Data.Text.IO as T

data DemoWindow = DemoWindow
    { appWindow :: Gtk.ApplicationWindow
    , headerBox :: Hdy.Leaflet
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

keyPressedCB :: MonadIO m => DemoWindow -> Gdk.EventKey -> m Bool
keyPressedCB win key = do
    kv <- get key #keyval
    st <- get key #state
    if (kv == Gdk.KEY_q || kv == Gdk.KEY_Q) &&
       st == [Gdk.ModifierTypeControlMask]
        then Gtk.widgetDestroy (appWindow win) $> True
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

demoWindowUI :: Text
demoWindowUI = $(embedStringFile "res/hdy-demo-window.ui")

buildDemoWindow :: BuildFn DemoWindow
buildDemoWindow = DemoWindow
    <$> getObject Gtk.ApplicationWindow "app_window"
    <*> getObject Hdy.Leaflet "header_box"
    <*> getObject Hdy.Leaflet "content_box"
    <*> getObject Gtk.Button "back"
    <*> getObject Gtk.ToggleButton "search_button"
    <*> getObject Gtk.StackSidebar "sidebar"
    <*> getObject Gtk.Stack "stack"
    <*> getObject Gtk.Box "box_dialer"
    <*> getObject Hdy.Dialer "dialer"
    <*> getObject Gtk.Label "display"
    <*> getObject Hdy.Arrows "arrows"
    <*> getObject Hdy.SearchBar "search_bar"
    <*> getObject Gtk.SearchEntry "search_entry"
    <*> getObject Gtk.ListBox "arrows_listbox"
    <*> getObject Hdy.ComboRow "arrows_direction_row"
    <*> getObject Gtk.ListBox "column_listbox"
    <*> getObject Gtk.ListBox "lists_listbox"
    <*> getObject Hdy.ComboRow "combo_row"
    <*> getObject Hdy.ComboRow "enum_combo_row"
    <*> getObject Hdy.HeaderGroup "header_group"
    <*> getObject Gtk.Adjustment "adj_arrows_count"
    <*> getObject Gtk.Adjustment "adj_arrows_duration"
    <*> getObject Gtk.Button "presentation_dialog_button"
    <*> getObject Gtk.Button "action_dialog_button"

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

presentationDialog :: MonadIO m => DemoWindow -> m ()
presentationDialog DemoWindow{..} = do
    dlg <- new Hdy.Dialog [#title := "HdyDialog", #transientFor := appWindow ]
    dialogLabelAndShow dlg

actionDialog :: MonadIO m => DemoWindow -> m ()
actionDialog DemoWindow {..} = do
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
        voType <- liftIO $ gobjectType @Hdy.ValueObject
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
        enumType <- liftIO $ boxedEnumType Gtk.LicenseUnknown
        Hdy.comboRowSetForEnum
            enumComboRow
            enumType
            (Just (`Hdy.enumValueRowName` nullPtr))

arrowsPageInit :: MonadIO m => DemoWindow -> m ()
arrowsPageInit DemoWindow{..} = do
    ty <- liftIO $ boxedEnumType Hdy.ArrowsDirectionLeft
    Hdy.comboRowSetForEnum arrowsDirectionRow ty 
            (Just (`Hdy.enumValueRowName` nullPtr))

demoWindow :: MonadIO m => Gtk.Application -> m DemoWindow
demoWindow app = do
    b <- Gtk.builderNewFromString demoWindowUI (-1)
    w@DemoWindow {..} <- buildWithBuilder buildDemoWindow b
    -- set application
    set appWindow [#application := app]
    arrowsPageInit w
    listsPageInit w
    listBoxSeparate columnListbox
    listBoxSeparate arrowsListbox
    set contentBox [ #visibleChildName := "content" ]
    -- signals
    on appWindow #keyPressEvent (keyPressedCB w)
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

    on presentationDialogButton #clicked (presentationDialog w)
    on actionDialogButton #clicked (actionDialog w)

    pure w
