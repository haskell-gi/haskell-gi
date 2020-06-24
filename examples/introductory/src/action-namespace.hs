{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables #-}

{- action-namespace.c example in GTK repository. Please refer to README for more. -}

module Main where

import           Control.Monad (when)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Foreign.Ptr (castPtr)

import           Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

type ActionEntryInfo = (Text, Gio.ActionEntryActivateFieldCallback_WithClosures)

docEntryInfos :: [ActionEntryInfo]
docEntryInfos = [ ("save", actionActivated)
                , ("print", actionActivated)
                , ("share", actionActivated)
                ]

winEntryInfos :: [ActionEntryInfo]
winEntryInfos = [ ( "fullscreen", actionActivated )
                , ( "close", actionActivated )
                ]

menuUIStr :: Text
menuUIStr = "<interface>\
            \  <menu id='doc-menu'>\
            \    <section>\
            \      <item>\
            \        <attribute name='label'>_Save</attribute>\
            \        <attribute name='action'>save</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Print</attribute>\
            \        <attribute name='action'>print</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Share</attribute>\
            \        <attribute name='action'>share</attribute>\
            \      </item>\
            \    </section>\
            \  </menu>\
            \  <menu id='win-menu'>\
            \    <section>\
            \      <item>\
            \        <attribute name='label'>_Fullscreen</attribute>\
            \        <attribute name='action'>fullscreen</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Close</attribute>\
            \        <attribute name='action'>close</attribute>\
            \      </item>\
            \    </section>\
            \  </menu>\
            \</interface>"

actionActivated :: Gio.ActionEntryActivateFieldCallback_WithClosures
actionActivated action _ userData = do
  withTransient Gtk.Window (castPtr userData) $ \parent -> do
    actionName <- get action #name >>= return . fromJust
    dialog <- new Gtk.MessageDialog [ #transientFor := parent
                                    , #destroyWithParent := True
                                    , #messageType := Gtk.MessageTypeInfo
                                    , #buttons := Gtk.ButtonsTypeClose
                                    , #text := "Activated action '" <> actionName <> "'"
                                    ]
    on dialog #response $ \_ -> #destroy dialog >> return ()
    #show dialog

newActionEntry :: ActionEntryInfo -> IO Gio.ActionEntry
newActionEntry (name, callback) = do
  callback <- ( Gio.mk_ActionEntryActivateFieldCallback
              . Gio.wrap_ActionEntryActivateFieldCallback Nothing
              ) callback
  name <- textToCString name
  new Gio.ActionEntry [#name := name, #activate := callback]

addActionEntries :: forall a. GObject a => Gio.ActionMap -> [ActionEntryInfo] -> a -> IO ()
addActionEntries actionMap entryInfos ptr = do
  ptr <- unsafeManagedPtrCastPtr ptr
  (flip $ Gio.actionMapAddActionEntries actionMap) ptr =<< mapM newActionEntry entryInfos

castWOMaybe :: forall o o'. (GObject o, GObject o') => (ManagedPtr o' -> o') -> o -> IO o'
castWOMaybe typeToCast obj = castTo typeToCast obj >>= return . fromJust

getCastedObjectFromBuilder :: forall a. GObject a => Gtk.Builder -> Text -> (ManagedPtr a -> a) -> IO a
getCastedObjectFromBuilder builder name typeToCast = #getObject builder name
                                                   >>= return . fromJust
                                                   >>= castWOMaybe typeToCast

maybeGVariantFromText :: Text -> IO (Maybe GVariant)
maybeGVariantFromText text = Just <$> gvariantFromText text

addSectionWithNameSpace :: Gio.MenuModel -> Text -> Gio.Menu -> IO ()
addSectionWithNameSpace model namespace menu = do
  section <- Gio.menuItemNewSection Nothing model
  #setAttributeValue section "action-namespace" =<< maybeGVariantFromText namespace
  #appendItem menu section

appActivate:: Gtk.Application -> IO ()
appActivate app = do
  windows <- #getWindows app
  when (null windows) $ do
    win <- new Gtk.ApplicationWindow [ #application := app
                                     , #defaultWidth := 200
                                     , #defaultHeight := 300
                                     ]
    docActions <- new Gio.SimpleActionGroup []
    actMap <- Gio.toActionMap docActions
    addActionEntries actMap docEntryInfos win
    addActionEntries actMap winEntryInfos win

    builder <- Gtk.builderNewFromString menuUIStr (-1)
    docMenu <- getCastedObjectFromBuilder builder "doc-menu" Gio.MenuModel
    winMenu <- getCastedObjectFromBuilder builder "win-menu" Gio.MenuModel
    buttonMenu <- new Gio.Menu []

    addSectionWithNameSpace docMenu "doc" buttonMenu
    addSectionWithNameSpace winMenu "win" buttonMenu

    button <- new Gtk.MenuButton [ #label := "Menu"
                                 , #halign := Gtk.AlignCenter
                                 , #valign := Gtk.AlignStart
                                 ]
    #insertActionGroup button "doc" =<< castTo Gio.ActionGroup docActions
    -- If following line is compiled then items belonging to "win" namespace will be enabled 
    -- #insertActionGroup button "win" =<< castTo Gio.ActionGroup docActions

    #setMenuModel button =<< castTo Gio.MenuModel buttonMenu
    #add win button

    #showAll win

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.examples.action-namespace"]
  on app #activate $ appActivate app

  #run app Nothing
  return ()
