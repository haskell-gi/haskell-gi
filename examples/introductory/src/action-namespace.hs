{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables #-}

module Main where

import           Control.Monad (when)
-- import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
-- import qualified Data.Text as Text 
import           Data.Text (Text)
import           Foreign.C.String (withCString)
import           Foreign.Ptr (castPtr, Ptr)

import           Data.GI.Base
-- import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio 
import qualified GI.Gtk as Gtk

type ActionEntryInfo = (Text, Gio.ActionEntryActivateFieldCallback_WithClosures) --C_ActionEntryActivateFieldCallback)

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

actionActivated :: Gio.ActionEntryActivateFieldCallback_WithClosures--C_ActionEntryActivateFieldCallback
actionActivated action param userData = do
  withTransient Gtk.Window (castPtr userData) $ \parent -> do
    -- withTransient Gio.SimpleAction action $ \action -> do
      actionName <- get action #name >>= return . fromJust
      dialog <- new Gtk.MessageDialog [ #parent := parent
                                      , #destroyWithParent := True
                                      , #messageType := Gtk.MessageTypeInfo
                                      , #buttons := Gtk.ButtonsTypeClose
                                      , #text := "Activated action '" <> actionName <> "'"
                                      ]
      on dialog #response $ \_ -> #destroy dialog >> return ()
      #show dialog

newActionEntry :: ActionEntryInfo -> IO Gio.ActionEntry
newActionEntry (name, callback) = do
  -- callback <- Gio.mk_ActionEntryActivateFieldCallback callback
  callback <- ( Gio.mk_ActionEntryActivateFieldCallback
              . Gio.wrap_ActionEntryActivateFieldCallback Nothing
              ) callback
  name <- textToCString name 
  new Gio.ActionEntry [#name := name, #activate := callback]

addActionEntries :: Gio.ActionMap -> [ActionEntryInfo] -> Ptr () -> IO ()
addActionEntries actionMap entryInfos ptr = do
  entries <- mapM newActionEntry entryInfos
  #addActionEntries actionMap entries ptr
  
castWOMaybe :: forall o o'. (GObject o, GObject o') => (ManagedPtr o' -> o') -> o -> IO o'
castWOMaybe typeToCast obj = castTo typeToCast obj >>= return . fromJust

getCastedObjectFromBuilder :: forall a. GObject a => Gtk.Builder -> Text -> (ManagedPtr a -> a) -> IO a
getCastedObjectFromBuilder builder name typeToCast = #getObject builder name 
                                                   >>= return . fromJust
                                                   >>= castWOMaybe typeToCast 

appActivate:: Gtk.Application -> IO ()
appActivate app = do
  windows <- #getWindows app
  when (null windows) $ do
    win <- new Gtk.ApplicationWindow [ #application := app
                                     , #defaultWidth := 200
                                     , #defaultHeight := 300
                                     ]
    docActions <- new Gio.SimpleActionGroup []
    actMap <- Gio.toActionMap docActions --fromJust <$> castTo Gio.ActionMap docActions
    winPtr <- unsafeManagedPtrCastPtr win
    addActionEntries actMap docEntryInfos winPtr
    addActionEntries actMap winEntryInfos winPtr

    builder <- Gtk.builderNewFromString menuUIStr (-1) --(Text.length menuUIStr)
    docMenu <- getCastedObjectFromBuilder builder "doc-menu" Gio.MenuModel
    winMenu <- getCastedObjectFromBuilder builder "win-menu" Gio.MenuModel
    buttonMenu <- new Gio.Menu []

    section <- Gio.menuItemNewSection Nothing docMenu
    Just <$> (gvariantFromText "doc") >>= #setAttributeValue section "action-namespace"
    #appendItem buttonMenu section
    section <- Gio.menuItemNewSection Nothing winMenu
    Just <$> (gvariantFromText "win") >>= #setAttributeValue section "action-namespace"
    #appendItem buttonMenu section

    button <- new Gtk.MenuButton [ #label := "Menu"
                                 , #halign := Gtk.AlignCenter
                                 , #valign := Gtk.AlignStart
                                 ]
    castTo Gio.ActionGroup docActions >>= #insertActionGroup button "doc"
    castTo Gio.MenuModel buttonMenu >>= #setMenuModel button
    #add win button

    #showAll win
      
  return ()



main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.action-namespace" ]
  on app #activate $ appActivate app

  #run app Nothing
  return ()