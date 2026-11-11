{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.GI.Base
import Declarative.Gtk.HListStore (HListStore(..))
import Declarative.Gtk.GOWrapper (goUnwrapHValue, GOWrapper(..))
import qualified GI.GObject as GObject
import qualified GI.Gtk as Gtk

import Control.Monad ((>=>), void)
import Data.Text (Text)
import qualified Data.Text as T

-- | Return the values contained in the HListStore, in order.
items :: HListStore a -> IO [a]
items l = collectItems 0 []
  where collectItems n acc = l.getItem n >>= \case
          Just item -> collectItems (n+1) (item : acc)
          Nothing -> return $ reverse acc

printList :: Show a => HListStore a -> IO ()
printList = items >=> print

data Person = Person { name :: Text, age :: Integer }
  deriving (Show, Ord, Eq)

constructModel :: IO (HListStore Person)
constructModel = do
  model <- new HListStore []

  model.append $ Person {name = "Ron", age = 25}
  model.append $ Person {name = "Ethan", age = 32}
  printList model

  -- Insert 19 so that every element before it is smaller than it.
  _ <- model.insertSorted $ Person {name = "Zack", age = 27}
  printList model

  -- Reverse sort the list.
  model.sortBy (flip compare)
  printList model

  return model

bindListItem :: (Person -> Text) -> GObject.Object -> IO ()
bindListItem display obj = do
  listItem <- unsafeCastTo Gtk.ListItem obj
  listItem.getItem >>= \case
    Nothing -> error "Empty listItem, aborting"
    Just item -> do
      goWrapper <- unsafeCastTo GOWrapper item
      listItem `get` #child >>= \case
        Nothing -> error "Empty list child, aborting"
        Just labelWidget -> do
          label <- unsafeCastTo Gtk.Label labelWidget
          hValue <- goUnwrapHValue goWrapper
          label `set` [#label := display hValue ]

-- XXX This should actually (bidirectionally) bind the property in the
-- Label, using attributes, so it can be modified/updated
-- automatically.
makeFactory :: (Person -> Text) -> IO Gtk.SignalListItemFactory
makeFactory display =
  new Gtk.SignalListItemFactory [
  On #setup (\obj -> do
                listItem <- unsafeCastTo Gtk.ListItem obj
                listItem `set` [#child :=> new Gtk.Label []]),
  On #bind (bindListItem display)
  ]

activate :: Gtk.Application -> IO ()
activate app = do
  people <- constructModel
  cv <- new Gtk.ColumnView [
    #showColumnSeparators := True,
    #model :=> new Gtk.SingleSelection [ #model := people,
                                         #autoselect := True ]
    ]

  new Gtk.ColumnViewColumn [#title := "Name",
                            #expand := True,
                            #factory :=> makeFactory name ]
    >>= cv.appendColumn

  new Gtk.ColumnViewColumn [#title := "Age",
                            #expand := True,
                            #factory :=> makeFactory (T.pack . show . age) ]
    >>= cv.appendColumn

  win <- new Gtk.ApplicationWindow [
    #application := app,
    #title := "HListStore example",
    #child :=> new Gtk.ScrolledWindow [ #child := cv ]
    ]
  win.present

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.test.HListStore",
                              On #activate (activate ?self)]
  void $ app.run Nothing
