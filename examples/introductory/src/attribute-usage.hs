{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import           Data.Int

import           Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import           Debug.Trace

appWinWidth :: Int32
appWinWidth = 500

-- This function receives old attribute value and returns the new one in IO monad
retAvgWidth :: Int32 -> IO Int32 
retAvgWidth old = do 
  putStr $ "Input default width (now " ++ show old ++ "):"
  valStr <- getLine
  let inpVal = read valStr :: Int32
  return $ (old + inpVal) `div` 2

retWidthDueHeight :: GObject o => o -> Int32
retWidthDueHeight obj = traceShow 1000 - 500  

reportDefaultWidth :: Gtk.ApplicationWindow -> IO ()
reportDefaultWidth appWin = do 
  w <- get appWin #defaultWidth
  putStrLn $ "Default width is set to " ++ show w ++ "."

appActivate :: Gtk.Application -> IO ()
appActivate app = do
  -- we can set attributes of GTK objects by various ways.
  
  -- when constructing
  appWin <- new Gtk.ApplicationWindow [#application := app]

  reportDefaultWidth appWin

  -- after construction using normal values
  set appWin [ #defaultHeight := 200
             -- using constant 
             , #defaultWidth := appWinWidth
             ]

  reportDefaultWidth appWin
             
  -- using a pure update function. 
  -- This function receives the current value of the attribute and returns the new value.
  set appWin [#defaultWidth :~ \old -> old - 100]

  reportDefaultWidth appWin

  -- You can set attributes with values encapsulated in IO monad
  set appWin [#defaultWidth :=> get appWin #defaultWidth
                                >>= \old -> putStr ("Input width (now " ++ show old ++ "px):") >> getLine
                                >>= return . read 
             ]

  reportDefaultWidth appWin

  -- .. or you can let a monadic update function return the new value. The current value is passed to this function.
  set appWin [#defaultWidth :~> retAvgWidth] 

  reportDefaultWidth appWin

  set appWin [#defaultWidth ::= retWidthDueHeight]

  reportDefaultWidth appWin  

  #showAll appWin
  return ()

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.attribute-usage"
                             , #flags := [Gio.ApplicationFlagsFlagsNone]
                             ]
  on app #activate $ do appActivate app

  #run app Nothing
  return ()