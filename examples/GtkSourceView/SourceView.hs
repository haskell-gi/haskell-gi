{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as S

import Data.GI.Base

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getProgName, getArgs)

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  let fname = case args of
        fname:[] -> fname
        _ -> error "Please give the name of the file to open as a cmdline arg"

  _ <- Gtk.init . Just . map T.pack $ progName : args

  scrolledWin <- new Gtk.ScrolledWindow []
  win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
                        , #iconName := "applications-haskell"
                        , #defaultWidth := 1024
                        , #defaultHeight := 768
                        , #child := scrolledWin ]
  on win #destroy Gtk.mainQuit

  lm <- new S.LanguageManager []
  maybeLang <- #guessLanguage lm (Just $ T.pack fname) Nothing
  sourceText <- TIO.readFile fname
  buffer <- case maybeLang of
    Just lang -> new S.Buffer [#text := sourceText, #language := lang]
    Nothing -> new S.Buffer [#text := sourceText]
  view <- new S.View [ #buffer := buffer
                     , #backgroundPattern := S.BackgroundPatternTypeGrid
                     , #showLineNumbers := True
                     , #showLineMarks := True
                     , #highlightCurrentLine := True ]

  #add scrolledWin view

  header <- new Gtk.HeaderBar [#showCloseButton := True,
                               #title := T.pack fname]
  #setTitlebar win (Just header)

  #showAll win

  Gtk.main
