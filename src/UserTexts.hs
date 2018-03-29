module UserTexts(
  UserText(..), 
  ErrorMessage(..),
  DialogWidgetTexts(..),
  Language(..),
  BoxText(..),
  localeGetLanguage
) where

import qualified System.FilePath as Path 
import qualified Text.Printf as Printf 
import qualified Control.Exception as Exception

data Language = English | German

defaultLanguage :: Language
defaultLanguage = English

localeGetLanguage :: Maybe String -> Language
localeGetLanguage Nothing = defaultLanguage
localeGetLanguage (Just languageString)
  | length languageString <= 2 = defaultLanguage
  | otherwise = localeGetLanguageDo $ take 2 languageString 
  where localeGetLanguageDo lang = case lang of 
                                       "de" -> German
                                       _    -> defaultLanguage

class UserText a where
  translate :: Language -> a -> String 

data ErrorMessage = 
  FileInternalErrorWhileSaving Path.FilePath |
  FileCouldNotSave Path.FilePath | 
  FileCouldNotBeFound Path.FilePath |
  FileIsCorrupted Path.FilePath |
  FileReadPermissionError Path.FilePath Exception.IOException |
  FileWritePermissionError Path.FilePath Exception.IOException

data DialogWidgetTexts =
  FileSaveCaption | FileOpenCaption | FileSave | FileOpen |
  LabyrinthFileFilter String

data BoxText = BoxStartFieldText | BoxTargetFieldText

instance UserText ErrorMessage where  
  translate English (FileInternalErrorWhileSaving file) = 
    Printf.printf "Internal error while saving \"%s\"." file
  translate English (FileCouldNotSave file) = 
    Printf.printf "Could not save file \"%s\"." file 
  translate English (FileCouldNotBeFound file) =
    Printf.printf "File \"%s\" could not be found." file 
  translate English (FileIsCorrupted file) =
    Printf.printf "The file \"%s\" is corrupted and could not be read." file
  translate English (FileReadPermissionError file _) =
    Printf.printf "Insufficient permissions while reading the file \"%s\"." file 
  translate English (FileWritePermissionError file _) =
    Printf.printf "Insufficient permissions while writing the file \"%s\"." file  

  translate German (FileInternalErrorWhileSaving file) = 
    Printf.printf "Interner Fehler beim Speichern der Datei \"%s\"." file
  translate German (FileCouldNotSave file) = 
    Printf.printf "Fehler beim Speichern der Datei \"%s\"." file 
  translate German (FileCouldNotBeFound file) =
    Printf.printf "Die Datei \"%s\" wurde nicht gefunden." file 
  translate German (FileIsCorrupted file) =
    Printf.printf "Die Datei \"%s\" ist beschädigt und konnte nicht geladen werden." file 
  translate German (FileReadPermissionError file _) =
    Printf.printf "Fehlende Berechtigung zum Lesen der Datei \"%s\"." file  
  translate German (FileWritePermissionError file _) =
    Printf.printf "Fehlende Berechtigung zum Schreiben der Datei \"%s\"." file   

instance UserText DialogWidgetTexts where  
  translate English FileSaveCaption = "Save File"
  translate English FileOpenCaption = "Open File"
  translate English FileSave = "Save"
  translate English FileOpen = "Open"
  translate English (LabyrinthFileFilter filter) =
    Printf.printf "Labyrinth files (%s)" filter

  translate German FileSaveCaption = "Datei Speichern"
  translate German FileOpenCaption = "Datei Öffnen"
  translate German FileSave = "Speichern"
  translate German FileOpen = "Öffnen" 
  translate German (LabyrinthFileFilter filter) =
    Printf.printf "Labyrinth Dateien (%s)" filter 

instance UserText BoxText where
  translate English BoxStartFieldText  = "S"
  translate English BoxTargetFieldText = "T"
  
  translate German BoxStartFieldText = "S"
  translate German BoxTargetFieldText = "Z" 
