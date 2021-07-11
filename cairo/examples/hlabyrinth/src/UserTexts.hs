module UserTexts(
  UserText(..), 
  ErrorMessage(..),
  DialogWidgetTexts(..),
  Language(..),
  BoxText(..),
  AboutHelpText(..),
  getLanguage,
  renderLegend
) where

import qualified System.FilePath as Path 
import qualified Text.Printf as Printf 
import qualified Control.Exception as Exception

data Language = English | German

defaultLanguage :: Language
defaultLanguage = English

getLanguage :: String -> Language
getLanguage languageString
  | length languageString < 2 = defaultLanguage
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
  FileWritePermissionError Path.FilePath Exception.IOException | 
  StartPointNotSet  | TargetPointNotSet | NoPathFound | 
  InternalErrorInPathFinder | InternalError |
  BorderSizeError Int Int |
  BoxSizeError [String]

data DialogWidgetTexts =
  FileSaveCaption | FileOpenCaption | FileSave | FileOpen |
  LabyrinthFileFilter String

data BoxText = BoxStartFieldText | BoxTargetFieldText

data LegendText = DrawBoxText | ClearBoxText | QuitText | PlaceStartText | 
                  PlaceTargetText | ClearLabyrinthText | FindPathText | ResetPathText |
                  SaveText | LoadText deriving(Enum, Bounded)

data AboutHelpText = ShortDescription | BorderSizeHelp Int | BoxSizeHelp Int [String] 
 
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
  translate English StartPointNotSet =
    Printf.printf "No start point has been set. No path can be calculated."
  translate English TargetPointNotSet =
    Printf.printf "No target point has been set. No path can be calculated." 
  translate English NoPathFound =
    Printf.printf "No path between start and target could be found."  
  translate English InternalErrorInPathFinder =
    Printf.printf "Internal error in path finding. Processing aborted."   
  translate English InternalError =
    Printf.printf "Internal error. Processing aborted."
  translate English (BorderSizeError borderSize boxSize) =
    Printf.printf "The border size \"%d\" must be smaller than the box size \"%d\"." borderSize boxSize
  translate English (BoxSizeError lst) =
    "Accepted box sizes are: " ++ show lst

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
  translate German StartPointNotSet =
    Printf.printf "Es wurde kein Startpunkt gesetzt. Daher kann kein Weg gefunden werden."
  translate German TargetPointNotSet =
    Printf.printf "Es wurde kein Zielpunkt gesetzt. Daher kann kein Weg gefunden werden." 
  translate German NoPathFound =
    Printf.printf "Es konnte kein Weg zwischen Start- und Zielpunkt gefunden werden."   
  translate German InternalErrorInPathFinder =
    Printf.printf "Es wurde ein interner Fehler in der Wegbestimmung festgestellt. Der Vorgang wurde abgebrochen."    
  translate German InternalError =
    Printf.printf "Es ist ein interner Fehler aufgetreten. Der Vorgang wurde abgebrochen."
  translate German (BorderSizeError borderSize boxSize) =
    Printf.printf "The Rahmendicke \"%d\" muss kleiner als die Kästchengröße \"%d\" sein." borderSize boxSize
  translate German (BoxSizeError lst) =
    "Erlaubte Kästchengrößen: " ++ show lst

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

instance UserText LegendText where
  translate English DrawBoxText        = "LEFT BTN: DRAW" 
  translate English ClearBoxText       = "RIGHT BTN: CLEAR" 
  translate English QuitText           = "ESC: QUIT"
  translate English PlaceStartText     = "s: PLACE START"
  translate English PlaceTargetText    = "t: PLACE TARGET"
  translate English ClearLabyrinthText = "c: CLEAR LABYRINTH" 
  translate English FindPathText       = "w: FIND PATH"  
  translate English ResetPathText      = "r: RESET PATH"   
  translate English SaveText           = "F1: SAVE"    
  translate English LoadText           = "F2: LOAD"     

  translate German DrawBoxText        = "MAUS LINKS: FELD ZEICHNEN" 
  translate German ClearBoxText       = "MAUS RECHTS: FELD LÖSCHEN" 
  translate German QuitText           = "ESC: BEENDEN"
  translate German PlaceStartText     = "s: START PLATZIEREN"
  translate German PlaceTargetText    = "t: ZIEL PLATZIEREN"
  translate German ClearLabyrinthText = "c: LABYRINTH LÖSCHEN" 
  translate German FindPathText       = "w: WEG FINDEN"  
  translate German ResetPathText      = "r: WEG ZURÜCKSETZEN"   
  translate German SaveText           = "F1: SPEICHERN"    
  translate German LoadText           = "F2: LADEN" 

instance UserText AboutHelpText where
  translate English ShortDescription  = "A simple labyrinth game"
  translate English (BorderSizeHelp defaultBorderSize) = 
    Printf.printf "Size of the border in pixels (default %d)" defaultBorderSize
  translate English (BoxSizeHelp defaultBoxSize allowedBoxSizes) = 
    Printf.printf "Size of a single box in pixels (default %d, possible values: %s)" defaultBoxSize (show allowedBoxSizes)

  translate German ShortDescription   = "Ein einfaches Labyrinthspiel"
  translate German (BorderSizeHelp defaultBorderSize) = 
    Printf.printf "Rahmendicke in Pixeln (Standardwert: \"%d\")" defaultBorderSize
  translate German (BoxSizeHelp defaultBoxSize allowedBoxSizes) = 
    Printf.printf "Kästchengröße in Pixeln (Standardwert: \"%d\", erlaubte Werte: %s)" defaultBoxSize (show allowedBoxSizes)

renderLegend :: Language -> String
renderLegend language = 
  let concatDelim "" y = y
      concatDelim x y  = x ++ " | " ++ y
      legendPieces = [minBound..maxBound] :: [LegendText] 
  in foldl concatDelim  "" (map (translate language) legendPieces)
