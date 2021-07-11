module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Exit(die)

import qualified GI.Gtk as GTK
import qualified GI.Pango as Pango
import qualified Data.Text as Text
import UserTexts

import MainWindow

allowedBoxSizes :: [String]
allowedBoxSizes = ["16", "32", "64", "128"] 

defaultBoxSize :: Int
defaultBoxSize = 64

defaultBorderSize :: Int
defaultBorderSize = 2

checkBorderSize :: MainConfiguration -> IO ()
checkBorderSize opt = checkBorderSizeDo borderSize boxSize language
  where borderSize = cfgBorderSize opt
        boxSize = cfgBoxSize opt 
        language = cfgLanguage opt
        checkBorderSizeDo borderSize boxSize language | borderSize < boxSize = return ()
                                                      | otherwise = die $ errorBorderSize language borderSize boxSize

errorBorderSize :: Language -> Int -> Int -> String
errorBorderSize language borderSize boxSize = 
  translate language $ BorderSizeError borderSize boxSize

main :: IO ()
main = 
  do
    GTK.init Nothing 
    language <- GTK.getDefaultLanguage
    languageString <- Pango.languageToString language 
    let language = getLanguage $ Text.unpack languageString 
    options <- execParser (opts language)
    checkBorderSize options
    run options
    where
      opts lang = info ((cmdParser lang) <**> helper)
       ( fullDesc
        <> progDesc (translate lang ShortDescription) )

cmdParser :: Language -> Parser MainConfiguration
cmdParser lang = MainConfiguration
                  <$> option (parseBoxSize lang)
                      ( long "box-size"
                    <> short 's'
                    <> help (helpBoxSize lang)
                    <> value defaultBoxSize
                    <> metavar "SIZE" )
                  <*> option auto
                    ( long "border-size"
                    <> short 'b'
                    <> help (helpBorderSize lang)
                    <> value defaultBorderSize
                    <> metavar "SIZE" )
                  <*> pure lang

parseBoxSize :: Language -> ReadM Int
parseBoxSize lang = str >>= \s -> 
                    if s `elem` allowedBoxSizes then return ( read s :: Int )
                    else readerError $ translate lang $ BoxSizeError allowedBoxSizes

helpBorderSize :: Language -> String
helpBorderSize lang = translate lang $ BorderSizeHelp defaultBorderSize

helpBoxSize :: Language -> String
helpBoxSize lang = translate lang $ BoxSizeHelp defaultBoxSize allowedBoxSizes

