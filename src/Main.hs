module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Exit(die)

import MainWindow

allowedBoxSizes :: [String]
allowedBoxSizes = ["16", "32", "64", "128"] 

defaultBoxSize :: Int
defaultBoxSize = 64

defaultBorderSize :: Int
defaultBorderSize = 2

checkBorderSize :: CmdOptions -> IO ()
checkBorderSize opt = checkBorderSizeDo borderSize boxSize 
  where borderSize = cmdBorderSize opt
        boxSize = cmdBoxSize opt 
        checkBorderSizeDo borderSize boxSize | borderSize < boxSize = return ()
                                             | otherwise = die $ errorBorderSize borderSize boxSize

errorBorderSize :: Int -> Int -> String
errorBorderSize borderSize boxSize = 
  "The border size " ++ show borderSize ++ " must be smaller or equal \
  \to the box size " ++ show boxSize ++ "."

main :: IO ()
main = 
  do
    options <- execParser opts
    checkBorderSize options
    run options
    where
      opts = info (cmdParser <**> helper)
       ( fullDesc
        <> progDesc "A simple labyrinth game" ) 

cmdParser :: Parser CmdOptions
cmdParser = CmdOptions
          <$> option parseBoxSize
              ( long "box-size"
             <> short 's'
             <> help helpBoxSize
             <> value defaultBoxSize
             <> metavar "SIZE" )
          <*> option auto
            ( long "border-size"
             <> short 'b'
             <> help helpBorderSize
             <> value defaultBorderSize
             <> metavar "SIZE" )

parseBoxSize :: ReadM Int
parseBoxSize = str >>= \s -> 
                   if s `elem` allowedBoxSizes then return ( read s :: Int )
                   else readerError ( "Accepted box sizes are " ++ show allowedBoxSizes )

helpBorderSize :: String
helpBorderSize = "Size of the border in pixels (default " ++ show defaultBorderSize ++ ")" 

helpBoxSize :: String
helpBoxSize = "Size of a single box in pixels " ++ show allowedBoxSizes 
               ++ " (default " ++ show defaultBoxSize ++ ")"
       
