module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import MainWindow

allowedBoxSizes :: [String]
allowedBoxSizes = ["16", "32", "64", "128"] 

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
      <> progDesc "A simple labyrinth game" ) 

cmdParser :: Parser CmdOptions
cmdParser = CmdOptions
          <$> option parseBoxSize
              ( long "box-size"
             <> short 's'
             <> help ( "Size of a single box in pixels "
                  ++ show allowedBoxSizes ) 
             <> value 64
             <> metavar "SIZE" )

parseBoxSize :: ReadM Int
parseBoxSize = str >>= \s -> 
                   if s `elem` allowedBoxSizes then return ( read s :: Int )
                   else readerError ( "Accepted box sizes are " ++ show allowedBoxSizes )
       
