module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data CmdOptions = CmdOptions 
  { cmdBoxSize :: Int }

allowedBoxSizes :: [String]
allowedBoxSizes = ["16", "32", "64", "128"] 

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
      <> progDesc "A simple labyrinth game" ) 

run :: CmdOptions -> IO()
run option = print $ cmdBoxSize option 

cmdParser :: Parser CmdOptions
cmdParser = CmdOptions
          <$> option parseBoxSize
              ( long "box-size"
             <> short 's'
             <> help ( "Size of a single box in pixels "
                  ++ ( printList allowedBoxSizes ) ) 
             <> value 64
             <> metavar "SIZE" )

parseBoxSize :: ReadM Int
parseBoxSize = str >>= \s -> 
                  case s `elem` allowedBoxSizes of
                    True -> return ( read s :: Int )
                    False -> readerError ( "Accepted box sizes are " ++ ( printList allowedBoxSizes ) )

printList :: [String] -> String
printList list = let foldfunc x y = case y of 
                                      [] -> x
                                      otherwise -> x ++ ", " ++ y
                 in "[" ++ ( foldr foldfunc "" list ) ++ "]"



       
