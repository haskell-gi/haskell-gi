module LoadSave(saveLabyrinth, loadLabyrinth) where

import Control.Exception(IOException,try)  

import qualified System.Directory as Directory 
import qualified System.IO as IO
import qualified System.FilePath as Path
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import qualified Data.Digest.Pure.MD5 as MD5 
import qualified Data.Serialize as Cereal
import qualified Data.SafeCopy as SafeCopy (safePut, safeGet) 
import qualified Text.Printf as Printf

import Data.Word(Word8)

import Labyrinth

internalErrorString :: Path.FilePath -> String
internalErrorString = Printf.printf "Internal error while saving \"%s\"" 

couldNotSaveString :: Path.FilePath -> String
couldNotSaveString = Printf.printf "Could not save file \"%s\""  

fileCouldNotBeFoundString :: Path.FilePath -> String 
fileCouldNotBeFoundString = Printf.printf "File \"%s\" could not be found." 

fileIsCorruptedError :: Path.FilePath -> String
fileIsCorruptedError = Printf.printf "The file \"%s\" is corrupted and could not be read."

labyFileMagicBytes :: [Word8]
labyFileMagicBytes = [76,97,98,121]  

saveLabyrinth :: Maybe FrozenLabyrinth -> FilePath 
                                       -> (String -> IO ()) 
                                       -> IO ()
saveLabyrinth Nothing file errorHandler = errorHandler $ internalErrorString file
saveLabyrinth (Just labyrinth) file errorHandler =
  do let byteString = ByteString.pack labyFileMagicBytes
         (_, encoded) = Cereal.runPutMLazy $ SafeCopy.safePut labyrinth
         hashed  = MD5.md5DigestBytes $ MD5.md5 encoded 
         withHash = ByteString.append byteString hashed
         complete = ByteString.append withHash $ LazyByteString.toStrict encoded
     result <- try (ByteString.writeFile file complete)    
     case result of 
         Left exc -> errorHandler $ show ( exc :: IOException )
         Right _  -> return ()

loadLabyrinth :: FilePath -> (String -> IO ()) -> IO (Maybe FrozenLabyrinth)
loadLabyrinth file errorHandler =
  do fileExists <- Directory.doesFileExist file
     if fileExists then loadLabyrinthDo file
     else do errorHandler $ fileCouldNotBeFoundString file 
             return Nothing
  where loadLabyrinthDo file = do result <- try (IO.withBinaryFile file IO.ReadMode (readLabyFile file)) 
                                  case result of
                                      Left exc -> do errorHandler $ show (exc :: IOException)
                                                     return Nothing
                                      Right (Left err) -> do errorHandler err
                                                             return Nothing
                                      Right (Right labyrinth) -> return $ Just labyrinth

readLabyFile :: FilePath -> IO.Handle -> IO (Either String FrozenLabyrinth)
readLabyFile file handle = 
  do magic <- ByteString.hGet handle (length labyFileMagicBytes)
     md5 <- ByteString.hGet handle md5DigestLength
     encodedLabyrinth <- ByteString.hGetContents handle
     let hashed = MD5.md5DigestBytes $ MD5.md5 $ LazyByteString.fromChunks [encodedLabyrinth]  
     if magic == ByteString.pack labyFileMagicBytes && 
        hashed == md5 then return $ decodeLabyrinth encodedLabyrinth 
     else return $ Left $ fileIsCorruptedError file
  where md5DigestLength = 16 

decodeLabyrinth :: ByteString.ByteString -> Either String FrozenLabyrinth
decodeLabyrinth encodedLabyrinth = 
  let decoded = Cereal.runGet SafeCopy.safeGet encodedLabyrinth
  in case decoded of 
      Left error -> Left error
      Right labyrinth -> Right labyrinth
