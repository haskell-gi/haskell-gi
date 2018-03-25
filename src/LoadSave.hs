module LoadSave(saveLabyrinth, loadLabyrinth) where

import Control.Exception(IOException,try)  

import qualified System.Directory as Directory 
import qualified Data.Binary as Binary 
import qualified System.IO as IO
import qualified System.FilePath as Path
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import qualified Data.Digest.Pure.MD5 as MD5 

import Labyrinth

internalErrorString :: FilePath -> String
internalErrorString file = "Internal error while saving: \"" ++ file ++ "\"."

couldNotSaveString :: FilePath -> String
couldNotSaveString file = "Could not save: " ++ file 

fileCouldNotBeFoundString :: FilePath -> String
fileCouldNotBeFoundString file = "File \"" ++ file ++ "\" could not be found." 

fileIsCorruptedError :: FilePath -> String
fileIsCorruptedError file = "The file: \"" ++ file ++ "\" is corrupted and could not be read."

labyFileMagicBytes :: [Binary.Word8]
labyFileMagicBytes = [76,97,98,121]  

saveLabyrinth :: Maybe FrozenLabyrinth -> FilePath 
                                       -> (String -> IO ()) 
                                       -> IO ()
saveLabyrinth Nothing file errorHandler = errorHandler $ internalErrorString file
saveLabyrinth (Just labyrinth) file errorHandler =
  do let byteString = ByteString.pack labyFileMagicBytes
         encoded = Binary.encode labyrinth
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
  let decoded = Binary.decodeOrFail $ LazyByteString.fromChunks [encodedLabyrinth]
  in case decoded of 
      Left (_,_,error) -> Left error
      Right (_,_,labyrinth) -> Right labyrinth
