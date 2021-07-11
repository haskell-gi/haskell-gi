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

import Data.Word(Word8)

import Labyrinth
import UserTexts

labyFileMagicBytes :: [Word8]
labyFileMagicBytes = [76,97,98,121]  

saveLabyrinth :: Maybe FrozenLabyrinth -> FilePath 
                                       -> IO (Either ErrorMessage ())
saveLabyrinth Nothing file = return $ Left $ FileInternalErrorWhileSaving file
saveLabyrinth (Just labyrinth) file =
  do let byteString = ByteString.pack labyFileMagicBytes
         (_, encoded) = Cereal.runPutMLazy $ SafeCopy.safePut labyrinth
         hashed  = MD5.md5DigestBytes $ MD5.md5 encoded 
         withHash = ByteString.append byteString hashed
         complete = ByteString.append withHash $ LazyByteString.toStrict encoded
     result <- try (ByteString.writeFile file complete)
     case result of 
         Left exc -> return $ Left $ FileWritePermissionError file exc
         Right _  -> return $ Right ()

loadLabyrinth :: FilePath -> IO (Either ErrorMessage FrozenLabyrinth)
loadLabyrinth file =
  do fileExists <- Directory.doesFileExist file
     if fileExists then loadLabyrinthDo file
     else return $ Left $ FileCouldNotBeFound file
  where loadLabyrinthDo :: FilePath -> IO (Either ErrorMessage FrozenLabyrinth)
        loadLabyrinthDo file = do result <- try (IO.withBinaryFile file IO.ReadMode (readLabyFile file)) 
                                  case result of
                                      Left exc -> return $ Left $ FileReadPermissionError file exc
                                      Right (Left err) -> return $ Left err
                                      Right (Right labyrinth) -> return $ Right labyrinth

readLabyFile :: FilePath -> IO.Handle -> IO (Either ErrorMessage FrozenLabyrinth)
readLabyFile file handle = 
  do magic <- ByteString.hGet handle (length labyFileMagicBytes)
     md5 <- ByteString.hGet handle md5DigestLength
     encodedLabyrinth <- ByteString.hGetContents handle
     let hashed = MD5.md5DigestBytes $ MD5.md5 $ LazyByteString.fromChunks [encodedLabyrinth]  
     if magic == ByteString.pack labyFileMagicBytes && 
        hashed == md5 then return $ decodeLabyrinth encodedLabyrinth file
     else return $ Left $ FileIsCorrupted file
  where md5DigestLength = 16 

decodeLabyrinth :: ByteString.ByteString -> (FilePath -> Either ErrorMessage FrozenLabyrinth)
decodeLabyrinth encodedLabyrinth = 
  let decoded = Cereal.runGet SafeCopy.safeGet encodedLabyrinth
  in case decoded of 
      Left _ -> Left . FileIsCorrupted
      Right labyrinth -> \_ -> Right labyrinth
