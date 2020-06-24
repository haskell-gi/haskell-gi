{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- This is a translation of the Hello World gstreamer example to haskell
-- Original C source:
-- https://gstreamer.freedesktop.org/documentation/application-development/basics/helloworld.html
-- Written by Jaro Reinders on 2017-01-09
--
-- All comments are copied from the source C code

import qualified GI.GLib as GLib
import qualified GI.Gst  as Gst

import Data.GI.Base.Properties (setObjectPropertyString)

import Control.Monad (void, when)
import System.Environment
import System.IO (stderr)
import System.Exit (exitFailure)
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)

busCall :: GLib.MainLoop -> Gst.Bus -> Gst.Message -> IO Bool
busCall loop _bus message = do
  messageTypes <- Gst.getMessageType message

  when (Gst.MessageTypeEos `elem` messageTypes) $ do
    putStrLn "End of stream"
    GLib.mainLoopQuit loop

  when (Gst.MessageTypeError `elem` messageTypes) $ do
    (gerror,_debug) <- Gst.messageParseError message
    T.hPutStrLn stderr . ("Error: " <>) =<< Gst.gerrorMessage gerror
    GLib.mainLoopQuit loop

  return True

onPadAdded :: Gst.Element -> Gst.Pad -> IO ()
onPadAdded decoder pad = do
  -- We can now link this pad with the vorbis-decoder sink pad
  putStrLn "Dynamic pad created, linking demuxer/decoder"
  Just sinkPad <- Gst.elementGetStaticPad decoder "sink"
  void $ Gst.padLink pad sinkPad

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  -- Initialization
  void $ Gst.init Nothing

  loop <- GLib.mainLoopNew Nothing False

  -- Check input arguments
  when (length args /= 1) $ do
    T.hPutStrLn stderr $ "Usage: " <> pack progName <> " <Ogg/Vorbis filename>"
    exitFailure
  let filename = head args

  -- Crate gstreamer elements
  pipeline <- Gst.pipelineNew (Just "audio-player")

  let makeElement factoryname name =
        fromMaybe (error $ unpack $ "Could not create " <> name)
        <$> Gst.elementFactoryMake factoryname (Just name)

  source  <- makeElement "filesrc"       "file-source"
  demuxer <- makeElement "oggdemux"      "ogg-demuxer"
  decoder <- makeElement "vorbisdec"     "vorbis-decoder"
  conv    <- makeElement "audioconvert"  "converter"
  sink    <- makeElement "autoaudiosink" "audio-output"

  -- Set up the pipeline

  -- we set the input filename to the source element
  setObjectPropertyString source "location" (Just $ pack filename)

  -- we add a message handler
  bus <- Gst.pipelineGetBus pipeline
  busWatchId <- Gst.busAddWatch bus GLib.PRIORITY_DEFAULT (busCall loop)

  -- we add all elements into the pipeline
  -- file-source | ogg-demuxer | vorbis-decoder | converter | alsa-output
  mapM_ (Gst.binAdd pipeline) [source, demuxer, decoder, conv, sink]

  -- we link the elements together
  -- file-source -> ogg-demuxer ~> vorbis-decoder -> converter -> alsa-output
  void $ Gst.elementLink source demuxer

  let elementLinkMany (a:b:cs) = (&&) <$> Gst.elementLink a b <*> elementLinkMany (b:cs)
      elementLinkMany _ = return True

  void $ elementLinkMany [decoder, conv, sink]

  void $ Gst.onElementPadAdded demuxer (onPadAdded decoder)

  {- note that the demuxer will be linked to the decoder dynamically.
     The reason is that Ogg may contain various streams (for example
     audio and video). The source pad(s) will be created at run time,
     by the demuxer when it detects the amount and nature of streams.
     Therefore we connect a callback function which will be executed
     when the "pad-added" is emitted.-}

  -- Set the pipeline to "playing" state
  putStrLn $ "Now playing: " ++ filename
  void $ Gst.elementSetState pipeline Gst.StatePlaying

  -- Iterate
  putStrLn "Running..."
  #run loop

  -- Out of the main loop, clean up nicely
  putStrLn "Returned, stopping playback"
  void $ Gst.elementSetState pipeline Gst.StateNull

  putStrLn "Deleting pipeline"
  void $ GLib.sourceRemove busWatchId
