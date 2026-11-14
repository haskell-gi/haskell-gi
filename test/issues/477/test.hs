-- | Regression test for the struct offset problem in
-- <https://github.com/haskell-gi/haskell-gi/issues/477>, see
-- <https://github.com/haskell-gi/haskell-gi/issues/477#issuecomment-3508045340>:
-- 'GstVideo.getVideoFrameBuffer' returned 'Nothing' for mapped frames.
--
-- GstVideoFrame embeds a GstVideoInfo, which ends in an anonymous
-- union. The typelib ignores that union, so it gets the size of
-- GstVideoInfo wrong, and with it the offsets of all the fields of
-- GstVideoFrame after "info" (the buffer was read from the "flags"
-- field, for instance). haskell-gi now computes these layouts from
-- the GIR data instead.
--
-- We check frames mapped from Haskell, frames written from C (in
-- test.c) and read from Haskell, and frames written from Haskell and
-- read from C.
module Main (main) where

import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Int (Int32)
import Data.List (sort)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr)
import System.Exit (exitFailure)

import Data.GI.Base.ManagedPtr (withManagedPtr)

import qualified GI.Gst as Gst
import qualified GI.GstVideo as GstVideo

foreign import ccall "issue477_map_frame" c_map_frame ::
  Ptr GstVideo.VideoFrame -> IO (Ptr Gst.Buffer)
foreign import ccall "issue477_frame_equals" c_frame_equals ::
  Ptr GstVideo.VideoFrame -> Ptr Gst.Buffer -> CInt -> Int32 -> IO CInt
foreign import ccall "issue477_frame_info_is_mapped" c_frame_info_is_mapped ::
  Ptr GstVideo.VideoFrame -> IO CInt
foreign import ccall "issue477_frame_info_is_zero" c_frame_info_is_zero ::
  Ptr GstVideo.VideoFrame -> IO CInt

type Checker = String -> Bool -> IO ()

width, height :: Int
width = 64
height = 48

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check = checker failures

  _ <- Gst.init Nothing
  haskellMaps check
  cWritesHaskellReads check
  haskellWritesCReads check

  n <- readIORef failures
  putStrLn (show n ++ " failures")
  unless (n == 0) exitFailure

checker :: IORef Int -> Checker
checker failures what ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
  unless ok $ modifyIORef failures (+ 1)

-- | The frame flags we use in the tests.
interlacedTff :: [GstVideo.VideoFrameFlags]
interlacedTff = [GstVideo.VideoFrameFlagsInterlaced, GstVideo.VideoFrameFlagsTff]

flagsValue :: [GstVideo.VideoFrameFlags] -> CInt
flagsValue = fromIntegral . sum . map fromEnum

bufferPtr :: Maybe Gst.Buffer -> IO (Ptr Gst.Buffer)
bufferPtr Nothing = return nullPtr
bufferPtr (Just b) = withManagedPtr b return

-- | Width, height and format of the GstVideoInfo embedded in the frame.
frameInfo :: GstVideo.VideoFrame -> IO (Int, Int, GstVideo.VideoFormat)
frameInfo frame = do
  info <- GstVideo.getVideoFrameInfo frame
  w <- GstVideo.getVideoInfoWidth info
  h <- GstVideo.getVideoInfoHeight info
  format <- GstVideo.getVideoInfoFinfo info
              >>= maybe (return GstVideo.VideoFormatUnknown)
                        GstVideo.getVideoFormatInfoFormat
  return (fromIntegral w, fromIntegral h, format)

-- | Map a frame from Haskell (the scenario in the issue), and read it
-- from Haskell and from C.
haskellMaps :: Checker -> IO ()
haskellMaps check = do
  putStrLn "* Frames mapped from Haskell"
  info <- GstVideo.videoInfoNew
  _ <- GstVideo.videoInfoSetFormat info GstVideo.VideoFormatRgba
                                   (fromIntegral width) (fromIntegral height)
  size <- GstVideo.getVideoInfoSize info
  Just buffer <- Gst.bufferNewAllocate (Nothing :: Maybe Gst.Allocator)
                                       (fromIntegral size) Nothing
  (mapped, frame) <- GstVideo.videoFrameMap info buffer [Gst.MapFlagsRead]
  check "gst_video_frame_map succeeds" mapped

  frameBuffer <- GstVideo.getVideoFrameBuffer frame
  check "getVideoFrameBuffer returns a buffer (it used to return Nothing)"
        (frameBuffer /= Nothing)
  expected <- withManagedPtr buffer return
  actual <- bufferPtr frameBuffer
  check "getVideoFrameBuffer returns the mapped buffer" (actual == expected)

  flags <- GstVideo.getVideoFrameFlags frame
  frameId <- GstVideo.getVideoFrameId frame
  check ("flags " ++ show flags ++ " and id " ++ show frameId
         ++ " are the defaults set by gst_video_frame_map")
        (null flags && frameId == -1)
  fi <- frameInfo frame
  check ("the embedded GstVideoInfo reads " ++ show fi)
        (fi == (width, height, GstVideo.VideoFormatRgba))

  ok <- withManagedPtr frame $ \p -> do
    same <- c_frame_equals p expected 0 (-1)
    infoOk <- c_frame_info_is_mapped p
    return (same /= 0 && infoOk /= 0)
  check "C reads the same buffer, flags, id and GstVideoInfo" ok

  -- Writing the fields after the GstVideoInfo from Haskell should not
  -- clobber the GstVideoInfo.
  GstVideo.setVideoFrameFlags frame interlacedTff
  GstVideo.setVideoFrameId frame 99
  okWrite <- withManagedPtr frame $ \p -> do
    same <- c_frame_equals p expected (flagsValue interlacedTff) 99
    infoOk <- c_frame_info_is_mapped p
    return (same /= 0 && infoOk /= 0)
  check "flags and id written from Haskell are seen from C, and leave the GstVideoInfo alone"
        okWrite

  GstVideo.videoFrameUnmap frame

-- | Frames mapped and written from C, and read from Haskell.
cWritesHaskellReads :: Checker -> IO ()
cWritesHaskellReads check = do
  putStrLn "* Frames written from C, read from Haskell"
  frame <- GstVideo.newZeroVideoFrame
  buffer <- withManagedPtr frame c_map_frame
  check "C maps the frame" (buffer /= nullPtr)
  unless (buffer == nullPtr) $ do
    frameBuffer <- GstVideo.getVideoFrameBuffer frame >>= bufferPtr
    check "getVideoFrameBuffer returns the buffer mapped by C" (frameBuffer == buffer)
    flags <- GstVideo.getVideoFrameFlags frame
    check ("getVideoFrameFlags returns " ++ show flags)
          (sort flags == sort interlacedTff)
    frameId <- GstVideo.getVideoFrameId frame
    check ("getVideoFrameId returns " ++ show frameId) (frameId == 42)
    fi <- frameInfo frame
    check ("the embedded GstVideoInfo reads " ++ show fi)
          (fi == (width, height, GstVideo.VideoFormatRgba))
    GstVideo.videoFrameUnmap frame

-- | Frames written from Haskell, and read from C.
haskellWritesCReads :: Checker -> IO ()
haskellWritesCReads check = do
  putStrLn "* Frames written from Haskell, read from C"
  Just buffer <- Gst.bufferNewAllocate (Nothing :: Maybe Gst.Allocator) 16 Nothing
  frame <- GstVideo.newZeroVideoFrame
  withManagedPtr buffer $ \bufPtr -> do
    GstVideo.setVideoFrameBuffer frame bufPtr
    GstVideo.setVideoFrameFlags frame interlacedTff
    GstVideo.setVideoFrameId frame 7
    (same, infoZero) <- withManagedPtr frame $ \p ->
      (,) <$> c_frame_equals p bufPtr (flagsValue interlacedTff) 7
          <*> c_frame_info_is_zero p
    check "the buffer, flags and id written from Haskell are seen from C" (same /= 0)
    check "and they are not written inside the GstVideoInfo" (infoZero /= 0)
    -- The frame does not own a reference to the buffer, so do not
    -- leave it there.
    GstVideo.clearVideoFrameBuffer frame
