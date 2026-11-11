{-# LANGUAGE OverloadedStrings #-}
-- | Runtime checks of struct fields whose layout haskell-gi needs to
-- recompute, since the typelib gets it wrong: bitfields, arrays of
-- structs containing bitfields, ... See
-- <https://github.com/haskell-gi/haskell-gi/issues/485> and
-- <https://github.com/haskell-gi/haskell-gi/issues/486>.
module Main (main) where

import Control.Monad (forM, forM_, replicateM, unless)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Exit (exitFailure)

import qualified GI.GLib as GLib
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: String -> Bool -> IO ()
      check what ok = do
        putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
        unless ok $ modifyIORef failures (+ 1)

  fontMap <- PangoCairo.fontMapGetDefault
  context <- Pango.fontMapCreateContext fontMap
  layout <- Pango.layoutNew context

  glyphStringGlyphs check layout
  logAttrs check layout
  bitfieldSetters check
  dateBitfields check

  n <- readIORef failures
  putStrLn (show n ++ " failures")
  unless (n == 0) exitFailure

type Checker = String -> Bool -> IO ()

-- | The glyphs in a PangoGlyphString are an array of PangoGlyphInfo
-- (whose size the typelib gets wrong, since it embeds a
-- PangoGlyphVisAttr with bitfields), with the length given by
-- another field (#485).
glyphStringGlyphs :: Checker -> Pango.Layout -> IO ()
glyphStringGlyphs check layout = do
  Pango.layoutSetText layout "Hello, world" (-1)
  Just line <- Pango.layoutGetLine layout 0
  runs <- Pango.getLayoutLineRuns line
  check "the layout line has runs" (not (null runs))
  forM_ runs $ \run -> do
    Just glyphString <- Pango.getGlyphItemGlyphs run
    numGlyphs <- Pango.getGlyphStringNumGlyphs glyphString
    glyphs <- fromMaybe [] <$> Pango.getGlyphStringGlyphs glyphString
    check ("GlyphString.glyphs has num_glyphs = " ++ show numGlyphs ++ " elements")
          (length glyphs == fromIntegral numGlyphs)
    widths <- forM glyphs $ \g ->
      Pango.getGlyphInfoGeometry g >>= Pango.getGlyphGeometryWidth
    total <- Pango.glyphStringGetWidth glyphString
    check ("the glyph widths add up to pango_glyph_string_get_width ("
           ++ show (sum widths) ++ " == " ++ show total ++ ")")
          (sum widths == total)

-- | Arrays of PangoLogAttr (a struct of bitfields), both returned by
-- Pango and filled in by Pango in an array we allocate (#486).
logAttrs :: Checker -> Pango.Layout -> IO ()
logAttrs check layout = do
  let text = "ab cd"
  Pango.layoutSetText layout text (-1)
  fromLayout <- Pango.layoutGetLogAttrsReadonly layout
  check "one PangoLogAttr per position in the text"
        (length fromLayout == T.length text + 1)

  buffer <- replicateM (T.length text + 1) Pango.newZeroLogAttr
  Just language <- Pango.languageFromString (Just "en")
  fromFunction <- Pango.getLogAttrs text (fromIntegral (T.length text)) (-1)
                                    language buffer

  layoutFields <- mapM logAttrFields fromLayout
  functionFields <- mapM logAttrFields fromFunction
  check "pango_get_log_attrs agrees with pango_layout_get_log_attrs_readonly"
        (functionFields == layoutFields)

  let at :: Int -> Int -> Word32
      at field pos = (layoutFields !! pos) !! field
      white = 0; wordStart = 1; wordEnd = 2
  check "is_white is set at the space, but not at the letters"
        (at white 2 == 1 && all (\p -> at white p == 0) [0, 1, 3, 4])
  check "is_word_start is set at the start of the words only"
        (map (at wordStart) [0 .. 4] == [1, 0, 0, 1, 0])
  check "is_word_end is set at the end of the words only"
        (map (at wordEnd) [0 .. 5] == [0, 0, 1, 0, 0, 1])

  where logAttrFields :: Pango.LogAttr -> IO [Word32]
        logAttrFields attr = forM [ Pango.getLogAttrIsWhite
                                  , Pango.getLogAttrIsWordStart
                                  , Pango.getLogAttrIsWordEnd ] ($ attr)

-- | Setting a bitfield should not modify the neighbouring bitfields.
bitfieldSetters :: Checker -> IO ()
bitfieldSetters check = do
  attr <- Pango.newZeroLogAttr
  Pango.setLogAttrIsWhite attr 1
  Pango.setLogAttrIsWordStart attr 1
  Pango.setLogAttrIsWordEnd attr 1
  Pango.setLogAttrIsWordStart attr 0
  values <- forM [ Pango.getLogAttrIsLineBreak, Pango.getLogAttrIsWhite
                 , Pango.getLogAttrIsWordStart, Pango.getLogAttrIsWordEnd
                 , Pango.getLogAttrIsSentenceEnd ] ($ attr)
  check ("bitfield setters only modify their own bits: " ++ show values)
        (values == [0, 1, 0, 1, 0])

-- | Wider bitfields: GDate has day:6, month:4 and year:16 bitfields.
dateBitfields :: Checker -> IO ()
dateBitfields check = do
  date <- GLib.dateNewDmy 11 GLib.DateMonthNovember 2026
  day <- GLib.getDateDay date
  month <- GLib.getDateMonth date
  year <- GLib.getDateYear date
  dmy <- GLib.getDateDmy date
  check ("reading the GDate bitfields: " ++ show (day, month, year, dmy))
        ((day, month, year, dmy) == (11, 11, 2026, 1))

  GLib.setDateYear date 1999
  valid <- GLib.dateValid date
  d <- GLib.dateGetDay date
  m <- GLib.dateGetMonth date
  y <- GLib.dateGetYear date
  check ("writing the GDate year bitfield: g_date_get_* returns " ++ show (d, m, y))
        (valid && (d, m, y) == (11, GLib.DateMonthNovember, 1999))
