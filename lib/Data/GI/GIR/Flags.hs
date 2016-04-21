-- | Parsing of bitfields, a.k.a. flags. They are represented in the
-- same way as enums, so this is a thin wrapper over that code.
module Data.GI.GIR.Flags
    ( Flags(..)
    , parseFlags
    ) where

import Data.GI.GIR.Enum (Enumeration, parseEnum)
import Data.GI.GIR.Parser

data Flags = Flags Enumeration
    deriving Show

parseFlags :: Parser (Name, Flags)
parseFlags = do
  (n, enum) <- parseEnum
  return (n, Flags enum)
