-- | Parsing of bitfields, a.k.a. flags. They are represented in the
-- same way as enums, so this is a thin wrapper over that code.
module GI.GIR.Flags
    ( Flags(..)
    , parseFlags
    ) where

import GI.GIR.Enum (Enumeration, parseEnum)
import GI.GIR.BasicTypes (ParseContext, Name)
import Text.XML (Element)

data Flags = Flags Enumeration
    deriving Show

parseFlags :: ParseContext -> Element -> Maybe (Name, Flags)
parseFlags ctx element = do
  (n, enum) <- parseEnum ctx element
  return (n, Flags enum)
