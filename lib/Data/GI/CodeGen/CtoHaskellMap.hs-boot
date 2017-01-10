module Data.GI.CodeGen.CtoHaskellMap ( cToHaskellMap, Hyperlink )
  where

import qualified Data.Map as M
import Data.GI.CodeGen.GtkDoc (CRef(..))
import Data.GI.CodeGen.API (API(..), Name(..))

data Hyperlink

cToHaskellMap :: [(Name, API)] -> M.Map CRef Hyperlink
