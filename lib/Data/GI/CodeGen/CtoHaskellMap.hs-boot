module Data.GI.CodeGen.CtoHaskellMap ( cToHaskellMap )
  where

import qualified Data.Map as M
import Data.Text (Text)
import Data.GI.CodeGen.GtkDoc (CRef(..))
import Data.GI.CodeGen.API (API(..), Name(..))

cToHaskellMap :: [(Name, API)] -> M.Map CRef Text
