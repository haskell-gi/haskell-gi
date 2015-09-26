module GI.Attributes
    ( genAttributes
    , genAllAttributes
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)
import Control.Monad.Writer (tell)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import GI.API
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Properties
import GI.CodeGen (genPrelude)

-- A list of distinct property names for all GObjects appearing in the
-- given list of APIs.
findObjectPropNames :: [(Name, API)] -> CodeGen [Text]
findObjectPropNames apis = S.toList <$> go apis S.empty
    where
      go :: [(Name, API)] -> S.Set Text -> CodeGen (S.Set Text)
      go [] set = return set
      go ((name, api):apis) set = do
        isGO <- apiIsGObject name api
        if isGO
        then case api of
               APIInterface iface ->
                   go apis $ insertProps (ifProperties iface) set
               APIObject object ->
                   go apis $ insertProps (objProperties object) set
               _ -> error $ "GObject not an Interface or Object!? " ++ show name
        else go apis set

      insertProps :: [Property] -> S.Set Text -> S.Set Text
      insertProps props set = foldr (S.insert . propName) set props

genPropertyAttr :: Text -> CodeGen ()
genPropertyAttr pName = group $ do
  line $ "-- Property \"" ++ T.unpack pName ++ "\""
  let name = (hyphensToCamelCase  . T.unpack) pName
  line $ "_" ++ lcFirst name ++ " :: Proxy \"" ++ T.unpack pName ++ "\""
  line $ "_" ++ lcFirst name ++ " = Proxy"

genAllAttributes :: [(Name, API)] -> String -> CodeGen ()
genAllAttributes allAPIs modulePrefix = do
  line "-- Generated code."
  blank
  line "{-# LANGUAGE DataKinds #-}"
  blank

  line $ "module " ++ modulePrefix ++ "Properties where"
  blank
  line $ "import Data.Proxy (Proxy(..))"
  blank

  propNames <- findObjectPropNames allAPIs
  forM_ propNames $ \name -> do
      genPropertyAttr name
      blank

genProps :: (Name, API) -> CodeGen ()
genProps (n, APIObject o) = genObjectProperties n o
genProps (n, APIInterface i) = genInterfaceProperties n i
genProps _ = return ()

genAttributes :: String -> [(Name, API)] -> String -> CodeGen ()
genAttributes name apis modulePrefix = do
  let mp = (modulePrefix ++)
      nm = ucFirst name

  code <- recurse' $ forM_ apis genProps

  -- Providing orphan instances is the whole point of these modules,
  -- tell GHC that this is fine.
  line "{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}"
  blank

  genPrelude (nm ++ "Attributes") modulePrefix

  deps <- getDeps
  forM_ deps $ \i -> when (i /= name) $ do
    line $ "import qualified " ++ mp (ucFirst i) ++ " as " ++ ucFirst i
    line $ "import qualified " ++ mp (ucFirst i) ++ "Attributes as "
             ++ ucFirst i ++ "A"

  line $ "import " ++ modulePrefix ++ nm
  blank

  tell code
