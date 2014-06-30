module GI.Lenses
    ( genLenses
    , genAllLenses
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Control.Monad.Writer (tell)
import qualified Data.Set as S

import GI.API
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Properties
import GI.CodeGen (genPrelude)

-- A list of distinct property names for all GObjects appearing in the
-- given list of APIs.
findObjectPropNames :: [(Name, API)] -> CodeGen [String]
findObjectPropNames apis = S.toList <$> go apis S.empty
    where
      go :: [(Name, API)] -> S.Set String -> CodeGen (S.Set String)
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

      insertProps :: [Property] -> S.Set String -> S.Set String
      insertProps props set = foldr (S.insert . propName) set props

genPropertyLens :: String -> CodeGen ()
genPropertyLens pName = group $ do
  line $ "-- Property \"" ++ pName ++ "\""
  let name = hyphensToCamelCase pName
  line $ "_" ++ lcFirst name ++ " :: Attr \"" ++ name ++ "\" o"
  line $ "_" ++ lcFirst name ++ " = undefined"

genProps :: (Name, API) -> CodeGen ()
genProps (n, APIObject o) = genObjectProperties n o
genProps (n, APIInterface i) = genInterfaceProperties n i
genProps _ = return ()

genAllLenses :: [(Name, API)] -> String -> CodeGen ()
genAllLenses allAPIs modulePrefix = do
  line $ "-- Generated code."
  blank
  line $ "{-# LANGUAGE ForeignFunctionInterface, ConstraintKinds,"
  line $ "    TypeFamilies, MultiParamTypeClasses, KindSignatures,"
  line $ "    FlexibleInstances, UndecidableInstances, DataKinds #-}"
  blank

  line $ "module " ++ modulePrefix ++ "Properties where"
  blank
  line $ "import " ++ modulePrefix ++ "Utils.Attributes"
  blank

  propNames <- findObjectPropNames allAPIs
  forM_ propNames $ \name -> do
      genPropertyLens name
      blank

genLenses :: String -> [(Name, API)] -> String -> CodeGen ()
genLenses name apis modulePrefix = do
  let mp = (modulePrefix ++)
      nm = ucFirst name

  -- We generate polymorphic lenses for all properties appearing in
  -- the current module and its dependencies. The reason for
  -- including also properties for the dependencies too is that in
  -- this way one can just import the top module (Gtk, say)
  -- unqualified, and obtain access to all the necessary
  -- lenses.

  code <- recurse' $ forM_ apis genProps

  genPrelude (nm ++ "Attributes") modulePrefix

  deps <- S.toList <$> getDeps
  forM_ deps $ \i -> when (i /= name) $ do
    line $ "import qualified " ++ mp (ucFirst i) ++ " as " ++ ucFirst i
    line $ "import qualified " ++ mp (ucFirst i) ++ "Attributes as "
             ++ ucFirst i ++ "A"

  line $ "import " ++ modulePrefix ++ nm
  blank

  mapM_ (\c -> tell c >> blank) $ codeToList code
