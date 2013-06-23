module GI.Lenses
    (genLenses
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Set as S

import GI.API
import GI.Code
import GI.GObject
import GI.SymbolNaming

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
  line $ "class HasProperty" ++ name ++ " o w where"
  indent $ do
    line $ "type " ++ name ++ "Readable o"
    line $ "type " ++ name ++ "Constructible o"
    line $ "type " ++ name ++ "OutType o"
    line $ "type " ++ name ++ "InConstraint o :: * -> Constraint"
    line $ "_" ++ lcFirst name ++ " :: (" ++ name ++ "InConstraint o) k => "
    indent $ line $ "Attr o (" ++ name ++ "OutType o) k (" ++ name
                      ++ "Readable o) w (" ++ name ++ "Constructible o)"

genLenses :: String -> [(Name, API)] -> String -> CodeGen ()
genLenses name apis modPrefix = do
  line $ "-- Generated code"
  blank
  line $ "{-# LANGUAGE ForeignFunctionInterface, ConstraintKinds,"
  line $ "    TypeFamilies, MultiParamTypeClasses, KindSignatures,"
  line $ "    FlexibleInstances, UndecidableInstances #-}"
  blank
  line $ "module " ++ modPrefix ++ name ++ "Lenses where"
  blank
  line $ "import " ++ modPrefix ++ "Utils.Attributes (Attr)"
  line $ "import GHC.Exts (Constraint)"
  blank

  -- We generate polymorphic lenses for all properties appearing in
  -- the current module and its dependencies. The reason for
  -- including also properties for the dependencies too is that in
  -- this way one can just import the top module (Gtk, say)
  -- unqualified, and obtain access to all the necessary
  -- lenses.
  propNames <- findObjectPropNames apis
  forM_ propNames $ \name -> do
      genPropertyLens name
      blank
