{-# LANGUAGE RecordWildCards, NamedFieldPuns, OverloadedStrings #-}

module GI.GObject
    ( klass
    , parseObjectHierarchy
    , instanceTree
    , isGObject
    , apiIsGObject
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative ((<$>))

import GI.API
import GI.Code
import GI.Type

klass n = n ++ "Klass"

-- Compute the (ordered) list of parents of the current object.
instanceTree :: Map Name Name -> Name -> [Name]
instanceTree ih n = case M.lookup n ih of
                         Just p -> do
                              p : (instanceTree ih p)
                         Nothing -> []

-- Returns whether the given object instance name is a descendant of
-- GObject.
isGObject :: Type -> CodeGen Bool
isGObject (TInterface ns n) = findAPIByName name >>= apiIsGObject name
                              where name = Name ns n
isGObject _ = return False

apiIsGObject :: Name -> API -> CodeGen Bool
apiIsGObject (Name "GObject" "Object") _ = return True
apiIsGObject n api = do
  cfg <- config
  case api of
    APIObject _ ->
        case M.lookup n (instances cfg) of
          Just (Name pns pn) -> isGObject (TInterface pns pn)
          Nothing -> return False
    APIInterface iface ->
        do let prs = ifPrerequisites iface
           prereqs <- (zip prs) <$> mapM findAPIByName prs
           or <$> mapM (uncurry apiIsGObject) prereqs
    _ -> return False

-- Construct the hierarchy of object instances. Also transform
-- GObject.InitiallyUnowned to GObject.Object (the only difference
-- is in the treatment of the floating reference, but we do not
-- want to expose that in the API).
parseObjectHierarchy :: Map Name API -> Map Name Name
parseObjectHierarchy input = M.mapMaybe (rename . readParent) input
                     where
                     readParent :: API -> Maybe Name
                     readParent (APIObject o) = objParent o
                     readParent _ = Nothing

                     rename :: Maybe Name -> Maybe Name
                     rename (Just (Name "GObject" "InitiallyUnowned")) =
                              Just (Name "GObject" "Object")
                     rename x = x
