{-# LANGUAGE RecordWildCards, NamedFieldPuns, OverloadedStrings #-}

module GI.GObject
    ( klass
    , parseObjectHierarchy
    , instanceTree
    , isGObject
    ) where

import Data.Map (Map)
import qualified Data.Map as M

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
isGObject (TInterface "GObject" "Object") = return True
isGObject (TInterface ns' n') = go ns' n'
          where
            go ns n = do
             cfg <- config
             case M.lookup (Name ns n) (instances cfg) of
               Just (Name "GObject" "Object") -> return True
               Just (Name pns pn) -> go pns pn
               Nothing -> return False
isGObject _ = return False

-- Construct the hierarchy of object instances. Also transform
-- GObject.InitiallyUnowned to GObject.Object (the only difference
-- is in the treatment of the floating reference, but we do not
-- want to expose that in the API).
parseObjectHierarchy :: Map Name API -> Map Name Name
parseObjectHierarchy input = M.mapMaybe (rename . readParent) input
                     where
                     readParent :: API -> Maybe Name
                     readParent (APIObject o) =
                                case objFields o of
                                     (Field _ (TInterface pns pn) _) : _ ->
                                            let pn' = Name pns pn in
                                            if isAnObject pn' then
                                               Just pn'
                                            else
                                               Nothing
                                     _ -> Nothing
                     readParent _ = Nothing

                     rename :: Maybe Name -> Maybe Name
                     rename (Just (Name "GObject" "InitiallyUnowned")) =
                              Just (Name "GObject" "Object")
                     rename x = x

                     isAnObject n = case M.lookup n input of
                                Just (APIObject _) -> True
                                Just (_) -> False
                                Nothing -> error $ "Did not find " ++
                                           (nameToString n) ++ " in input."

                     nameToString (Name ns n) = ns ++ "." ++ n
