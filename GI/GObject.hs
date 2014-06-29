module GI.GObject
    ( klass
    , goConstraint
    , instanceTree
    , isGObject
    , apiIsGObject
    ) where

import Control.Applicative ((<$>))

import GI.API
import GI.Code
import GI.Type

klass n = n ++ "Klass"
goConstraint n = n ++ "K"

-- Compute the (ordered) list of parents of the current object.
instanceTree :: Name -> CodeGen [Name]
instanceTree n = do
  api <- findAPIByName n
  case getParent api of
    Just p -> (p :) <$> instanceTree p
    Nothing -> return []

-- Returns whether the given object instance name is a descendant of
-- GObject.
isGObject :: Type -> CodeGen Bool
isGObject (TInterface ns n) = findAPIByName name >>= apiIsGObject name
                              where name = Name ns n
isGObject _ = return False

apiIsGObject :: Name -> API -> CodeGen Bool
apiIsGObject (Name "GObject" "Object") _ = return True
apiIsGObject _ api = do
  case api of
    APIObject _ ->
        case getParent api of
          Just (Name pns pn) -> isGObject (TInterface pns pn)
          Nothing -> return False
    APIInterface iface ->
        do let prs = ifPrerequisites iface
           prereqs <- (zip prs) <$> mapM findAPIByName prs
           or <$> mapM (uncurry apiIsGObject) prereqs
    _ -> return False

-- Find the parent of a given object. For the purposes of the binding
-- we do not need to distinguish between GObject.Object and
-- GObject.InitiallyUnowned.
getParent :: API -> Maybe Name
getParent (APIObject o) = rename $ objParent o
    where
      rename :: Maybe Name -> Maybe Name
      rename (Just (Name "GObject" "InitiallyUnowned")) =
          Just (Name "GObject" "Object")
      rename x = x
getParent _ = Nothing
