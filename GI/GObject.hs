module GI.GObject
    ( klass
    , goConstraint
    , instanceTree
    , isGObject
    , apiIsGObject
    , isInitiallyUnowned
    , apiIsInitiallyUnowned
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import GI.API
import GI.Code
import GI.Type

klass n = n ++ "Klass"
goConstraint n = n ++ "K"

-- Find the parent of a given object when building the
-- instanceTree. For the purposes of the binding we do not need to
-- distinguish between GObject.Object and GObject.InitiallyUnowned.
getParent :: API -> Maybe Name
getParent (APIObject o) = rename $ objParent o
    where
      rename :: Maybe Name -> Maybe Name
      rename (Just (Name "GObject" "InitiallyUnowned")) =
          Just (Name "GObject" "Object")
      rename x = x
getParent _ = Nothing

-- Compute the (ordered) list of parents of the current object.
instanceTree :: Name -> CodeGen [Name]
instanceTree n = do
  api <- findAPIByName n
  case getParent api of
    Just p -> (p :) <$> instanceTree p
    Nothing -> return []

-- Returns whether the given type is a descendant of the given parent.
typeDoParentSearch :: Name -> Type -> CodeGen Bool
typeDoParentSearch parent (TInterface ns n) = findAPIByName name >>=
                                              apiDoParentSearch parent name
                                                  where name = Name ns n
typeDoParentSearch _ _ = return False

apiDoParentSearch :: Name -> Name -> API -> CodeGen Bool
apiDoParentSearch parent n api
    | parent == n = return True
    | otherwise   = case api of
      APIObject o ->
        case objParent o of
          Just (Name pns pn) -> typeDoParentSearch parent (TInterface pns pn)
          Nothing -> return False
      APIInterface iface ->
        do let prs = ifPrerequisites iface
           prereqs <- zip prs <$> mapM findAPIByName prs
           or <$> mapM (uncurry (apiDoParentSearch parent)) prereqs
      _ -> return False

isGObject :: Type -> CodeGen Bool
isGObject = typeDoParentSearch $ Name "GObject" "Object"

apiIsGObject :: Name -> API -> CodeGen Bool
apiIsGObject = apiDoParentSearch $ Name "GObject" "Object"

isInitiallyUnowned :: Type -> CodeGen Bool
isInitiallyUnowned = typeDoParentSearch $ Name "GObject" "InitiallyUnowned"

apiIsInitiallyUnowned :: Name -> API -> CodeGen Bool
apiIsInitiallyUnowned = apiDoParentSearch $ Name "GObject" "InitiallyUnowned"
