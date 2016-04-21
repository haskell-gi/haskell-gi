module Data.GI.CodeGen.GObject
    ( isGObject
    , apiIsGObject
    , nameIsGObject
    , isInitiallyUnowned
    , apiIsInitiallyUnowned
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Type

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

-- | Check whether the given name descends from GObject.
nameIsGObject :: Name -> CodeGen Bool
nameIsGObject n = findAPIByName n >>= apiIsGObject n

apiIsGObject :: Name -> API -> CodeGen Bool
apiIsGObject = apiDoParentSearch $ Name "GObject" "Object"

isInitiallyUnowned :: Type -> CodeGen Bool
isInitiallyUnowned = typeDoParentSearch $ Name "GObject" "InitiallyUnowned"

apiIsInitiallyUnowned :: Name -> API -> CodeGen Bool
apiIsInitiallyUnowned = apiDoParentSearch $ Name "GObject" "InitiallyUnowned"
