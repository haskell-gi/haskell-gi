module Data.GI.CodeGen.GObject
    ( isGObject
    , apiIsGObject
    , nameIsGObject
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Type

-- Returns whether the given type is a descendant of the given parent.
typeDoParentSearch :: Name -> Type -> CodeGen Bool
typeDoParentSearch parent (TInterface n) = findAPIByName n >>=
                                           apiDoParentSearch parent n
typeDoParentSearch _ _ = return False

apiDoParentSearch :: Name -> Name -> API -> CodeGen Bool
apiDoParentSearch parent n api
    | parent == n = return True
    | otherwise   = case api of
      APIObject o ->
        case objParent o of
          Just  p -> typeDoParentSearch parent (TInterface p)
          Nothing -> return False
      APIInterface iface ->
        do let prs = ifPrerequisites iface
           prereqs <- zip prs <$> mapM findAPIByName prs
           or <$> mapM (uncurry (apiDoParentSearch parent)) prereqs
      _ -> return False

-- | Check whether the given type descends from GObject.
isGObject :: Type -> CodeGen Bool
isGObject = typeDoParentSearch $ Name "GObject" "Object"

-- | Check whether the given name descends from GObject.
nameIsGObject :: Name -> CodeGen Bool
nameIsGObject n = findAPIByName n >>= apiIsGObject n

-- | Check whether the given API descends from GObject.
apiIsGObject :: Name -> API -> CodeGen Bool
apiIsGObject = apiDoParentSearch $ Name "GObject" "Object"
