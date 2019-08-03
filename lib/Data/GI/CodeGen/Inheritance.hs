{-# LANGUAGE ScopedTypeVariables #-}
module Data.GI.CodeGen.Inheritance
    ( fullObjectPropertyList
    , fullInterfacePropertyList
    , fullObjectSignalList
    , fullInterfaceSignalList
    , fullObjectMethodList
    , fullInterfaceMethodList
    , instanceTree
    ) where

import Control.Monad (foldM, when)
import qualified Data.Map as M
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code (findAPIByName, CodeGen, line)
import Data.GI.CodeGen.Util (tshow)
import Data.GI.CodeGen.Fixups (dropMovedItems)

-- | Find the parent of a given object when building the
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

-- | Compute the (ordered) list of parents of the current object.
instanceTree :: Name -> CodeGen [Name]
instanceTree n = do
  api <- findAPIByName n
  case getParent api of
    Just p -> (p :) <$> instanceTree p
    Nothing -> return []

-- A class for qualities of an object/interface that it inherits from
-- its ancestors. Properties and Signals are two classes of interest.
class Inheritable i where
    ifInheritables :: Interface -> [i]
    objInheritables :: Object -> [i]
    iName :: i -> Text

instance Inheritable Property where
    ifInheritables = ifProperties
    objInheritables = objProperties
    iName = propName

instance Inheritable Signal where
    ifInheritables = ifSignals
    objInheritables = objSignals
    iName = sigName

instance Inheritable Method where
    ifInheritables = ifMethods
    objInheritables = objMethods
    iName = name . methodName

-- Returns a list of all inheritables defined for this object
-- (including those defined by its ancestors and the interfaces it
-- implements), together with the name of the interface defining the
-- property.
apiInheritables :: Inheritable i => Name -> CodeGen [(Name, i)]
apiInheritables n = do
  api <- findAPIByName n
  case dropMovedItems api of
    Just (APIInterface iface) -> return $ map ((,) n) (ifInheritables iface)
    Just (APIObject object) -> return $ map ((,) n) (objInheritables object)
    _ -> error $ "apiInheritables : Unexpected API : " ++ show n

fullAPIInheritableList :: Inheritable i => Name -> CodeGen [(Name, i)]
fullAPIInheritableList n = do
  api <- findAPIByName n
  case api of
    APIInterface iface -> fullInterfaceInheritableList n iface
    APIObject object -> fullObjectInheritableList n object
    _ -> error $ "FullAPIInheritableList : Unexpected API : " ++ show n

fullObjectInheritableList :: Inheritable i => Name -> Object ->
                             CodeGen [(Name, i)]
fullObjectInheritableList n obj = do
  iT <- instanceTree n
  (++) <$> (concat <$> mapM apiInheritables (n : iT))
       <*> (concat <$> mapM apiInheritables (objInterfaces obj))

fullInterfaceInheritableList :: Inheritable i => Name -> Interface ->
                                CodeGen [(Name, i)]
fullInterfaceInheritableList n iface =
  (++) (map ((,) n) (ifInheritables iface))
    <$> (concat <$> mapM fullAPIInheritableList (ifPrerequisites iface))

-- | It is sometimes the case that a property name or signal is defined
-- both in an object and in one of its ancestors/implemented
-- interfaces. This is harmless if the properties are isomorphic
-- (there will be more than one qualified set of property
-- setters/getters that we can call, but they are all isomorphic). If
-- they are not isomorphic we print a warning, and choose to use the
-- one closest to the leaves of the object hierarchy.
removeDuplicates :: forall i. (Eq i, Show i, Inheritable i) =>
                        Bool -> [(Name, i)] -> CodeGen [(Name, i)]
removeDuplicates verbose inheritables =
    (filterTainted . M.toList) <$> foldM filterDups M.empty inheritables
    where
      filterDups :: M.Map Text (Bool, Name, i) -> (Name, i) ->
                    CodeGen (M.Map Text (Bool, Name, i))
      filterDups m (name, prop) =
        case M.lookup (iName prop) m of
          Just (tainted, n, p)
              | tainted     -> return m
              | (p == prop) -> return m -- Duplicated, but isomorphic property
              | otherwise   ->
                do when verbose $ do
                     line   "--- XXX Duplicated object with different types:"
                     line $ "  --- " <> tshow n <> " -> " <> tshow p
                     line $ "  --- " <> tshow name <> " -> " <> tshow prop
                   -- Tainted
                   return $ M.insert (iName prop) (True, n, p) m
          Nothing -> return $ M.insert (iName prop) (False, name, prop) m
      filterTainted :: [(Text, (Bool, Name, i))] -> [(Name, i)]
      filterTainted xs =
          [(name, prop) | (_, (_, name, prop)) <- xs]

-- | List all properties defined for an object, including those
-- defined by its ancestors.
fullObjectPropertyList :: Name -> Object -> CodeGen [(Name, Property)]
fullObjectPropertyList n o = fullObjectInheritableList n o >>=
                         removeDuplicates True

-- | List all properties defined for an interface, including those
-- defined by its prerequisites.
fullInterfacePropertyList :: Name -> Interface -> CodeGen [(Name, Property)]
fullInterfacePropertyList n i = fullInterfaceInheritableList n i >>=
                            removeDuplicates True

-- | List all signals defined for an object, including those
-- defined by its ancestors.
fullObjectSignalList :: Name -> Object -> CodeGen [(Name, Signal)]
fullObjectSignalList n o = fullObjectInheritableList n o >>=
                           removeDuplicates True

-- | List all signals defined for an interface, including those
-- defined by its prerequisites.
fullInterfaceSignalList :: Name -> Interface -> CodeGen [(Name, Signal)]
fullInterfaceSignalList n i = fullInterfaceInheritableList n i >>=
                              removeDuplicates True

-- | List all methods defined for an object, including those defined
-- by its ancestors.
fullObjectMethodList :: Name -> Object -> CodeGen [(Name, Method)]
fullObjectMethodList n o = fullObjectInheritableList n o >>=
                           removeDuplicates False

-- | List all methods defined for an interface, including those
-- defined by its prerequisites.
fullInterfaceMethodList :: Name -> Interface -> CodeGen [(Name, Method)]
fullInterfaceMethodList n i = fullInterfaceInheritableList n i >>=
                              removeDuplicates False
