{-# LANGUAGE ScopedTypeVariables #-}
module GI.Inheritable
    ( fullObjectPropertyList
    , fullInterfacePropertyList
    , fullObjectSignalList
    , fullInterfaceSignalList
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (foldM)
import qualified Data.Map as M
import Data.Text (Text)

import GI.API
import GI.Code (findAPIByName, CodeGen, line)
import GI.GObject (instanceTree)

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

-- Returns a list of all inheritables defined for this object
-- (including those defined by its ancestors and the interfaces it
-- implements), together with the name of the interface defining the
-- property.
apiInheritables :: Inheritable i => Name -> CodeGen [(Name, i)]
apiInheritables n = do
  api <- findAPIByName n
  case api of
    APIInterface iface -> return $ map ((,) n) (ifInheritables iface)
    APIObject object -> return $ map ((,) n) (objInheritables object)
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

-- It is sometimes the case that a property name or signal is defined
-- both in an object and in one of its ancestors/implemented
-- interfaces. This is harmless if the properties are isomorphic
-- (there will be more than one qualified set of property
-- setters/getters that we can call, but they are all isomorphic). If
-- they are not isomorphic we refuse to set either, and print a
-- warning in the generated code.
removeDuplicates :: forall i. (Eq i, Show i, Inheritable i) =>
                        [(Name, i)] -> CodeGen [(Name, i)]
removeDuplicates inheritables =
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
                do line   "--- XXX Duplicated object with different types:"
                   line $ "  --- " ++ show n ++ " -> " ++ show p
                   line $ "  --- " ++ show name ++ " -> " ++ show prop
                   -- Tainted
                   return $ M.insert (iName prop) (True, n, p) m
          Nothing -> return $ M.insert (iName prop) (False, name, prop) m
      filterTainted :: [(Text, (Bool, Name, i))] -> [(Name, i)]
      filterTainted xs =
          [(name, prop) | (_, (tainted, name, prop)) <- xs, not tainted]

fullObjectPropertyList :: Name -> Object -> CodeGen [(Name, Property)]
fullObjectPropertyList n o = fullObjectInheritableList n o >>=
                         removeDuplicates

fullInterfacePropertyList :: Name -> Interface -> CodeGen [(Name, Property)]
fullInterfacePropertyList n i = fullInterfaceInheritableList n i >>=
                            removeDuplicates

fullObjectSignalList :: Name -> Object -> CodeGen [(Name, Signal)]
fullObjectSignalList n o = fullObjectInheritableList n o >>=
                           removeDuplicates

fullInterfaceSignalList :: Name -> Interface -> CodeGen [(Name, Signal)]
fullInterfaceSignalList n i = fullInterfaceInheritableList n i >>=
                              removeDuplicates
