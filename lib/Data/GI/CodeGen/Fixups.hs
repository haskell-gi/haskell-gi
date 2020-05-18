-- | Various fixups in the introspection data.
module Data.GI.CodeGen.Fixups
    ( dropMovedItems
    , guessPropertyNullability
    , detectGObject
    , dropDuplicatedFields
    , checkClosureDestructors
    , fixSymbolNaming
    ) where

import Data.Char (generalCategory, GeneralCategory(UppercaseLetter))
import Data.Maybe (isNothing, isJust)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Set as S
import qualified Data.Text as T

import Data.GI.CodeGen.API

-- | Remove functions and methods annotated with "moved-to".
dropMovedItems :: API -> Maybe API
dropMovedItems (APIFunction f) = if fnMovedTo f == Nothing
                                 then Just (APIFunction f)
                                 else Nothing
dropMovedItems (APIInterface i) =
    (Just . APIInterface) i {ifMethods = filterMovedMethods (ifMethods i)}
dropMovedItems (APIObject o) =
    (Just . APIObject) o {objMethods = filterMovedMethods (objMethods o)}
dropMovedItems (APIStruct s) =
    (Just . APIStruct) s {structMethods = filterMovedMethods (structMethods s)}
dropMovedItems (APIUnion u) =
    (Just . APIUnion) u {unionMethods = filterMovedMethods (unionMethods u)}
dropMovedItems a = Just a

-- | Drop the moved methods.
filterMovedMethods :: [Method] -> [Method]
filterMovedMethods = filter (isNothing . methodMovedTo)

-- | GObject-introspection does not currently support nullability
-- annotations, so we try to guess the nullability from the
-- nullability annotations of the curresponding get/set methods, which
-- in principle should be reliable.
guessPropertyNullability :: (Name, API) -> (Name, API)
guessPropertyNullability (n, APIObject obj) =
    (n, APIObject (guessObjectPropertyNullability obj))
guessPropertyNullability (n, APIInterface iface) =
    (n, APIInterface (guessInterfacePropertyNullability iface))
guessPropertyNullability other = other

-- | Guess nullability for the properties of an object.
guessObjectPropertyNullability :: Object -> Object
guessObjectPropertyNullability obj =
    obj {objProperties = map (guessNullability (objMethods obj))
                         (objProperties obj)}

-- | Guess nullability for the properties of an interface.
guessInterfacePropertyNullability :: Interface -> Interface
guessInterfacePropertyNullability iface =
    iface {ifProperties = map (guessNullability (ifMethods iface))
                              (ifProperties iface)}

-- | Guess the nullability for a property, given the list of methods
-- for the object/interface.
guessNullability :: [Method] -> Property -> Property
guessNullability methods = guessReadNullability methods
                         . guessWriteNullability methods

-- | Guess whether "get" on the given property may return NULL, based
-- on the corresponding "get_prop_name" method, if it exists.
guessReadNullability :: [Method] -> Property -> Property
guessReadNullability methods p
    | isJust (propReadNullable p) = p
    | otherwise = p {propReadNullable = nullableGetter}
    where
      nullableGetter :: Maybe Bool
      nullableGetter =
          let prop_name = T.replace "-" "_" (propName p)
          in case findMethod methods ("get_" <> prop_name) of
               Nothing -> Nothing
               -- Check that it looks like a sensible getter
               -- for the property.
               Just m ->
                   let c = methodCallable m
                   in if length (args c) == 1 &&
                      returnType c == Just (propType p) &&
                      returnTransfer c == TransferNothing &&
                      skipReturn c == False &&
                      callableThrows c == False &&
                      methodType m == OrdinaryMethod &&
                      methodMovedTo m == Nothing
                      then Just (returnMayBeNull c)
                      else Nothing

-- | Guess whether "set" on the given property may return NULL, based
-- on the corresponding "set_prop_name" method, if it exists.
guessWriteNullability :: [Method] -> Property -> Property
guessWriteNullability methods p
    | isJust (propWriteNullable p) = p
    | otherwise = p {propWriteNullable = nullableSetter}
    where
      nullableSetter :: Maybe Bool
      nullableSetter =
          let prop_name = T.replace "-" "_" (propName p)
          in case findMethod methods ("set_" <> prop_name) of
               Nothing -> Nothing
               -- Check that it looks like a sensible setter.
               Just m ->
                   let c = methodCallable m
                   in if length (args c) == 2 &&
                          (argType . last . args) c == propType p &&
                          returnType c == Nothing &&
                          (transfer . last . args) c == TransferNothing &&
                          (direction . last . args) c == DirectionIn &&
                          methodMovedTo m == Nothing &&
                          methodType m == OrdinaryMethod &&
                          callableThrows c == False
                      then Just ((mayBeNull . last . args) c)
                      else Nothing

-- | Find the first method with the given name, if any.
findMethod :: [Method] -> T.Text -> Maybe Method
findMethod methods n = case filter ((== n) . name . methodName) methods of
                         [m] -> Just m
                         _ -> Nothing

-- | Not every interface that provides signals/properties is marked as
-- requiring GObject, but this is necessarily the case, so fix the
-- introspection data accordingly.
detectGObject :: (Name, API) -> (Name, API)
detectGObject (n, APIInterface iface) =
  if not (null (ifProperties iface) && null (ifSignals iface))
  then let gobject = Name "GObject" "Object"
       in if gobject `elem` (ifPrerequisites iface)
          then (n, APIInterface iface)
          else (n, APIInterface (iface {ifPrerequisites =
                                        gobject : ifPrerequisites iface}))
  else (n, APIInterface iface)
detectGObject api = api

-- | Drop any fields whose name coincides with that of a previous
-- element. Note that this function keeps ordering.
dropDuplicatedEnumFields :: Enumeration -> Enumeration
dropDuplicatedEnumFields enum =
  enum{enumMembers = dropDuplicates S.empty (enumMembers enum)}
  where dropDuplicates :: S.Set T.Text -> [EnumerationMember] -> [EnumerationMember]
        dropDuplicates _        []     = []
        dropDuplicates previous (m:ms) =
          if enumMemberName m `S.member` previous
          then dropDuplicates previous ms
          else m : dropDuplicates (S.insert (enumMemberName m) previous) ms

-- | Some libraries include duplicated flags by mistake, drop those.
dropDuplicatedFields :: (Name, API) -> (Name, API)
dropDuplicatedFields (n, APIFlags (Flags enum)) =
  (n, APIFlags (Flags $ dropDuplicatedEnumFields enum))
dropDuplicatedFields (n, api) = (n, api)

-- | Sometimes arguments are marked as being a user_data destructor,
-- but there is no associated user_data argument. In this case we drop
-- the annotation.
checkClosureDestructors :: (Name, API) -> (Name, API)
checkClosureDestructors (n, APIObject o) =
  (n, APIObject (o {objMethods = checkMethodDestructors (objMethods o)}))
checkClosureDestructors (n, APIInterface i) =
  (n, APIInterface (i {ifMethods = checkMethodDestructors (ifMethods i)}))
checkClosureDestructors (n, APIStruct s) =
  (n, APIStruct (s {structMethods = checkMethodDestructors (structMethods s)}))
checkClosureDestructors (n, APIUnion u) =
  (n, APIUnion (u {unionMethods = checkMethodDestructors (unionMethods u)}))
checkClosureDestructors (n, APIFunction f) =
  (n, APIFunction (f {fnCallable = checkCallableDestructors (fnCallable f)}))
checkClosureDestructors (n, api) = (n, api)

checkMethodDestructors :: [Method] -> [Method]
checkMethodDestructors = map checkMethod
  where checkMethod :: Method -> Method
        checkMethod m = m {methodCallable =
                             checkCallableDestructors (methodCallable m)}

-- | If any argument for the callable has a associated destroyer for
-- the user_data, but no associated user_data, drop the destroyer
-- annotation.
checkCallableDestructors :: Callable -> Callable
checkCallableDestructors c = c {args = map checkArg (args c)}
  where checkArg :: Arg -> Arg
        checkArg arg = if argDestroy arg >= 0 && argClosure arg == -1
                       then arg {argDestroy = -1}
                       else arg

-- | Some symbols have names that are not valid Haskell identifiers,
-- fix that here.
fixSymbolNaming :: (Name, API) -> (Name, API)
fixSymbolNaming (n, APIConst c) = (fixConstantName n, APIConst c)
fixSymbolNaming (n, api) = (n, api)

-- | Make sure that the given name is a valid Haskell identifier in
-- patterns.
--
-- === __Examples__
-- >>> fixConstantName (Name "IBus" "0")
-- Name {namespace = "IBus", name = "C'0"}
--
-- >>> fixConstantName (Name "IBus" "a")
-- Name {namespace = "IBus", name = "C'a"}
--
-- >>> fixConstantName (Name "IBus" "A")
-- Name {namespace = "IBus", name = "A"}
fixConstantName :: Name -> Name
fixConstantName (Name ns n)
  | not (T.null n) && generalCategory (T.head n) /= UppercaseLetter
  = Name ns ("C'" <> n)
  | otherwise = Name ns n
