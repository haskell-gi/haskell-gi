-- | Various fixups in the introspection data.
module Data.GI.CodeGen.Fixups
    ( dropMovedItems
    , guessPropertyNullability
    , detectGObject
    , dropDuplicatedFields
    , checkClosureDestructors
    , fixClosures
    , fixCallbackUserData
    , fixSymbolNaming
    ) where

import Data.Char (generalCategory, GeneralCategory(UppercaseLetter))
import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.Map as M
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Set as S
import qualified Data.Text as T

import Data.GI.CodeGen.Type
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
    iface { ifProperties = map (guessNullability (ifMethods iface))
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
              getter = fromMaybe ("get_" <> prop_name) (propGetter p)
          in case findMethod methods getter of
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
              setter = fromMaybe ("set_" <> prop_name) (propSetter p)
          in case findMethod methods setter of
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

-- | If any argument for the callable has an associated destroyer for
-- the user_data, but no associated user_data, drop the destroyer
-- annotation.
checkCallableDestructors :: Callable -> Callable
checkCallableDestructors c = c {args = map checkArg (args c)}
  where checkArg :: Arg -> Arg
        checkArg arg = if argDestroy arg >= 0 && argClosure arg == -1
                       then arg {argDestroy = -1}
                       else arg

-- | Sometimes it is the callback that is annotated with the (closure
-- user_data) annotation, and sometimes the user_data parameter
-- itself, with (closure callback) pointing to the callback. The
-- following code makes sure that the annotation is on the callable
-- only. Note that this goes against the official gobject
-- introspection spec, but there is more code using this convention
-- than otherwise, and the gir generator seems to add closure
-- annotations in both directions when using the new convention
-- anyway.
fixCallableClosures :: Callable -> Callable
fixCallableClosures c = c {args = map fixupArg (zip [0..] (args c))}
  where fixupArg :: (Int, Arg) -> Arg
        fixupArg (n, arg) = if isUserData arg
                            then arg {argClosure = -1}
                            else
                              case M.lookup n reverseMap of
                                Just user_data -> arg {argClosure = user_data}
                                Nothing -> arg

        -- Map from callbacks to their corresponding user_data
        -- arguments, obtained by looking to the argClosure value for
        -- the user_data argument.
        reverseMap :: M.Map Int Int
        reverseMap = M.fromList
          . map (\(n, arg) -> (argClosure arg, n))
          . filter (isUserData . snd)
          . filter ((/= -1) . argClosure . snd)
          $ zip [0..] (args c)

        isUserData :: Arg -> Bool
        isUserData arg = argScope arg == ScopeTypeInvalid ||
                         argType arg == TBasicType TPtr

-- | Closures are often incorrectly assigned, with the closure
-- annotation on the callback, instead of in the closure (user_data)
-- parameter itself. The following makes sure that things are as they
-- should.
fixClosures :: (Name, API) -> (Name, API)
fixClosures (n, APIObject o) =
  (n, APIObject (o {objMethods = fixMethodClosures (objMethods o)}))
fixClosures (n, APIInterface i) =
  (n, APIInterface (i {ifMethods = fixMethodClosures (ifMethods i)}))
fixClosures (n, APIStruct s) =
  (n, APIStruct (s {structMethods = fixMethodClosures (structMethods s)}))
fixClosures (n, APIUnion u) =
  (n, APIUnion (u {unionMethods = fixMethodClosures (unionMethods u)}))
fixClosures (n, APIFunction f) =
  (n, APIFunction (f {fnCallable = fixCallableClosures (fnCallable f)}))
fixClosures (n, api) = (n, api)

fixMethodClosures :: [Method] -> [Method]
fixMethodClosures = map fixMethod
  where fixMethod :: Method -> Method
        fixMethod m = m {methodCallable =
                            fixCallableClosures (methodCallable m)}

-- | The last argument of callbacks is often a @user_data@ argument,
-- but currently gobject-introspection does not have an annotation
-- representing this. This is generally OK, since the gir generator
-- will mark these arguments as @(closure)@ if they are named
-- @user_data@, and we do the right things in this case, but recently
-- there has been a push to "fix" these annotations by removing them
-- without providing any replacement, which breaks the bindings. See
-- https://gitlab.gnome.org/GNOME/gobject-introspection/-/issues/450
-- Here we try to guess which arguments in callbacks are user_data
-- arguments.
fixCallbackUserData :: (Name, API) -> (Name, API)
fixCallbackUserData (n, APICallback cb) =
  (n, APICallback (cb {cbCallable = fixCallableUserData (cbCallable cb)}))
fixCallbackUserData (n, api) = (n, api)

-- | Any argument with a closure index pointing to itself is a
-- "user_data" type argument.
fixCallableUserData :: Callable -> Callable
fixCallableUserData c = c {args = fixLast 0 (args c)}
  where
    fixLast :: Int -> [Arg] -> [Arg]
    fixLast _ [] = []
    fixLast n (arg:[])
      | argType arg == TBasicType TPtr &&
        argClosure arg == n =
          [arg {argClosure = -1, argCallbackUserData = True}]
      | otherwise = [arg]
    fixLast n (arg:rest) = arg : fixLast (n+1) rest

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
