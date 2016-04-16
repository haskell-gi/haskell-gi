-- | Various fixups in the introspection data.
module GI.Fixups
    ( dropMovedItems
    , guessPropertyNullability
    ) where

import Data.Maybe (isNothing, isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T

import GI.API

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
                   in if length (args c) == 0 &&
                      returnType c == Just (propType p) &&
                      returnTransfer c == TransferNothing &&
                      skipReturn c == False &&
                      methodThrows m == False &&
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
                   in if length (args c) == 1 &&
                          (argType . head . args) c == propType p &&
                          returnType c == Nothing &&
                          (transfer . head . args) c == TransferNothing &&
                          (direction . head . args) c == DirectionIn &&
                          methodMovedTo m == Nothing &&
                          methodType m == OrdinaryMethod &&
                          methodThrows m == False
                      then Just ((mayBeNull . head . args) c)
                      else Nothing

-- | Find the first method with the given name, if any.
findMethod :: [Method] -> T.Text -> Maybe Method
findMethod methods n =
    case filter ((== n) . name . methodName) methods of
      [m] -> Just m
      _ -> Nothing
