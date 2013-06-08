-- Routines dealing with memory management in marshalling functions.

module GI.Transfer
    ( prepareForCCall
    , unprepareForCCall
    , freeInArg
    , freeElements
    , freeContainer
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import GI.API
import GI.Code
import GI.GObject
import GI.SymbolNaming
import GI.Type
import GI.Util
import GI.Internal.ArgInfo

-- Given an argument to a callable and the name of the variable
-- containing the corresponding C type (as constructed by the routines
-- in GI.Conversions), prepare the object for calling into the C
-- function.
prepareForCCall :: String -> Arg -> CodeGen ()
prepareForCCall label arg =
    case direction arg of
      DirectionIn -> prepareIn label arg True 
      DirectionInout -> prepareIn (unprime label) arg True
      DirectionOut -> return ()

-- Undoes any action performed by prepareForCCall, called in order to
-- avoid memory leaks when the C function emits a GError.
unprepareForCCall :: String -> Arg -> CodeGen ()
unprepareForCCall label arg =
    case direction arg of
      DirectionIn -> prepareIn label arg False
      DirectionInout -> prepareIn (unprime label) arg False
      DirectionOut -> return ()

-- Prepare will be true when called from prepareCCall and False when
-- called from unprepareCCall
prepareIn :: String -> Arg -> Bool -> CodeGen ()
prepareIn label arg prepare = do
  isGO <- isGObject (argType arg)
  case argType arg of
    TInterface _ _ ->
        if transfer arg /= TransferNothing && isGO
        then do
          -- The function "steals" the argument, so add a ref for when
          -- the Haskell runtime destroys the wrapping object (which
          -- will unref the object).
          refFn <- if prepare
                   then (++ "g_object_ref") <$> qualify "GObject"
                   else (++ "g_object_unref") <$> qualify "GObject"
          line $ parenthesize (refFn ++ " . castPtr") ++ " " ++ label
        else return ()
    _ -> return ()

-- Basic primitives for freeing the given types. For containers this
-- is only for freeing the container itself, freeing the elements is
-- done separately.
basicFreeFn :: Type -> Maybe String
basicFreeFn (TBasicType TUTF8) = Just "F.free"
basicFreeFn (TBasicType TFileName) = Just "F.free"
basicFreeFn (TBasicType _) = Nothing
basicFreeFn (TInterface _ _) = Nothing -- prepareForCCall deals with this
basicFreeFn (TCArray _ _ _ _) = Just "F.free"
basicFreeFn (TGArray _) = Just "unrefGArray"
basicFreeFn (TPtrArray _) = Just "unrefPtrArray"
basicFreeFn (TByteArray) = Just "unrefByteArray"
basicFreeFn (TGList _) = Just "g_list_free"
basicFreeFn (TGSList _) = Just "g_slist_free"
basicFreeFn (TGHash _ _) = Nothing
basicFreeFn (TError) = Nothing

-- If the given type maps to a list in Haskell, return the type of the
-- elements.
elementType :: Type -> Maybe Type
elementType (TCArray _ _ _ (TBasicType TUInt8)) = Nothing -- ByteString
elementType (TCArray _ _ _ t) = Just t
elementType (TGArray t) = Just t
elementType (TPtrArray t) = Just t
elementType (TGList t) = Just t
elementType (TGSList t) = Just t
elementType _ = Nothing

-- Return the name of the function mapping over elements in a
-- container type.
elementMap :: Type -> String -> Maybe String
elementMap (TCArray _ _ _ (TBasicType TUInt8)) _ = Nothing -- ByteString
elementMap (TCArray True _ _ _) _ = Just "mapZeroTerminatedCArray"
elementMap (TCArray False fixed _ _) _
    | fixed > (-1) = Just $ parenthesize $ "mapCArrayWithLength " ++ show fixed
elementMap (TCArray False (-1) _ _) len
    = Just $ parenthesize $ "mapCArrayWithLength " ++ len
elementMap (TGArray _) _ = Just "mapGArray"
elementMap (TPtrArray _) _ = Just "mapPtrArray"
elementMap (TGList _) _ = Just "mapGList"
elementMap (TGSList _) _ = Just "mapGSList"
elementMap _ _ = Nothing

-- Free just the container, but not the elements.
freeContainer :: Type -> String -> [String]
freeContainer t label =
    case basicFreeFn t of
      Nothing -> []
      Just fn -> [fn ++ " " ++ label]

-- Free the elements in a container type.
freeElements :: Type -> String -> String -> [String]
freeElements t label len =
    fromMaybe [] $ do
      inner <- elementType t
      innerFree <- basicFreeFn inner
      mapFn <- elementMap t len
      return [mapFn ++ " " ++ innerFree ++ " " ++ label]

freeInIn arg label len =
    let t = argType arg in
    case transfer arg of
      TransferNothing -> freeElements t label len ++ freeContainer t label
      TransferContainer -> freeElements t label len
      TransferEverything -> []

freeInOut label = ["F.free " ++ label]

-- Given an input argument to a C callable, and its label in the code,
-- return the list of actions relevant to freeing the memory allocated
-- for the argument (if appropriate, depending on the ownership
-- transfer semantics of the callable).
freeInArg :: Arg -> String -> String -> [String]
freeInArg arg label len = case direction arg of
                            DirectionIn -> freeInIn arg label len
                            DirectionOut -> freeInOut label
                            DirectionInout -> freeInOut label
