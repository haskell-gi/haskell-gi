-- Routines dealing with memory management in marshalling functions.

module GI.Transfer
    ( freeInArg
    , freeElements
    , freeContainer

    , freeInArgOnError
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe, isJust)

import GI.API
import GI.Code
import GI.Conversions
import GI.GObject
import GI.Type
import GI.Util
import GI.Internal.ArgInfo

-- Basic primitives for freeing the given types. Types that point to
-- Haskell objects with memory managed by the GC should not be freed
-- here. For containers this is only for freeing the container itself,
-- freeing the elements is done separately.
basicFreeFn :: Type -> Maybe String
basicFreeFn (TBasicType TUTF8) = Just "freeMem"
basicFreeFn (TBasicType TFileName) = Just "freeMem"
basicFreeFn (TBasicType _) = Nothing
basicFreeFn (TInterface _ _) = Nothing
basicFreeFn (TCArray False (-1) (-1) (TBasicType _)) = Nothing -- Just passing it along
basicFreeFn (TCArray _ _ _ _) = Just "freeMem"
basicFreeFn (TGArray _) = Just "unrefGArray"
basicFreeFn (TPtrArray _) = Just "unrefPtrArray"
basicFreeFn (TByteArray) = Just "unrefGByteArray"
basicFreeFn (TGList _) = Just "g_list_free"
basicFreeFn (TGSList _) = Just "g_slist_free"
basicFreeFn (TGHash _ _) = Nothing
basicFreeFn (TError) = Nothing
basicFreeFn (TVariant) = Nothing
basicFreeFn (TParamSpec) = Nothing

-- Basic free primitives in the case that an error occured. This is
-- run in the exception handler, so any type which we ref/allocate
-- with the expectation that the called function will consume it (on
-- TransferEverything) should be freed here.
basicFreeFnOnError :: Type -> Transfer -> CodeGen (Maybe String)
basicFreeFnOnError (TBasicType TUTF8) _ = return $ Just "freeMem"
basicFreeFnOnError (TBasicType TFileName) _ = return $ Just "freeMem"
basicFreeFnOnError (TBasicType _) _ = return Nothing
basicFreeFnOnError TVariant transfer =
    return $ if transfer == TransferEverything
             then Just "unrefGVariant"
             else Nothing
basicFreeFnOnError TParamSpec transfer =
    return $ if transfer == TransferEverything
             then Just "unrefGParamSpec"
             else Nothing
basicFreeFnOnError t@(TInterface _ _) transfer = do
  api <- findAPI t
  case api of
    Just (APIObject _) -> if transfer == TransferEverything
                          then do
                            isGO <- isGObject t
                            if isGO
                            then return $ Just "unrefObject"
                            else do
                              line $ "-- XXX Transfer a non-GObject object"
                              return Nothing
                          else return Nothing
    Just (APIInterface _) -> if transfer == TransferEverything
                             then do
                               isGO <- isGObject t
                               if isGO
                               then return $ Just "unrefObject"
                               else do
                                 line $ "-- XXX Transfer a non-GObject object"
                                 return Nothing
                             else return Nothing
    Just (APIUnion u) -> if transfer == TransferEverything
                         then if unionIsBoxed u
                              then return $ Just "freeBoxed"
                              else do
                                line $ "-- XXX Transfer a non-boxed union"
                                return Nothing
                         else return Nothing
    Just (APIStruct s) -> if transfer == TransferEverything
                          then if structIsBoxed s
                               then return $ Just "freeBoxed"
                               else do
                                 line $ "-- XXX Transfer a non-boxed struct"
                                 return Nothing
                          else return Nothing
    _ -> return Nothing
-- Arrays without length info are just passed around, we do not need
-- to free them.
basicFreeFnOnError (TCArray False (-1) (-1) (TBasicType _)) _ = return Nothing
basicFreeFnOnError (TCArray _ _ _ _) _ = return $ Just "freeMem"
basicFreeFnOnError (TGArray _) _ = return $ Just "unrefGArray"
basicFreeFnOnError (TPtrArray _) _ = return $ Just "unrefPtrArray"
basicFreeFnOnError (TByteArray) _ = return $ Just "unrefGByteArray"
basicFreeFnOnError (TGList _) _ = return $ Just "g_list_free"
basicFreeFnOnError (TGSList _) _ = return $ Just "g_slist_free"
basicFreeFnOnError (TGHash _ _) _ = return Nothing
basicFreeFnOnError (TError) _ = return Nothing

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
freeContainer :: Type -> String -> CodeGen [String]
freeContainer t label =
    case basicFreeFn t of
      Nothing -> return []
      Just fn -> return [fn ++ " " ++ label]

-- Free the elements in a container type.
freeElements :: Type -> String -> String -> CodeGen [String]
freeElements t label len = return $ fromMaybe [] $ do
   inner <- elementType t
   innerFree <- basicFreeFn inner
   mapFn <- elementMap t len
   return [mapFn ++ " " ++ innerFree ++ " " ++ label]

-- Free the elements of a container type in the case an error ocurred,
-- in particular args that should have been transferred did not get
-- transfered.
freeElementsOnError :: Arg -> String -> String -> CodeGen [String]
freeElementsOnError arg label len =
    case elementType (argType arg) of
      Nothing -> return []
      Just inner -> do
        innerFree <- basicFreeFnOnError inner (transfer arg)
        case innerFree of
          Nothing -> return []
          Just freeFn ->
              case elementMap inner len of
                Nothing -> return []
                Just mapFn -> return [mapFn ++ " " ++ freeFn ++ " " ++ label]

freeIn arg label len = do
    let t = argType arg
    case transfer arg of
      TransferNothing -> (++) <$> freeElements t label len <*> freeContainer t label
      TransferContainer -> freeElements t label len
      TransferEverything -> return []

freeInOnError arg label len =
    (++) <$> freeElementsOnError arg label len
             <*> freeContainer (argType arg) label

freeOut label = return ["freeMem " ++ label]

-- Given an input argument to a C callable, and its label in the code,
-- return the list of actions relevant to freeing the memory allocated
-- for the argument (if appropriate, depending on the ownership
-- transfer semantics of the callable).
freeInArg :: Arg -> String -> String -> CodeGen [String]
freeInArg arg label len = do
  weAlloc <- isJust <$> requiresAlloc (argType arg)
  -- Arguments that we alloc ourselves do not need to be freed, they
  -- will always be soaked up by the wrapPtr constructor, or they will
  -- be DirectionIn.
  if not weAlloc
  then case direction arg of
         DirectionIn -> freeIn arg label len
         DirectionOut -> freeOut label
         DirectionInout -> freeOut label
  else return []

-- Same thing as freeInArg, but called in case the call to C didn't
-- succeed. We thus free everything we allocated in preparation for
-- the call, including args that would have been transferred to C.
freeInArgOnError :: Arg -> String -> String -> CodeGen [String]
freeInArgOnError arg label len = case direction arg of
                            DirectionIn -> freeInOnError arg label len
                            DirectionOut -> freeOut label
                            DirectionInout -> freeOut label
