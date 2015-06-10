-- Routines dealing with memory management in marshalling functions.

module GI.Transfer
    ( freeInArg
    , freeElements
    , freeContainer

    , freeInArgOnError
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Maybe (isJust)

import GI.API
import GI.Code
import GI.Conversions
import GI.GObject
import GI.SymbolNaming (ucFirst)
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
basicFreeFn (TCArray False (-1) (-1) _) = Nothing -- Just passing it along
basicFreeFn (TCArray{}) = Just "freeMem"
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
                              line "-- XXX Transfer a non-GObject object"
                              return Nothing
                          else return Nothing
    Just (APIInterface _) -> if transfer == TransferEverything
                             then do
                               isGO <- isGObject t
                               if isGO
                               then return $ Just "unrefObject"
                               else do
                                 line "-- XXX Transfer a non-GObject object"
                                 return Nothing
                             else return Nothing
    Just (APIUnion u) -> if transfer == TransferEverything
                         then if unionIsBoxed u
                              then return $ Just "freeBoxed"
                              else do
                                line "-- XXX Transfer a non-boxed union"
                                return Nothing
                         else return Nothing
    Just (APIStruct s) -> if transfer == TransferEverything
                          then if structIsBoxed s
                               then return $ Just "freeBoxed"
                               else do
                                 line "-- XXX Transfer a non-boxed struct"
                                 return Nothing
                          else return Nothing
    _ -> return Nothing
-- Arrays without length info are just passed along, we do not need to
-- free them.
basicFreeFnOnError (TCArray False (-1) (-1) _) _ = return Nothing
basicFreeFnOnError (TCArray{}) _ = return $ Just "freeMem"
basicFreeFnOnError (TGArray _) _ = return $ Just "unrefGArray"
basicFreeFnOnError (TPtrArray _) _ = return $ Just "unrefPtrArray"
basicFreeFnOnError (TByteArray) _ = return $ Just "unrefGByteArray"
basicFreeFnOnError (TGList _) _ = return $ Just "g_list_free"
basicFreeFnOnError (TGSList _) _ = return $ Just "g_slist_free"
basicFreeFnOnError (TGHash _ _) _ = return Nothing
basicFreeFnOnError (TError) _ = return Nothing

-- Free just the container, but not the elements.
freeContainer :: Type -> String -> CodeGen [String]
freeContainer t label =
    case basicFreeFn t of
      Nothing -> return []
      Just fn -> return [fn ++ " " ++ label]

-- Free one element using the given free function.
freeElem :: Type -> String -> String -> ExcCodeGen String
freeElem t label free =
    case elementTypeAndMap t undefined of
      Nothing -> return free
      Just (TCArray False _ _ _, _) ->
          badIntroError $ "Element type in container \"" ++ label ++
                            "\" is an array of unknown length."
      Just (innerType, mapFn) -> do
        let elemFree = "freeElemOf" ++ ucFirst label
        fullyFree innerType (prime label) >>= \case
                  Nothing -> return $ free ++ " e"
                  Just elemInnerFree -> do
                     line $ "let " ++ elemFree ++ " e = " ++ mapFn ++ " "
                              ++ elemInnerFree ++ " e >> " ++ free ++ " e"
                     return elemFree

-- Construct a function to free the memory associated with a type, and
-- recursively free any elements of this type in case that it is a
-- container.
fullyFree :: Type -> String -> ExcCodeGen (Maybe String)
fullyFree t label = case basicFreeFn t of
                      Nothing -> return Nothing
                      Just free -> Just <$> freeElem t label free

-- Like fullyFree, but free the toplevel element using basicFreeFnOnError.
fullyFreeOnError :: Type -> String -> Transfer -> ExcCodeGen (Maybe String)
fullyFreeOnError t label transfer =
    basicFreeFnOnError t transfer >>= \case
        Nothing -> return Nothing
        Just free -> Just <$> freeElem t label free

-- Free the elements in a container type.
freeElements :: Type -> String -> String -> ExcCodeGen [String]
freeElements t label len =
   case elementTypeAndMap t len of
     Nothing -> return []
     Just (inner, mapFn) ->
         fullyFree inner label >>= \case
                   Nothing -> return []
                   Just innerFree ->
                       return [mapFn ++ " " ++ innerFree ++ " " ++ label]

-- Free the elements of a container type in the case an error ocurred,
-- in particular args that should have been transferred did not get
-- transfered.
freeElementsOnError :: Arg -> String -> String -> ExcCodeGen [String]
freeElementsOnError arg label len =
    case elementTypeAndMap (argType arg) len of
      Nothing -> return []
      Just (inner, mapFn) ->
         fullyFreeOnError inner label (transfer arg) >>= \case
                   Nothing -> return []
                   Just innerFree ->
                       return [mapFn ++ " " ++ innerFree ++ " " ++ label]

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
freeInArg :: Arg -> String -> String -> ExcCodeGen [String]
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
freeInArgOnError :: Arg -> String -> String -> ExcCodeGen [String]
freeInArgOnError arg label len = case direction arg of
                            DirectionIn -> freeInOnError arg label len
                            DirectionOut -> freeOut label
                            DirectionInout -> freeOut label
