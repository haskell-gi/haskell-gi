-- Routines dealing with memory management in marshalling functions.

module Data.GI.CodeGen.Transfer
    ( freeInArg
    , freeInArgOnError
    , freeContainerType
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Monad (when)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Conversions
import Data.GI.CodeGen.GObject
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util

-- Basic primitives for freeing the given types. Types that point to
-- Haskell objects with memory managed by the GC should not be freed
-- here. For containers this is only for freeing the container itself,
-- freeing the elements is done separately.
basicFreeFn :: Type -> Maybe Text
basicFreeFn (TBasicType TUTF8) = Just "freeMem"
basicFreeFn (TBasicType TFileName) = Just "freeMem"
basicFreeFn (TBasicType _) = Nothing
basicFreeFn (TInterface _) = Nothing
-- Just passed along
basicFreeFn (TCArray False (-1) (-1) (TBasicType TUInt8)) = Nothing
basicFreeFn (TCArray{}) = Just "freeMem"
basicFreeFn (TGArray _) = Just "unrefGArray"
basicFreeFn (TPtrArray _) = Just "unrefPtrArray"
basicFreeFn (TByteArray) = Just "unrefGByteArray"
basicFreeFn (TGList _) = Just "g_list_free"
basicFreeFn (TGSList _) = Just "g_slist_free"
basicFreeFn (TGHash _ _) = Just "unrefGHashTable"
basicFreeFn (TError) = Nothing
basicFreeFn (TVariant) = Nothing
basicFreeFn (TGValue) = Nothing
basicFreeFn (TParamSpec) = Nothing
basicFreeFn (TGClosure _) = Nothing

-- Basic free primitives in the case that an error occured. This is
-- run in the exception handler, so any type which we ref/allocate
-- with the expectation that the called function will consume it (on
-- TransferEverything) should be freed here.
basicFreeFnOnError :: Type -> Transfer -> CodeGen (Maybe Text)
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
basicFreeFnOnError TGValue transfer =
    return $ if transfer == TransferEverything
             then Just "SP.freeMem"
             else Nothing
basicFreeFnOnError (TGClosure _) transfer =
    return $ if transfer == TransferEverything
             then Just "B.GClosure.unrefGClosure"
             else Nothing
basicFreeFnOnError t@(TInterface _) transfer = do
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
-- Just passed along
basicFreeFnOnError (TCArray False (-1) (-1) (TBasicType TUInt8)) _ = return Nothing
basicFreeFnOnError (TCArray{}) _ = return $ Just "freeMem"
basicFreeFnOnError (TGArray _) _ = return $ Just "unrefGArray"
basicFreeFnOnError (TPtrArray _) _ = return $ Just "unrefPtrArray"
basicFreeFnOnError (TByteArray) _ = return $ Just "unrefGByteArray"
basicFreeFnOnError (TGList _) _ = return $ Just "g_list_free"
basicFreeFnOnError (TGSList _) _ = return $ Just "g_slist_free"
basicFreeFnOnError (TGHash _ _) _ = return $ Just "unrefGHashTable"
basicFreeFnOnError (TError) _ = return Nothing

-- Free just the container, but not the elements.
freeContainer :: Type -> Text -> CodeGen [Text]
freeContainer t label =
    case basicFreeFn t of
      Nothing -> return []
      Just fn -> return [fn <> " " <> label]

-- Free one element using the given free function.
freeElem :: Type -> Text -> Text -> ExcCodeGen Text
freeElem t label free =
    case elementTypeAndMap t undefined of
      Nothing -> return free
      Just (TCArray False _ _ _, _) ->
          badIntroError $ "Element type in container \"" <> label <>
                            "\" is an array of unknown length."
      Just (innerType, mapFn) -> do
        let elemFree = "freeElemOf" <> ucFirst label
        fullyFree innerType (prime label) >>= \case
                  Nothing -> return $ free <> " e"
                  Just elemInnerFree -> do
                     line $ "let " <> elemFree <> " e = " <> mapFn <> " "
                              <> elemInnerFree <> " e >> " <> free <> " e"
                     return elemFree

-- Construct a function to free the memory associated with a type, and
-- recursively free any elements of this type in case that it is a
-- container.
fullyFree :: Type -> Text -> ExcCodeGen (Maybe Text)
fullyFree t label = case basicFreeFn t of
                      Nothing -> return Nothing
                      Just free -> Just <$> freeElem t label free

-- Like fullyFree, but free the toplevel element using basicFreeFnOnError.
fullyFreeOnError :: Type -> Text -> Transfer -> ExcCodeGen (Maybe Text)
fullyFreeOnError t label transfer =
    basicFreeFnOnError t transfer >>= \case
        Nothing -> return Nothing
        Just free -> Just <$> freeElem t label free

-- Free the elements in a container type.
freeElements :: Type -> Text -> Text -> ExcCodeGen [Text]
freeElements t label len =
   case elementTypeAndMap t len of
     Nothing -> return []
     Just (inner, mapFn) ->
         fullyFree inner label >>= \case
                   Nothing -> return []
                   Just innerFree ->
                       return [mapFn <> " " <> innerFree <> " " <> label]

-- | Free a container and/or the contained elements, depending on the
-- transfer mode.
freeContainerType :: Transfer -> Type -> Text -> Text -> ExcCodeGen ()
freeContainerType transfer (TGHash _ _) label _ = freeGHashTable transfer label
freeContainerType transfer t label len = do
      when (transfer == TransferEverything) $
           mapM_ line =<< freeElements t label len
      when (transfer /= TransferNothing) $
           mapM_ line =<< freeContainer t label

freeGHashTable :: Transfer -> Text -> ExcCodeGen ()
freeGHashTable TransferNothing _ = return ()
freeGHashTable TransferContainer label =
    notImplementedError $ "Hash table argument with transfer = Container? "
                        <> label
-- Hash tables support setting a free function for keys and elements,
-- we assume that these are always properly set. The worst that can
-- happen this way is a memory leak, as opposed to a double free if we
-- try do free anything here.
freeGHashTable TransferEverything label =
    line $ "unrefGHashTable " <> label

-- Free the elements of a container type in the case an error ocurred,
-- in particular args that should have been transferred did not get
-- transfered.
freeElementsOnError :: Transfer -> Type -> Text -> Text ->
                       ExcCodeGen [Text]
freeElementsOnError transfer t label len =
    case elementTypeAndMap t len of
      Nothing -> return []
      Just (inner, mapFn) ->
         fullyFreeOnError inner label transfer >>= \case
                   Nothing -> return []
                   Just innerFree ->
                       return [mapFn <> " " <> innerFree <> " " <> label]

freeIn :: Transfer -> Type -> Text -> Text -> ExcCodeGen [Text]
freeIn transfer (TGHash _ _) label _ =
    freeInGHashTable transfer label
freeIn transfer t label len =
    case transfer of
      TransferNothing -> (<>) <$> freeElements t label len <*> freeContainer t label
      TransferContainer -> freeElements t label len
      TransferEverything -> return []

freeInOnError :: Transfer -> Type -> Text -> Text -> ExcCodeGen [Text]
freeInOnError transfer (TGHash _ _) label _ =
    freeInGHashTable transfer label
freeInOnError transfer t label len =
    (<>) <$> freeElementsOnError transfer t label len
             <*> freeContainer t label

-- See freeGHashTable above.
freeInGHashTable :: Transfer -> Text -> ExcCodeGen [Text]
freeInGHashTable TransferEverything _ = return []
freeInGHashTable TransferContainer label =
    notImplementedError $ "Hash table argument with TransferContainer? "
                        <> label
freeInGHashTable TransferNothing label = return ["unrefGHashTable " <> label]

freeOut :: Text -> CodeGen [Text]
freeOut label = return ["freeMem " <> label]

-- | Given an input argument to a C callable, and its label in the code,
-- return the list of actions relevant to freeing the memory allocated
-- for the argument (if appropriate, depending on the ownership
-- transfer semantics of the callable).
freeInArg :: Arg -> Text -> Text -> ExcCodeGen [Text]
freeInArg arg label len = do
  -- Arguments that we alloc ourselves do not always need to be freed,
  -- they will sometimes be soaked up by the wrapPtr constructor, or
  -- they will be DirectionIn.
  if willWrap arg
    then return []
    else case direction arg of
         DirectionIn -> freeIn (transfer arg) (argType arg) label len
         DirectionOut -> freeOut label
         DirectionInout -> freeOut label

  -- Whether memory ownership of the pointer passed in to the function
  -- will be assumed by the C->Haskell wrapper.
  where willWrap :: Arg -> Bool
        willWrap = argCallerAllocates

-- | Same thing as freeInArg, but called in case the call to C didn't
-- succeed. We thus free everything we allocated in preparation for
-- the call, including args that would have been transferred to C.
freeInArgOnError :: Arg -> Text -> Text -> ExcCodeGen [Text]
freeInArgOnError arg label len =
    case direction arg of
      DirectionIn -> freeInOnError (transfer arg) (argType arg) label len
      DirectionOut -> freeOut label
      DirectionInout ->
          -- Caller-allocates arguments are like "in" arguments for
          -- memory management purposes.
          if argCallerAllocates arg
          then freeInOnError (transfer arg) (argType arg) label len
          else freeOut label
