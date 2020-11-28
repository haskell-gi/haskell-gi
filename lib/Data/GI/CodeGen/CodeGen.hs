module Data.GI.CodeGen.CodeGen
    ( genConstant
    , genFunction
    , genModule
    ) where

import Control.Monad (forM, forM_, when, unless, filterM)
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, catMaybes, mapMaybe)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Callable (genCCallableWrapper)
import Data.GI.CodeGen.Constant (genConstant)
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.EnumFlags (genEnum, genFlags)
import Data.GI.CodeGen.Fixups (dropMovedItems, guessPropertyNullability,
                               detectGObject, dropDuplicatedFields,
                               checkClosureDestructors, fixSymbolNaming)
import Data.GI.CodeGen.GObject
import Data.GI.CodeGen.Haddock (deprecatedPragma, addSectionDocumentation,
                                writeHaddock,
                                RelativeDocPosition(DocBeforeSymbol))
import Data.GI.CodeGen.Inheritance (instanceTree, fullObjectMethodList,
                       fullInterfaceMethodList)
import Data.GI.CodeGen.Properties (genInterfaceProperties, genObjectProperties,
                      genNamespacedPropLabels)
import Data.GI.CodeGen.OverloadedSignals (genInterfaceSignals, genObjectSignals)
import Data.GI.CodeGen.OverloadedMethods (genMethodList, genMethodInfo,
                             genUnsupportedMethodInfo)
import Data.GI.CodeGen.Signal (genSignal, genCallback)
import Data.GI.CodeGen.Struct (genStructOrUnionFields, extractCallbacksInStruct,
                  fixAPIStructs, ignoreStruct, genZeroStruct, genZeroUnion,
                  genBoxed, genWrappedPtr)
import Data.GI.CodeGen.SymbolNaming (upperName, classConstraint,
                                     submoduleLocation, lowerName, qualifiedAPI,
                                     normalizedAPIName, safeCast)
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util (tshow)

genFunction :: Name -> Function -> CodeGen ()
genFunction n (Function symbol fnMovedTo callable) =
    -- Only generate the function if it has not been moved.
    when (Nothing == fnMovedTo) $
      group $ do
        line $ "-- function " <> name n
        handleCGExc (\e -> do
                        line ("-- XXX Could not generate function "
                              <> name n
                              <> "\n")
                        printCGError e)
                        (do
                          genCCallableWrapper n symbol callable
                          export (NamedSubsection MethodSection $ lowerName n) (lowerName n)
                        )

-- | Create the newtype wrapping the ManagedPtr for the given type.
genNewtype :: Text -> CodeGen ()
genNewtype name' = do
  group $ do
    bline $ "newtype " <> name' <> " = " <> name' <> " (SP.ManagedPtr " <> name' <> ")"
    indent $ line $ "deriving (Eq)"

  group $ do
    bline $ "instance SP.ManagedPtrNewtype " <> name' <> " where"
    indent $ line $ "toManagedPtr (" <> name' <> " p) = p"

-- | Generate wrapper for structures.
genStruct :: Name -> Struct -> CodeGen ()
genStruct n s = unless (ignoreStruct n s) $ do
   let Name _ name' = normalizedAPIName (APIStruct s) n

   writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
   genNewtype name'
   exportDecl (name' <> ("(..)"))

   addSectionDocumentation ToplevelSection (structDocumentation s)

   if structIsBoxed s
   then genBoxed n (fromJust $ structTypeInit s)
   else genWrappedPtr n (structAllocationInfo s) (structSize s)

   -- Generate a builder for a structure filled with zeroes.
   genZeroStruct n s

   -- Generate code for fields.
   genStructOrUnionFields n (structFields s)

   -- Methods
   methods <- forM (structMethods s) $ \f -> do
       let mn = methodName f
       isFunction <- symbolFromFunction (methodSymbol f)
       if not isFunction
       then handleCGExc
               (\e -> do line ("-- XXX Could not generate method "
                               <> name' <> "::" <> name mn)
                         printCGError e
                         return Nothing)
               (genMethod n f >> return (Just (n, f)))
       else return Nothing

   -- Overloaded methods
   cppIf CPPOverloading $
        genMethodList n (catMaybes methods)

-- | Generated wrapper for unions.
genUnion :: Name -> Union -> CodeGen ()
genUnion n u = do
  let Name _ name' = normalizedAPIName (APIUnion u) n

  writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
  genNewtype name'
  exportDecl (name' <> "(..)")

  addSectionDocumentation ToplevelSection (unionDocumentation u)

  if unionIsBoxed u
  then genBoxed n (fromJust $ unionTypeInit u)
  else genWrappedPtr n (unionAllocationInfo u) (unionSize u)

  -- Generate a builder for a structure filled with zeroes.
  genZeroUnion n u

  -- Generate code for fields.
  genStructOrUnionFields n (unionFields u)

  -- Methods
  methods <- forM (unionMethods u) $ \f -> do
      let mn = methodName f
      isFunction <- symbolFromFunction (methodSymbol f)
      if not isFunction
      then handleCGExc
                (\e -> do line ("-- XXX Could not generate method "
                                <> name' <> "::" <> name mn)
                          printCGError e
                          return Nothing)
                (genMethod n f >> return (Just (n, f)))
      else return Nothing

  -- Overloaded methods
  cppIf CPPOverloading $
       genMethodList n (catMaybes methods)

-- | When parsing the GIR file we add the implicit object argument to
-- methods of an object.  Since we are prepending an argument we need
-- to adjust the offset of the length arguments of CArrays, and
-- closure and destroyer offsets.
fixMethodArgs :: Callable -> Callable
fixMethodArgs c = c {  args = args'' , returnType = returnType' }
    where
      returnType' = maybe Nothing (Just . fixCArrayLength) (returnType c)
      args' = map (fixDestroyers . fixClosures . fixLengthArg) (args c)
      args'' = fixInstance (head args') : tail args'

      fixLengthArg :: Arg -> Arg
      fixLengthArg arg = arg { argType = fixCArrayLength (argType arg)}

      fixCArrayLength :: Type -> Type
      fixCArrayLength (TCArray zt fixed length t) =
          if length > -1
          then TCArray zt fixed (length+1) t
          else TCArray zt fixed length t

      fixCArrayLength t = t

      fixDestroyers :: Arg -> Arg
      fixDestroyers arg = let destroy = argDestroy arg in
                          if destroy > -1
                          then arg {argDestroy = destroy + 1}
                          else arg

      fixClosures :: Arg -> Arg
      fixClosures arg = let closure = argClosure arg in
                        if closure > -1
                        then arg {argClosure = closure + 1}
                        else arg

      -- We always treat the instance argument of a method as non-null
      -- and "in", even if sometimes the introspection data may say
      -- otherwise.
      fixInstance :: Arg -> Arg
      fixInstance arg = arg { mayBeNull = False
                            , direction = DirectionIn}

-- For constructors we want to return the actual type of the object,
-- rather than a generic superclass (so Gtk.labelNew returns a
-- Gtk.Label, rather than a Gtk.Widget)
fixConstructorReturnType :: Bool -> Name -> Callable -> Callable
fixConstructorReturnType returnsGObject cn c = c { returnType = returnType' }
    where
      returnType' = if returnsGObject then
                        Just (TInterface cn)
                    else
                        returnType c

genMethod :: Name -> Method -> ExcCodeGen ()
genMethod cn m@(Method {
                  methodName = mn,
                  methodSymbol = sym,
                  methodCallable = c,
                  methodType = t
                }) = do
    let name' = upperName cn
    returnsGObject <- maybe (return False) isGObject (returnType c)
    line $ "-- method " <> name' <> "::" <> name mn
    line $ "-- method type : " <> tshow t
    let -- Mangle the name to namespace it to the class.
        mn' = mn { name = name cn <> "_" <> name mn }
    let c'  = if Constructor == t
              then fixConstructorReturnType returnsGObject cn c
              else c
        c'' = if OrdinaryMethod == t
              then fixMethodArgs c'
              else c'
    genCCallableWrapper mn' sym c''
    export (NamedSubsection MethodSection $ lowerName mn) (lowerName mn')

    cppIf CPPOverloading $
         genMethodInfo cn (m {methodCallable = c''})

-- | Generate an import for the gvalue getter for the given type. It
-- returns the name of the function on the Haskell side.
genGValueGetter :: Text -> Text -> CodeGen Text
genGValueGetter name' get_value_fn = group $ do
  let symb = "gv_get_" <> get_value_fn
  line $ "foreign import ccall \"" <> get_value_fn <> "\" " <> symb <> " ::"
  indent $ line $ "FP.Ptr B.GValue.GValue -> IO (FP.Ptr " <> name' <> ")"
  return symb

-- | Generate an import for the gvalue setter for the given type. It
-- returns the name of the function on the Haskell side.
genGValueSetter :: Text -> Text -> CodeGen Text
genGValueSetter name' set_value_fn = group $ do
  let symb = "gv_set_" <> set_value_fn
  line $ "foreign import ccall \"" <> set_value_fn <> "\" " <> symb <> " ::"
  indent $ line $ "FP.Ptr B.GValue.GValue -> FP.Ptr " <> name' <> " -> IO ()"
  return symb

-- | Generate the GValue instances for the given GObject.
genGValueInstance :: Name -> Text -> Text -> Text -> Text -> CodeGen ()
genGValueInstance n get_type_fn newFn get_value_fn set_value_fn = do
  let name' = upperName n
      doc = "Convert '" <> name' <> "' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'."

  writeHaddock DocBeforeSymbol doc

  group $ do
    bline $ "instance B.GValue.IsGValue (Maybe " <> name' <> ") where"
    indent $ group $ do
      line $ "gvalueGType_ = " <> get_type_fn
      line $ "gvalueSet_ gv P.Nothing = " <> set_value_fn <> " gv (FP.nullPtr :: FP.Ptr " <> name' <> ")"
      line $ "gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (" <> set_value_fn <> " gv)"
      line $ "gvalueGet_ gv = do"
      indent $ group $ do
        line $ "ptr <- " <> get_value_fn <> " gv :: IO (FP.Ptr " <> name' <> ")"
        line $ "if ptr /= FP.nullPtr"
        line $ "then P.Just <$> " <> newFn <> " " <> name' <> " ptr"
        line $ "else return P.Nothing"

-- | Type casting with type checking, returns the function returning the
-- GType for the oject.
genCasts :: Name -> Text -> [Name] -> CodeGen Text
genCasts n ti parents = do
  isGO <- isGObject (TInterface n)
  let name' = upperName n

  get_type_fn <- do
    let cn_ = "c_" <> ti
    group $ do
      line $ "foreign import ccall \"" <> ti <> "\""
      indent $ line $ cn_ <> " :: IO B.Types.GType"
    return cn_

  group $ do
    bline $ "instance B.Types.TypedObject " <> name' <> " where"
    indent $ do
      line $ "glibType = " <> get_type_fn

  when isGO $ group $ do
      bline $ "instance B.Types.GObject " <> name'

  className <- classConstraint n
  group $ do
    exportDecl className
    writeHaddock DocBeforeSymbol (classDoc name')

    -- Create the IsX constraint. We cannot simply say
    --
    -- > type IsX o = (GObject o, ...)
    --
    -- since we sometimes need to refer to @IsX@ itself, without
    -- applying it. We instead use the trick of creating a class with
    -- a universal instance.
    let constraints = if isGO
                      then "(SP.GObject o, O.IsDescendantOf " <> name' <> " o)"
                      else "(SP.BoxedPtr o, SP.TypedObject o, O.IsDescendantOf " <> name' <> " o)"
    bline $ "class " <> constraints <> " => " <> className <> " o"
    bline $ "instance " <> constraints <> " => " <> className <> " o"

    blank

    parentAPIs <- mapM (\n -> getAPI (TInterface n)) parents
    qualifiedParents <- mapM (uncurry qualifiedAPI) (zip parentAPIs parents)
    bline $ "instance O.HasParentTypes " <> name'
    line $ "type instance O.ParentTypes " <> name' <> " = '["
      <> T.intercalate ", " qualifiedParents <> "]"

  -- Safe downcasting.
  group $ do
    cast <- safeCast n
    exportDecl cast
    writeHaddock DocBeforeSymbol (castDoc name')
    bline $ cast <> " :: (MIO.MonadIO m, " <> className <> " o) => o -> m " <> name'
    line $ cast <> " = MIO.liftIO . B.ManagedPtr.unsafeCastTo " <> name'

  return get_type_fn

  where castDoc :: Text -> Text
        castDoc name' = "Cast to `" <> name' <>
                        "`, for types for which this is known to be safe. " <>
                        "For general casts, use `Data.GI.Base.ManagedPtr.castTo`."

        classDoc :: Text -> Text
        classDoc name' = "Type class for types which can be safely cast to `"
                         <> name' <> "`, for instance with `to" <> name' <> "`."

-- | Wrap a given Object. We enforce that every Object that we wrap is a
-- GObject. This is the case for everything except the ParamSpec* set
-- of objects, we deal with these separately.
genObject :: Name -> Object -> CodeGen ()
genObject n o = do
  let Name _ name' = normalizedAPIName (APIObject o) n
  let t = TInterface n
  isGO <- isGObject t

  writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
  genNewtype name'
  exportDecl (name' <> "(..)")

  addSectionDocumentation ToplevelSection (objDocumentation o)

  -- Type safe casting to parent objects, and implemented interfaces.
  parents <- instanceTree n
  get_type_fn <- genCasts n (objTypeInit o) (parents <> objInterfaces o)

  if isGO
    then genGValueInstance n get_type_fn "B.ManagedPtr.newObject" "B.GValue.get_object" "B.GValue.set_object"
    else case (objGetValueFunc o, objSetValueFunc o) of
           (Just get_value_fn, Just set_value_fn) -> do
             getter <- genGValueGetter name' get_value_fn
             setter <- genGValueSetter name' set_value_fn
             genGValueInstance n get_type_fn "B.ManagedPtr.newPtr" getter setter
           _ -> line $ "--- XXX Missing getter and/or setter, so no GValue instance could be generated."

  cppIf CPPOverloading $
       fullObjectMethodList n o >>= genMethodList n

  if isGO
    then do
      forM_ (objSignals o) $ \s -> genSignal s n

      genObjectProperties n o
      cppIf CPPOverloading $
        genNamespacedPropLabels n (objProperties o) (objMethods o)
      cppIf CPPOverloading $
        genObjectSignals n o
    else group $ do
      let allocInfo = AllocationInfo {
            allocCalloc = AllocationOpUnknown,
            allocCopy = case objRefFunc o of
                          Just ref -> AllocationOp ref
                          Nothing -> AllocationOpUnknown,
            allocFree = case objUnrefFunc o of
                          Just unref -> AllocationOp unref
                          Nothing -> AllocationOpUnknown
            }
      genWrappedPtr n allocInfo 0

  -- Methods
  forM_ (objMethods o) $ \f -> do
    let mn = methodName f
    handleCGExc (\e -> do line ("-- XXX Could not generate method "
                                <> name' <> "::" <> name mn)
                          printCGError e
                          cppIf CPPOverloading $
                            genUnsupportedMethodInfo n f)
                (genMethod n f)

genInterface :: Name -> Interface -> CodeGen ()
genInterface n iface = do
  let Name _ name' = normalizedAPIName (APIInterface iface) n

  line $ "-- interface " <> name' <> " "
  writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
  deprecatedPragma name' $ ifDeprecated iface
  genNewtype name'
  exportDecl (name' <> "(..)")

  addSectionDocumentation ToplevelSection (ifDocumentation iface)

  isGO <- apiIsGObject n (APIInterface iface)
  if isGO
  then do
    let cn_ = fromMaybe (error "GObject derived interface without a type!") (ifTypeInit iface)
    gobjectPrereqs <- filterM nameIsGObject (ifPrerequisites iface)
    allParents <- forM gobjectPrereqs $ \p -> (p : ) <$> instanceTree p
    let uniqueParents = nub (concat allParents)
    get_type_fn <- genCasts n cn_ uniqueParents
    genGValueInstance n get_type_fn "B.ManagedPtr.newObject" "B.GValue.get_object" "B.GValue.set_object"

    genInterfaceProperties n iface
    cppIf CPPOverloading $
       genNamespacedPropLabels n (ifProperties iface) (ifMethods iface)

  else group $ do
    cls <- classConstraint n
    exportDecl cls
    writeHaddock DocBeforeSymbol ("Type class for types which implement `"
                                  <> name' <> "`.")

    -- Create the IsX constraint. We cannot simply say
    --
    -- > type IsX o = (ManagedPtrNewtype o, O.IsDescendantOf X o)
    --
    -- since we sometimes need to refer to @IsX@ itself, without
    -- applying it. We instead use the trick of creating a class with
    -- a universal instance.
    let constraints = "(ManagedPtrNewtype o, O.IsDescendantOf " <> name' <> " o)"
    bline $ "class " <> constraints <> " => " <> cls <> " o"
    bline $ "instance " <> constraints <> " => " <> cls <> " o"

    genWrappedPtr n (ifAllocationInfo iface) 0

    when (not . null . ifProperties $ iface) $ group $ do
       comment $ "XXX Skipping property generation for non-GObject interface"

  -- Methods
  cppIf CPPOverloading $
       fullInterfaceMethodList n iface >>= genMethodList n

  forM_ (ifMethods iface) $ \f -> do
      let mn = methodName f
      isFunction <- symbolFromFunction (methodSymbol f)
      unless isFunction $
             handleCGExc
             (\e -> do comment ("XXX Could not generate method "
                                <> name' <> "::" <> name mn)
                       printCGError e
                       cppIf CPPOverloading (genUnsupportedMethodInfo n f))
             (genMethod n f)

  -- Signals
  forM_ (ifSignals iface) $ \s -> handleCGExc
     (\e -> do line $ T.concat ["-- XXX Could not generate signal ", name', "::"
                               , sigName s]
               printCGError e)
     (genSignal s n)

  cppIf CPPOverloading $
     genInterfaceSignals n iface

-- Some type libraries include spurious interface/struct methods,
-- where a method Mod.Foo::func also appears as an ordinary function
-- in the list of APIs. If we find a matching function (without the
-- "moved-to" annotation), we don't generate the method.
--
-- It may be more expedient to keep a map of symbol -> function.
symbolFromFunction :: Text -> CodeGen Bool
symbolFromFunction sym = do
    apis <- getAPIs
    return $ any (hasSymbol sym . snd) $ M.toList apis
    where
        hasSymbol sym1 (APIFunction (Function { fnSymbol = sym2,
                                                fnMovedTo = movedTo })) =
            sym1 == sym2 && movedTo == Nothing
        hasSymbol _ _ = False

genAPI :: Name -> API -> CodeGen ()
genAPI n (APIConst c) = genConstant n c
genAPI n (APIFunction f) = genFunction n f
genAPI n (APIEnum e) = genEnum n e
genAPI n (APIFlags f) = genFlags n f
genAPI n (APICallback c) = genCallback n c
genAPI n (APIStruct s) = genStruct n s
genAPI n (APIUnion u) = genUnion n u
genAPI n (APIObject o) = genObject n o
genAPI n (APIInterface i) = genInterface n i

-- | Generate the code for a given API in the corresponding module.
genAPIModule :: Name -> API -> CodeGen ()
genAPIModule n api = submodule (submoduleLocation n api) $ genAPI n api

genModule' :: M.Map Name API -> CodeGen ()
genModule' apis = do
  mapM_ (uncurry genAPIModule)
    -- We provide these ourselves
    $ filter (not . handWritten)
    -- Some callback types are defined inside structs
    $ map fixAPIStructs
    -- Some APIs contain duplicated fields by mistake, drop
    -- the duplicates.
    $ map dropDuplicatedFields
    $ mapMaybe (traverse dropMovedItems)
    $ M.toList apis

  -- Make sure we generate a "Callbacks" module, since it is imported
  -- by other modules. It is fine if it ends up empty.
  submodule "Callbacks" (return ())
  where
    -- Whether we provide hand-written bindings for the given API,
    -- replacing the ones that would be autogenerated from the
    -- introspection data.
    handWritten :: (Name, API) -> Bool
    handWritten (Name "GLib" "Array", _) = True
    handWritten (Name "GLib" "Error", _) = True
    handWritten (Name "GLib" "HashTable", _) = True
    handWritten (Name "GLib" "List", _) = True
    handWritten (Name "GLib" "SList", _) = True
    handWritten (Name "GLib" "Variant", _) = True
    handWritten (Name "GObject" "Value", _) = True
    handWritten (Name "GObject" "Closure", _) = True
    handWritten _ = False

genModule :: M.Map Name API -> CodeGen ()
genModule apis = do
  -- Reexport Data.GI.Base for convenience (so it does not need to be
  -- imported separately).
  line "import Data.GI.Base"
  exportModule "Data.GI.Base"

  -- Some API symbols are embedded into structures, extract these and
  -- inject them into the set of APIs loaded and being generated.
  let embeddedAPIs = (fixAPIs . M.fromList
                     . concatMap extractCallbacksInStruct
                     . M.toList) apis
  allAPIs <- getAPIs
  let contextAPIs = M.union (fixAPIs allAPIs) embeddedAPIs
      targetAPIs = M.union (fixAPIs apis) embeddedAPIs

  recurseWithAPIs contextAPIs (genModule' targetAPIs)

  where
    fixAPIs :: M.Map Name API -> M.Map Name API
    fixAPIs apis = M.fromList
      -- Try to guess nullability of properties when there is no
      -- nullability info in the GIR.
      $ map guessPropertyNullability
      -- Not every interface providing signals or properties is
      -- correctly annotated as descending from GObject, fix this.
      $ map detectGObject
      -- Make sure that every argument marked as being a
      -- destructor for a user_data argument has an associated
      -- user_data argument.
      $ map checkClosureDestructors
      -- Make sure that the symbols to be generated are valid
      -- Haskell identifiers, when necessary.
      $ map fixSymbolNaming
      $ M.toList apis
