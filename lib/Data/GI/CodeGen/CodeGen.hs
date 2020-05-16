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
                  genWrappedPtr)
import Data.GI.CodeGen.SymbolNaming (upperName, classConstraint,
                                     submoduleLocation, lowerName, qualifiedAPI)
import Data.GI.CodeGen.Type
import Data.GI.CodeGen.Util (tshow)

-- | Standard derived instances for newtypes wrapping @ManagedPtr@s.
newtypeDeriving :: CodeGen ()
newtypeDeriving = indent $ line $ "deriving (Eq)"

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

-- | Generate the GValue instances for the given GObject.
genBoxedGValueInstance :: Name -> Text -> CodeGen ()
genBoxedGValueInstance n get_type_fn = do
  let name' = upperName n
      doc = "Convert '" <> name' <> "' to and from 'Data.GI.Base.GValue.GValue' with 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'."

  writeHaddock DocBeforeSymbol doc

  group $ do
    bline $ "instance B.GValue.IsGValue " <> name' <> " where"
    indent $ group $ do
      line $ "toGValue o = do"
      indent $ group $ do
        line $ "gtype <- " <> get_type_fn
        line $ "B.ManagedPtr.withManagedPtr o (B.GValue.buildGValue gtype B.GValue.set_boxed)"
      line $ "fromGValue gv = do"
      indent $ group $ do
        line $ "ptr <- B.GValue.get_boxed gv :: IO (Ptr " <> name' <> ")"
        line $ "B.ManagedPtr.newBoxed " <> name' <> " ptr"

genBoxedObject :: Name -> Text -> CodeGen ()
genBoxedObject n typeInit = do
  let name' = upperName n
      get_type_fn = "c_" <> typeInit

  group $ do
    line $ "foreign import ccall \"" <> typeInit <> "\" " <>
            get_type_fn <> " :: "
    indent $ line "IO GType"
  group $ do
       line $ "instance BoxedObject " <> name' <> " where"
       indent $ line $ "boxedType _ = " <> get_type_fn

  genBoxedGValueInstance n get_type_fn

  hsBoot $ line $ "instance BoxedObject " <> name' <> " where"

-- | Generate wrapper for structures.
genStruct :: Name -> Struct -> CodeGen ()
genStruct n s = unless (ignoreStruct n s) $ do
   let name' = upperName n

   writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
   let decl = line $ "newtype " <> name' <> " = " <> name' <> " (ManagedPtr " <> name' <> ")"
   hsBoot decl
   decl
   newtypeDeriving

   addSectionDocumentation ToplevelSection (structDocumentation s)

   if structIsBoxed s
   then genBoxedObject n (fromJust $ structTypeInit s)
   else genWrappedPtr n (structAllocationInfo s) (structSize s)

   exportDecl (name' <> ("(..)"))

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
  let name' = upperName n

  writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
  let decl = line $ "newtype " <> name' <> " = " <> name' <> " (ManagedPtr " <> name' <> ")"
  hsBoot decl
  decl
  newtypeDeriving

  addSectionDocumentation ToplevelSection (unionDocumentation u)

  if unionIsBoxed u
  then genBoxedObject n (fromJust $ unionTypeInit u)
  else genWrappedPtr n (unionAllocationInfo u) (unionSize u)

  exportDecl (name' <> "(..)")

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

-- | Generate the GValue instances for the given GObject.
genGObjectGValueInstance :: Name -> Text -> CodeGen ()
genGObjectGValueInstance n get_type_fn = do
  let name' = upperName n
      doc = "Convert '" <> name' <> "' to and from 'Data.GI.Base.GValue.GValue' with 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'."

  writeHaddock DocBeforeSymbol doc

  group $ do
    bline $ "instance B.GValue.IsGValue " <> name' <> " where"
    indent $ group $ do
      line $ "toGValue o = do"
      indent $ group $ do
        line $ "gtype <- " <> get_type_fn
        line $ "B.ManagedPtr.withManagedPtr o (B.GValue.buildGValue gtype B.GValue.set_object)"
      line $ "fromGValue gv = do"
      indent $ group $ do
        line $ "ptr <- B.GValue.get_object gv :: IO (Ptr " <> name' <> ")"
        line $ "B.ManagedPtr.newObject " <> name' <> " ptr"

-- Type casting with type checking
genGObjectCasts :: Name -> Text -> [Name] -> CodeGen ()
genGObjectCasts n cn_ parents = do
  let name' = upperName n
      get_type_fn = "c_" <> cn_

  group $ do
    line $ "foreign import ccall \"" <> cn_ <> "\""
    indent $ line $ get_type_fn <> " :: IO GType"

  group $ do
    bline $ "instance GObject " <> name' <> " where"
    indent $ group $ do
            line $ "gobjectType = " <> get_type_fn

  genGObjectGValueInstance n get_type_fn

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
    let constraints = "(GObject o, O.IsDescendantOf " <> name' <> " o)"
    bline $ "class " <> constraints <> " => " <> className <> " o"
    bline $ "instance " <> constraints <> " => " <> className <> " o"

    blank

    qualifiedParents <- mapM qualifiedAPI parents
    bline $ "instance O.HasParentTypes " <> name'
    line $ "type instance O.ParentTypes " <> name' <> " = '["
      <> T.intercalate ", " qualifiedParents <> "]"

  -- Safe downcasting.
  group $ do
    let safeCast = "to" <> name'
    exportDecl safeCast
    writeHaddock DocBeforeSymbol (castDoc name')
    line $ safeCast <> " :: (MonadIO m, " <> className <> " o) => o -> m " <> name'
    line $ safeCast <> " = liftIO . unsafeCastTo " <> name'

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
  let name' = upperName n
  let t = TInterface n
  isGO <- isGObject t

  if not isGO
  then line $ "-- APIObject \"" <> name' <>
                "\" does not descend from GObject, it will be ignored."
  else do
    writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
    bline $ "newtype " <> name' <> " = " <> name' <> " (ManagedPtr " <> name' <> ")"
    newtypeDeriving
    exportDecl (name' <> "(..)")

    addSectionDocumentation ToplevelSection (objDocumentation o)

    -- Type safe casting to parent objects, and implemented interfaces.
    parents <- instanceTree n
    genGObjectCasts n (objTypeInit o) (parents <> objInterfaces o)

    cppIf CPPOverloading $
         fullObjectMethodList n o >>= genMethodList n

    forM_ (objSignals o) $ \s -> genSignal s n

    genObjectProperties n o
    cppIf CPPOverloading $
         genNamespacedPropLabels n (objProperties o) (objMethods o)
    cppIf CPPOverloading $
         genObjectSignals n o

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
  let name' = upperName n

  line $ "-- interface " <> name' <> " "
  writeHaddock DocBeforeSymbol ("Memory-managed wrapper type.")
  deprecatedPragma name' $ ifDeprecated iface
  bline $ "newtype " <> name' <> " = " <> name' <> " (ManagedPtr " <> name' <> ")"
  newtypeDeriving
  exportDecl (name' <> "(..)")

  addSectionDocumentation ToplevelSection (ifDocumentation iface)

  forM_ (ifSignals iface) $ \s -> handleCGExc
     (\e -> do line $ T.concat ["-- XXX Could not generate signal ", name', "::"
                               , sigName s]
               printCGError e)
     (genSignal s n)

  cppIf CPPOverloading $
     genInterfaceSignals n iface

  isGO <- apiIsGObject n (APIInterface iface)
  if isGO
  then do
    let cn_ = fromMaybe (error "GObject derived interface without a type!") (ifTypeInit iface)
    gobjectPrereqs <- filterM nameIsGObject (ifPrerequisites iface)
    allParents <- forM gobjectPrereqs $ \p -> (p : ) <$> instanceTree p
    let uniqueParents = nub (concat allParents)
    genGObjectCasts n cn_ uniqueParents

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
          $ filter ((`notElem` [ Name "GLib" "Array"
                               , Name "GLib" "Error"
                               , Name "GLib" "HashTable"
                               , Name "GLib" "List"
                               , Name "GLib" "SList"
                               , Name "GLib" "Variant"
                               , Name "GObject" "Value"
                               , Name "GObject" "Closure"]) . fst)
          $ mapMaybe (traverse dropMovedItems)
            -- Some callback types are defined inside structs
          $ map fixAPIStructs
            -- Try to guess nullability of properties when there is no
            -- nullability info in the GIR.
          $ map guessPropertyNullability
            -- Not every interface providing signals or properties is
            -- correctly annotated as descending from GObject, fix this.
          $ map detectGObject
            -- Some APIs contain duplicated fields by mistake, drop
            -- the duplicates.
          $ map dropDuplicatedFields
            -- Make sure that every argument marked as being a
            -- destructor for a user_data argument has an associated
            -- user_data argument.
          $ map checkClosureDestructors
            -- Make sure that the symbols to be generated are valid
            -- Haskell identifiers, when necessary.
          $ map fixSymbolNaming
          $ M.toList
          $ apis

  -- Make sure we generate a "Callbacks" module, since it is imported
  -- by other modules. It is fine if it ends up empty.
  submodule "Callbacks" (return ())

genModule :: M.Map Name API -> CodeGen ()
genModule apis = do
  -- Reexport Data.GI.Base for convenience (so it does not need to be
  -- imported separately).
  line "import Data.GI.Base"
  exportModule "Data.GI.Base"

  -- Some API symbols are embedded into structures, extract these and
  -- inject them into the set of APIs loaded and being generated.
  let embeddedAPIs = (M.fromList
                     . concatMap extractCallbacksInStruct
                     . M.toList) apis
  allAPIs <- getAPIs
  recurseWithAPIs (M.union allAPIs embeddedAPIs)
       (genModule' (M.union apis embeddedAPIs))
