module GI.CodeGen
    ( genConstant
    , genFunction
    , genModule
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif
import Control.Monad (forM, forM_, when, unless, filterM)
import Data.List (nub)
import Data.Tuple (swap)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)

import Foreign.Storable (sizeOf)
import Foreign.C (CUInt)

import GI.API
import GI.Callable (genCallable)
import GI.Constant (genConstant)
import GI.Code
import GI.GObject
import GI.Inheritance (instanceTree)
import GI.Properties (genInterfaceProperties, genObjectProperties)
import GI.OverloadedSignals (genInterfaceSignals, genObjectSignals)
import GI.Signal (genSignal, genCallback)
import GI.Struct (genStructOrUnionFields, extractCallbacksInStruct,
                  fixAPIStructs, ignoreStruct)
import GI.SymbolNaming (upperName, classConstraint, noName)
import GI.Type
import GI.Util (tshow)

genFunction :: Name -> Function -> CodeGen ()
genFunction n (Function symbol throws callable) =
    submodule "Functions" $ group $ do
      line $ "-- function " <> symbol
      handleCGExc (\e -> line ("-- XXX Could not generate function "
                           <> symbol
                           <> "\n-- Error was : " <> describeCGError e))
                   (genCallable n symbol callable throws)

genBoxedObject :: Name -> Text -> CodeGen ()
genBoxedObject n typeInit = do
  name' <- upperName n

  group $ do
    line $ "foreign import ccall \"" <> typeInit <> "\" c_" <>
            typeInit <> " :: "
    indent $ line "IO GType"
  group $ do
       line $ "instance BoxedObject " <> name' <> " where"
       indent $ line $ "boxedType _ = c_" <> typeInit

  hsBoot $ line $ "instance BoxedObject " <> name' <> " where"

genBoxedEnum :: Name -> Text -> CodeGen ()
genBoxedEnum n typeInit = do
  name' <- upperName n

  group $ do
    line $ "foreign import ccall \"" <> typeInit <> "\" c_" <>
            typeInit <> " :: "
    indent $ line "IO GType"
  group $ do
       line $ "instance BoxedEnum " <> name' <> " where"
       indent $ line $ "boxedEnumType _ = c_" <> typeInit

genEnumOrFlags :: Name -> Enumeration -> ExcCodeGen ()
genEnumOrFlags n@(Name ns name) (Enumeration fields eDomain maybeTypeInit storageBytes isDeprecated) = do
  -- Conversion functions expect enums and flags to map to CUInt,
  -- which we assume to be of 32 bits. Fail early, instead of giving
  -- strange errors at runtime.
  when (sizeOf (0 :: CUInt) /= 4) $
       notImplementedError $ "Unsupported CUInt size: " <> tshow (sizeOf (0 :: CUInt))
  when (storageBytes /= 4) $
       notImplementedError $ "Storage of size /= 4 not supported : " <> tshow storageBytes

  name' <- upperName n
  fields' <- forM fields $ \(fieldName, value) -> do
      n <- upperName $ Name ns (name <> "_" <> fieldName)
      return (n, value)

  line $ deprecatedPragma name' isDeprecated

  group $ do
    exportDecl (name' <> "(..)")
    line $ "data " <> name' <> " = "
    indent $
      case fields' of
        ((fieldName, _value):fs) -> do
          line $ "  " <> fieldName
          forM_ fs $ \(n, _) -> line $ "| " <> n
          line $ "| Another" <> name' <> " Int"
          line "deriving (Show, Eq)"
        _ -> return ()
  group $ do
    line $ "instance Enum " <> name' <> " where"
    indent $ do
            forM_ fields' $ \(n, v) ->
                line $ "fromEnum " <> n <> " = " <> tshow v
            line $ "fromEnum (Another" <> name' <> " k) = k"
    let valueNames = M.toList . M.fromListWith (curry snd) $ map swap fields'
    blank
    indent $ do
            forM_ valueNames $ \(v, n) ->
                line $ "toEnum " <> tshow v <> " = " <> n
            line $ "toEnum k = Another" <> name' <> " k"
  maybe (return ()) (genErrorDomain name') eDomain
  maybe (return ()) (genBoxedEnum n) maybeTypeInit

genEnum :: Name -> Enumeration -> CodeGen ()
genEnum n@(Name _ name) enum = submodule "Enums" $ do
  line $ "-- Enum " <> name

  -- Reexported as-is by GI.Pkg.Types
  setModuleFlags [Reexport, NoTypesImport, NoCallbacksImport]

  handleCGExc (\e -> line $ "-- XXX Could not generate: " <> describeCGError e)
              (genEnumOrFlags n enum)

-- Very similar to enums, but we also declare ourselves as members of
-- the IsGFlag typeclass.
genFlags :: Name -> Flags -> CodeGen ()
genFlags n@(Name _ name) (Flags enum) = submodule "Flags" $ do
  line $ "-- Flags " <> name

  -- Reexported as-is by GI.Pkg.Types
  setModuleFlags [Reexport, NoTypesImport, NoCallbacksImport]

  handleCGExc (\e -> line $ "-- XXX Could not generate: " <> describeCGError e)
              (do
                genEnumOrFlags n enum

                name' <- upperName n
                group $ line $ "instance IsGFlag " <> name')

genErrorDomain :: Text -> Text -> CodeGen ()
genErrorDomain name' domain = do
  group $ do
    line $ "instance GErrorClass " <> name' <> " where"
    indent $ line $
               "gerrorClassDomain _ = \"" <> domain <> "\""
  -- Generate type specific error handling (saves a bit of typing, and
  -- it's clearer to read).
  group $ do
    let catcher = "catch" <> name'
    line $ catcher <> " ::"
    indent $ do
            line   "IO a ->"
            line $ "(" <> name' <> " -> GErrorMessage -> IO a) ->"
            line   "IO a"
    line $ catcher <> " = catchGErrorJustDomain"
  group $ do
    let handler = "handle" <> name'
    line $ handler <> " ::"
    indent $ do
            line $ "(" <> name' <> " -> GErrorMessage -> IO a) ->"
            line   "IO a ->"
            line   "IO a"
    line $ handler <> " = handleGErrorJustDomain"
  exportToplevel ("catch" <> name')
  exportToplevel ("handle" <> name')

genStruct :: Name -> Struct -> CodeGen ()
genStruct n s = unless (ignoreStruct n s) $ do
   name' <- upperName n

   submodule "Structs" $ submodule name' $ do
      let decl = line $ "newtype " <> name' <> " = " <> name' <> " (ForeignPtr " <> name' <> ")"
      hsBoot decl
      decl

      addModuleDocumentation (structDocumentation s)

      when (structIsBoxed s) $
           genBoxedObject n (fromJust $ structTypeInit s)
      exportDecl (name' <> ("(..)"))

      noName name'

      -- Generate code for fields.
      genStructOrUnionFields n (structFields s)

      -- Methods
      forM_ (structMethods s) $ \(mn, f) ->
          do isFunction <- symbolFromFunction (methodSymbol f)
             unless isFunction $
                  handleCGExc
                  (\e -> line ("-- XXX Could not generate method "
                               <> name' <> "::" <> name mn <> "\n"
                               <> "-- Error was : " <> describeCGError e))
                  (genMethod n mn f)

genUnion :: Name -> Union -> CodeGen ()
genUnion n u = do
  name' <- upperName n

  submodule "Unions" $ submodule name' $ do
     let decl = line $ "newtype " <> name' <> " = " <> name' <> " (ForeignPtr " <> name' <> ")"
     hsBoot decl
     decl

     when (unionIsBoxed u) $
          genBoxedObject n (fromJust $ unionTypeInit u)
     exportDecl (name' <> "(..)")

     noName name'

     -- Generate code for fields.
     genStructOrUnionFields n (unionFields u)

     -- Methods
     forM_ (unionMethods u) $ \(mn, f) ->
         do isFunction <- symbolFromFunction (methodSymbol f)
            unless isFunction $
                   handleCGExc
                   (\e -> line ("-- XXX Could not generate method "
                                <> name' <> "::" <> name mn <> "\n"
                                <> "-- Error was : " <> describeCGError e))
                   (genMethod n mn f)

-- Add the implicit object argument to methods of an object.  Since we
-- are prepending an argument we need to adjust the offset of the
-- length arguments of CArrays, and closure and destroyer offsets.
fixMethodArgs :: Name -> Callable -> Callable
fixMethodArgs cn c = c {  args = args' , returnType = returnType' }
    where
      returnType' = fixCArrayLength $ returnType c
      args' = objArg : map (fixDestroyers . fixClosures . fixLengthArg) (args c)

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

      objArg = Arg {
                 argCName = "_obj",
                 argType = TInterface (namespace cn) (name cn),
                 direction = DirectionIn,
                 mayBeNull = False,
                 argScope = ScopeTypeInvalid,
                 argClosure = -1,
                 argDestroy = -1,
                 transfer = TransferNothing }

-- For constructors we want to return the actual type of the object,
-- rather than a generic superclass (so Gtk.labelNew returns a
-- Gtk.Label, rather than a Gtk.Widget)
fixConstructorReturnType :: Bool -> Name -> Callable -> Callable
fixConstructorReturnType returnsGObject cn c = c { returnType = returnType' }
    where
      returnType' = if returnsGObject then
                        TInterface (namespace cn) (name cn)
                    else
                        returnType c

genMethod :: Name -> Name -> Method -> ExcCodeGen ()
genMethod cn mn (Method {
                    methodSymbol = sym,
                    methodCallable = c,
                    methodType = t,
                    methodThrows = throws
                 }) = do
    name' <- upperName cn
    returnsGObject <- isGObject (returnType c)
    line $ "-- method " <> name' <> "::" <> name mn
    line $ "-- method type : " <> tshow t
    let -- Mangle the name to namespace it to the class.
        mn' = mn { name = name cn <> "_" <> name mn }
    let c'  = if Constructor == t
              then fixConstructorReturnType returnsGObject cn c
              else c
        c'' = if OrdinaryMethod == t
              then fixMethodArgs cn c'
              else c'
    genCallable mn' sym c'' throws

-- Type casting with type checking
genGObjectCasts :: Bool -> Name -> Text -> [Name] -> CodeGen ()
genGObjectCasts isIU n cn_ parents = do
  name' <- upperName n
  qualifiedParents <- traverse upperName parents

  group $ do
    line $ "foreign import ccall \"" <> cn_ <> "\""
    indent $ line $ "c_" <> cn_ <> " :: IO GType"

  group $ do
    let parentObjectsType = name' <> "ParentTypes"
    line $ "type instance ParentTypes " <> name' <> " = " <> parentObjectsType
    line $ "type " <> parentObjectsType <> " = '[" <>
         T.intercalate ", " qualifiedParents <> "]"

  group $ do
    bline $ "instance GObject " <> name' <> " where"
    indent $ group $ do
            line $ "gobjectIsInitiallyUnowned _ = " <> tshow isIU
            line $ "gobjectType _ = c_" <> cn_

  let className = classConstraint name'
  group $ do
    exportDecl className
    bline $ "class GObject o => " <> className <> " o"
    bline $ "instance (GObject o, IsDescendantOf " <> name' <> " o) => "
             <> className <> " o"

  -- Safe downcasting.
  group $ do
    let safeCast = "to" <> name'
    exportDecl safeCast
    line $ safeCast <> " :: " <> className <> " o => o -> IO " <> name'
    line $ safeCast <> " = unsafeCastTo " <> name'

-- Wrap a given Object. We enforce that every Object that we wrap is a
-- GObject. This is the case for everything except the ParamSpec* set
-- of objects, we deal with these separately.
genObject :: Name -> Object -> CodeGen ()
genObject n o = do
  name' <- upperName n
  let t = (\(Name ns' n') -> TInterface ns' n') n
  isGO <- isGObject t

  if not isGO
  then line $ "-- APIObject \"" <> name' <>
                "\" does not descend from GObject, it will be ignored."
  else submodule "Objects" $ submodule name' $
       do
         bline $ "newtype " <> name' <> " = " <> name' <> " (ForeignPtr " <> name' <> ")"
         exportDecl (name' <> "(..)")

         -- Type safe casting to parent objects, and implemented interfaces.
         isIU <- isInitiallyUnowned t
         parents <- instanceTree n
         genGObjectCasts isIU n (objTypeInit o) (parents <> objInterfaces o)

         noName name'

         forM_ (objSignals o) $ \s ->
          handleCGExc
          (line . (T.concat ["-- XXX Could not generate signal ", name', "::"
                          , sigName s
                          , "\n", "-- Error was : "] <>) . describeCGError)
          (genSignal s n)

         genObjectProperties n o
         genObjectSignals n o

         -- Methods
         forM_ (objMethods o) $ \(mn, f) ->
           handleCGExc
           (\e -> line ("-- XXX Could not generate method "
                        <> name' <> "::" <> name mn <> "\n"
                        <> "-- Error was : " <> describeCGError e))
           (genMethod n mn f)

genInterface :: Name -> Interface -> CodeGen ()
genInterface n iface = do
  name' <- upperName n

  submodule "Interfaces" $ submodule name' $ do
     line $ "-- interface " <> name' <> " "
     line $ deprecatedPragma name' $ ifDeprecated iface
     bline $ "newtype " <> name' <> " = " <> name' <> " (ForeignPtr " <> name' <> ")"
     exportDecl (name' <> "(..)")

     noName name'

     forM_ (ifSignals iface) $ \s -> handleCGExc
          (line . (T.concat ["-- XXX Could not generate signal ", name', "::"
                          , sigName s
                          , "\n", "-- Error was : "] <>) . describeCGError)
          (genSignal s n)

     genInterfaceProperties n iface
     genInterfaceSignals n iface

     isGO <- apiIsGObject n (APIInterface iface)
     if isGO
     then do
       let cn_ = fromMaybe (error "GObject derived interface without a type!") (ifTypeInit iface)
       isIU <- apiIsInitiallyUnowned n (APIInterface iface)
       gobjectPrereqs <- filterM nameIsGObject (ifPrerequisites iface)
       allParents <- forM gobjectPrereqs $ \p -> (p : ) <$> instanceTree p
       let uniqueParents = nub (concat allParents)
       genGObjectCasts isIU n cn_ uniqueParents

     else group $ do
       let cls = classConstraint name'
       exportDecl cls
       bline $ "class ForeignPtrNewtype a => " <> cls <> " a"
       bline $ "instance (ForeignPtrNewtype o, IsDescendantOf " <> name' <> " o) => " <> cls <> " o"
       let parentObjectsType = name' <> "ParentTypes"
       line $ "type instance ParentTypes " <> name' <> " = " <> parentObjectsType
       line $ "type " <> parentObjectsType <> " = '[]"

     -- Methods
     forM_ (ifMethods iface) $ \(mn, f) -> do
         isFunction <- symbolFromFunction (methodSymbol f)
         unless isFunction $
                handleCGExc
                (\e -> line ("-- XXX Could not generate method "
                             <> name' <> "::" <> name mn <> "\n"
                             <> "-- Error was : " <> describeCGError e))
                (genMethod n mn f)

-- Some type libraries include spurious interface/struct methods,
-- where a method Mod.Foo::func also appears as an ordinary function
-- in the list of APIs. If we find a matching function, we don't
-- generate the method.
--
-- It may be more expedient to keep a map of symbol -> function.
--
-- XXX Maybe the GIR helps here? Sometimes such functions are
-- annotated with the "moved-to".
symbolFromFunction :: Text -> CodeGen Bool
symbolFromFunction sym = do
    apis <- getAPIs
    return $ any (hasSymbol sym . snd) $ M.toList apis
    where
        hasSymbol sym1 (APIFunction (Function { fnSymbol = sym2 })) = sym1 == sym2
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

genModule' :: M.Map Name API -> CodeGen ()
genModule' apis = do
  mapM_ (uncurry genAPI)
            -- We provide these ourselves
          $ filter ((`notElem` [ Name "GLib" "Array"
                               , Name "GLib" "Error"
                               , Name "GLib" "HashTable"
                               , Name "GLib" "List"
                               , Name "GLib" "SList"
                               , Name "GLib" "Variant"
                               , Name "GObject" "Value"
                               , Name "GObject" "Closure"]) . fst)
            -- Some callback types are defined inside structs
          $ map fixAPIStructs
          $ M.toList
          $ apis

  -- Make sure we generate a "Callbacks" module, since it is imported
  -- by other modules. It is fine if it ends up empty.
  submodule "Callbacks" $ return ()

genModule :: M.Map Name API -> CodeGen ()
genModule apis = do
  -- Some API symbols are embedded into structures, extract these and
  -- inject them into the set of APIs loaded and being generated.
  let embeddedAPIs = (M.fromList
                     . concatMap extractCallbacksInStruct
                     . M.toList) apis
  allAPIs <- getAPIs
  recurseWithAPIs (M.union allAPIs embeddedAPIs)
       (genModule' (M.union apis embeddedAPIs))
