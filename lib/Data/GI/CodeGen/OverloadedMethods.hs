module Data.GI.CodeGen.OverloadedMethods
    ( genMethodList
    , genMethodInfo
    , genUnsupportedMethodInfo
    ) where

import Control.Monad (forM, forM_, when)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Conversions (ExposeClosures(..))
import Data.GI.CodeGen.Callable (callableSignature, Signature(..),
                                 ForeignSymbol(..), fixupCallerAllocates)
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.ModulePath (dotModulePath)
import Data.GI.CodeGen.SymbolNaming (lowerName, upperName, qualifiedSymbol,
                                     moduleLocation, hackageModuleLink)
import Data.GI.CodeGen.Util (ucFirst)

-- | Qualified name for the info for a given method.
methodInfoName :: Name -> Method -> CodeGen e Text
methodInfoName n method =
    let infoName = upperName n <> (ucFirst . lowerName . methodName) method
                   <> "MethodInfo"
    in qualifiedSymbol infoName n

-- | Appropriate instances so overloaded labels are properly resolved.
genMethodResolver :: Text -> CodeGen e ()
genMethodResolver n = do
  addLanguagePragma "TypeApplications"
  group $ do
    line $ "instance (info ~ Resolve" <> n <> "Method t " <> n <> ", "
          <> "O.OverloadedMethod info " <> n <> " p) => OL.IsLabel t ("
          <> n <> " -> p) where"
    line $ "#if MIN_VERSION_base(4,10,0)"
    indent $ line $ "fromLabel = O.overloadedMethod @info"
    line $ "#else"
    indent $ line $ "fromLabel _ = O.overloadedMethod @info"
    line $ "#endif"

  -- The circular instance trick is to avoid the liberal coverage
  -- condition. We should be using DYSFUNCTIONAL pragmas instead, once
  -- those are implemented:
  -- https://github.com/ghc-proposals/ghc-proposals/pull/374
  cppIf (CPPMinVersion "base" (4,13,0)) $ group $ do
    line $ "instance (info ~ Resolve" <> n <> "Method t " <> n <> ", "
          <> "O.OverloadedMethod info " <> n <> " p, "
          <> "R.HasField t " <> n <> " p) => "
          <> "R.HasField t " <> n <> " p where"
    indent $ line $ "getField = O.overloadedMethod @info"

  group $ do
    line $ "instance (info ~ Resolve" <> n <> "Method t " <> n <> ", "
          <> "O.OverloadedMethodInfo info " <> n <> ") => "
          <> "OL.IsLabel t (O.MethodProxy info "
          <> n <> ") where"
    line $ "#if MIN_VERSION_base(4,10,0)"
    indent $ line $ "fromLabel = O.MethodProxy"
    line $ "#else"
    indent $ line $ "fromLabel _ = O.MethodProxy"
    line $ "#endif"

-- | Generate the `MethodList` instance given the list of methods for
-- the given named type. Returns a Haddock comment summarizing the
-- list of methods available.
genMethodList :: Name -> [(Name, Method)] -> CodeGen e ()
genMethodList n methods = do
  let name = upperName n
  let filteredMethods = filter isOrdinaryMethod methods
      gets = filter isGet filteredMethods
      sets = filter isSet filteredMethods
      others = filter (\m -> not (isSet m || isGet m)) filteredMethods
      orderedMethods = others ++ gets ++ sets
  infos <- forM orderedMethods $ \(owner, method) ->
           do mi <- methodInfoName owner method
              return ((lowerName . methodName) method, mi)
  group $ do
    let resolver = "Resolve" <> name <> "Method"
    export (Section MethodSection) resolver
    line $ "type family " <> resolver <> " (t :: Symbol) (o :: DK.Type) :: DK.Type where"
    indent $ forM_ infos $ \(label, info) -> do
        line $ resolver <> " \"" <> label <> "\" o = " <> info
    indent $ line $ resolver <> " l o = O.MethodResolutionFailed l o"

  genMethodResolver name

  docs <- methodListDocumentation others gets sets
  prependSectionFormattedDocs (Section MethodSection) docs

  where isOrdinaryMethod :: (Name, Method) -> Bool
        isOrdinaryMethod (_, m) = methodType m == OrdinaryMethod

        isGet :: (Name, Method) -> Bool
        isGet (_, m) = "get_" `T.isPrefixOf` (name . methodName) m

        isSet :: (Name, Method) -> Bool
        isSet (_, m) = "set_" `T.isPrefixOf` (name . methodName) m

-- | Format a haddock comment with the information about available
-- methods.
methodListDocumentation :: [(Name, Method)] -> [(Name, Method)]
                           -> [(Name, Method)] -> CodeGen e Text
methodListDocumentation [] [] [] = return ""
methodListDocumentation ordinary getters setters = do
  ordinaryFormatted <- formatMethods ordinary
  gettersFormatted <- formatMethods getters
  settersFormatted <- formatMethods setters

  return $ "\n\n === __Click to display all available methods, including inherited ones__\n"
    <> "==== Methods\n" <> ordinaryFormatted
    <> "\n==== Getters\n" <> gettersFormatted
    <> "\n==== Setters\n" <> settersFormatted

  where formatMethods :: [(Name, Method)] -> CodeGen e Text
        formatMethods [] = return "/None/.\n"
        formatMethods methods = do
          qualifiedMethods <- forM methods $ \(owner, m) -> do
            api <- findAPIByName owner
            let mn = lowerName (methodName m)
            return $ "[" <> mn <>
              "](\"" <> dotModulePath (moduleLocation owner api)
              <> "#g:method:" <> mn <> "\")"
          return $ T.intercalate ", " qualifiedMethods <> ".\n"

-- | Treat the instance argument of a method as non-null, even if the
-- introspection data may say otherwise. Returns the modified
-- callable, together with a boolean value indicating where the
-- nullability annotation has been erased.
nonNullableInstanceArg :: Callable -> (Callable, Bool)
nonNullableInstanceArg c = case args c of
  inst:rest -> (c {args = inst {mayBeNull = False} : rest}, mayBeNull inst)
  [] -> (c, False)

-- | Generate the `MethodInfo` type and instance for the given method.
genMethodInfo :: Name -> Method -> ExcCodeGen ()
genMethodInfo n m =
    when (methodType m == OrdinaryMethod) $
      group $ do
        api <- findAPIByName n
        infoName <- methodInfoName n m
        let (callable, nullableInstance) =
              nonNullableInstanceArg . fixupCallerAllocates $ methodCallable m
        sig <- callableSignature callable (KnownForeignSymbol undefined) WithoutClosures
        bline $ "data " <> infoName
        let (obj, otherTypes) = case map snd (signatureArgTypes sig) of
              -- This should not happen, since ordinary methods always
              -- have the instance as first argument.
              [] -> error $ "Internal error: too few parameters! " ++ show m
              (obj':otherTypes') -> (obj', otherTypes')
            sigConstraint = "signature ~ (" <> T.intercalate " -> "
              (otherTypes ++ [signatureReturnType sig]) <> ")"

        hackageLink <- hackageModuleLink n
        let mn = methodName m
            mangled = lowerName (mn {name = name n <> "_" <> name mn})
            dbgInfo = dotModulePath (moduleLocation n api) <> "." <> mangled

        group $ do
          line $ "instance ("
            <> T.intercalate ", " (sigConstraint : signatureConstraints sig)
            <> ") => O.OverloadedMethod " <> infoName <> " " <> obj
            <> " signature where"
          if nullableInstance
            then indent $ line $ "overloadedMethod i = " <> mangled <> " (Just i)"
            else indent $ line $ "overloadedMethod = " <> mangled

        group $ do
          line $ "instance O.OverloadedMethodInfo " <> infoName <> " " <> obj
            <> " where"
          indent $ do
            line $ "overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {"
            indent $ do
              line $ "O.resolvedSymbolName = \"" <> dbgInfo <> "\","
              line $ "O.resolvedSymbolURL = \"" <>
                hackageLink <> "#v:" <> mangled <> "\""
              line $ "})"

        export (NamedSubsection MethodSection $ lowerName mn) infoName

-- | Generate a method info that is not actually callable, but rather
-- gives a type error when trying to use it.
genUnsupportedMethodInfo :: Name -> Method -> CodeGen e ()
genUnsupportedMethodInfo n m = do
  infoName <- methodInfoName n m
  line $ "-- XXX: Dummy instance, since code generation failed.\n"
           <> "-- Please file a bug at http://github.com/haskell-gi/haskell-gi."
  bline $ "data " <> infoName
  group $ do
    line $ "instance (p ~ (), o ~ O.UnsupportedMethodError \""
      <> lowerName (methodName m) <> "\" " <> name n
      <> ") => O.OverloadedMethod " <> infoName <> " o p where"
    indent $ line $ "overloadedMethod = undefined"

  group $ do
    line $ "instance (o ~ O.UnsupportedMethodError \""
      <> lowerName (methodName m) <> "\" " <> name n
      <> ") => O.OverloadedMethodInfo " <> infoName <> " o where"
    indent $ line $ "overloadedMethodInfo = undefined"

  export ToplevelSection infoName
