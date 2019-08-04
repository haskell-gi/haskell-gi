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
import Data.GI.CodeGen.SymbolNaming (lowerName, upperName, qualifiedSymbol)
import Data.GI.CodeGen.Util (ucFirst)

-- | Qualified name for the info for a given method.
methodInfoName :: Name -> Method -> CodeGen Text
methodInfoName n method =
    let infoName = upperName n <> (ucFirst . lowerName . methodName) method
                   <> "MethodInfo"
    in qualifiedSymbol infoName n

-- | Appropriate instances so overloaded labels are properly resolved.
genMethodResolver :: Text -> CodeGen ()
genMethodResolver n = do
  addLanguagePragma "TypeApplications"
  group $ do
    line $ "instance (info ~ Resolve" <> n <> "Method t " <> n <> ", "
          <> "O.MethodInfo info " <> n <> " p) => OL.IsLabel t ("
          <> n <> " -> p) where"
    line $ "#if MIN_VERSION_base(4,10,0)"
    indent $ line $ "fromLabel = O.overloadedMethod @info"
    line $ "#else"
    indent $ line $ "fromLabel _ = O.overloadedMethod @info"
    line $ "#endif"

-- | Generate the `MethodList` instance given the list of methods for
-- the given named type.
genMethodList :: Name -> [(Name, Method)] -> CodeGen ()
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
    export (NamedSubsection MethodSection "Overloaded methods") resolver
    line $ "type family " <> resolver <> " (t :: Symbol) (o :: *) :: * where"
    indent $ forM_ infos $ \(label, info) -> do
        line $ resolver <> " \"" <> label <> "\" o = " <> info
    indent $ line $ resolver <> " l o = O.MethodResolutionFailed l o"

  genMethodResolver name

  where isOrdinaryMethod :: (Name, Method) -> Bool
        isOrdinaryMethod (_, m) = methodType m == OrdinaryMethod

        isGet :: (Name, Method) -> Bool
        isGet (_, m) = "get_" `T.isPrefixOf` (name . methodName) m

        isSet :: (Name, Method) -> Bool
        isSet (_, m) = "set_" `T.isPrefixOf` (name . methodName) m

-- | Generate the `MethodInfo` type and instance for the given method.
genMethodInfo :: Name -> Method -> ExcCodeGen ()
genMethodInfo n m =
    when (methodType m == OrdinaryMethod) $
      group $ do
        infoName <- methodInfoName n m
        let callable = fixupCallerAllocates (methodCallable m)
        sig <- callableSignature callable (KnownForeignSymbol undefined) WithoutClosures
        bline $ "data " <> infoName
        -- This should not happen, since ordinary methods always
        -- have the instance as first argument.
        when (null (signatureArgTypes sig)) $
          error $ "Internal error: too few parameters! " ++ show m
        let (obj:otherTypes) = map snd (signatureArgTypes sig)
            sigConstraint = "signature ~ (" <> T.intercalate " -> "
              (otherTypes ++ [signatureReturnType sig]) <> ")"
        line $ "instance (" <> T.intercalate ", " (sigConstraint :
                                                   signatureConstraints sig)
                 <> ") => O.MethodInfo " <> infoName <> " " <> obj <> " signature where"
        let mn = methodName m
            mangled = lowerName (mn {name = name n <> "_" <> name mn})
        indent $ line $ "overloadedMethod = " <> mangled
        export (NamedSubsection MethodSection $ lowerName mn) infoName

-- | Generate a method info that is not actually callable, but rather
-- gives a type error when trying to use it.
genUnsupportedMethodInfo :: Name -> Method -> CodeGen ()
genUnsupportedMethodInfo n m = do
  infoName <- methodInfoName n m
  line $ "-- XXX: Dummy instance, since code generation failed.\n"
           <> "-- Please file a bug at http://github.com/haskell-gi/haskell-gi."
  bline $ "data " <> infoName
  line $ "instance (p ~ (), o ~ O.UnsupportedMethodError \""
           <> lowerName (methodName m) <> "\" " <> name n
           <> ") => O.MethodInfo " <> infoName <> " o p where"
  indent $ line $ "overloadedMethod = undefined"
  export ToplevelSection infoName
