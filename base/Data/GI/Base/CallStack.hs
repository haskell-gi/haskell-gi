{-# LANGUAGE ImplicitParams, KindSignatures, ConstraintKinds #-}
-- | A compatibility layer for `CallStack`, so that we can have
-- uniform signatures even in old GHC versions (even if the
-- functionality itself does not work there).
module Data.GI.Base.CallStack
  ( HasCallStack
  , prettyCallStack
  , callStack
  ) where

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (HasCallStack, prettyCallStack, callStack)
#elif MIN_VERSION_base(4,8,1)
import Data.List (intercalate)
import qualified GHC.Stack as S
import GHC.SrcLoc (SrcLoc(..))
import GHC.Exts (Constraint)
type HasCallStack = ((?callStack :: S.CallStack) :: Constraint)
type CallStack = [(String, SrcLoc)]
#else
import GHC.Exts (Constraint)
type HasCallStack = (() :: Constraint)
type CallStack = ()
#endif

#if !MIN_VERSION_base(4,9,0)
-- | Return the current `CallStack`.
callStack :: HasCallStack => CallStack
#if MIN_VERSION_base(4,8,1)
callStack = drop 1 (S.getCallStack ?callStack)
#else
callStack = ()
#endif
#endif

#if !MIN_VERSION_base(4,9,0)
prettyCallStack :: CallStack -> String
#if MIN_VERSION_base(4,8,1)
-- | Give a text representation of the current `CallStack`.
prettyCallStack = intercalate "\n" . prettyCallStackLines
  where prettySrcLoc :: SrcLoc -> String
        prettySrcLoc l = foldr (++) "" [ srcLocFile l, ":"
                                       , show (srcLocStartLine l), ":"
                                       , show (srcLocStartCol l), " in "
                                       , srcLocPackage l, ":", srcLocModule l
                                       ]

        prettyCallStackLines :: CallStack -> [String]
        prettyCallStackLines cs = case cs of
          []  -> []
          stk -> "CallStack (from HasCallStack):"
                 : map (("  " ++) . prettyCallSite) stk

        prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc
#else
prettyCallStack _ = "<CallStack only available with GHC >= 7.10.2>"
#endif
#endif
