module GI.Signal
    ( genSignal
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Data.List (intercalate)
import Data.Typeable (typeOf)

import GI.API
import GI.Callable (hOutType, arrayLengths)
import GI.Code
import GI.Conversions
import GI.SymbolNaming
import GI.Type
import GI.Util (split)
import GI.Internal.ArgInfo

genSignal :: Name -> Signal -> Name -> Object -> CodeGen ()
genSignal sn (Signal { sigCallable = cb }) on _o = do
  on' <- upperName on
  let (w:ws) = split '-' $ name sn
      sn' = w ++ concatMap ucFirst ws
  line $ "-- signal " ++ on' ++ "::" ++ name sn

  let inArgs = filter ((== DirectionIn) . direction) $ args cb
      hInArgs = filter (not . (`elem` (arrayLengths cb))) inArgs
      outArgs = filter ((== DirectionOut) . direction) $ args cb
      hOutArgs = filter (not . (`elem` (arrayLengths cb))) outArgs

  -- Callback prototype
  let cbType = on' ++ ucFirst sn' ++ "Callback"
  group $ do
    line $ "type " ++ cbType ++ " = "
    indent $ do
      -- gtk2hs does not pass the object to the callback, we follow
      -- the same conventions.
      --  t <- haskellType $ TInterface
      -- (namespace on) (name on) line $ show t ++ " ->"
      forM_ hInArgs $ \arg -> do
        ht <- haskellType $ argType arg
        line $ show ht ++ " ->"
      ret <- io <$> hOutType cb hOutArgs False
      line $ show ret

  -- Wrapper for connecting functions to the signal
  -- XXX It would be better to walk through the whole tree and
  -- disambiguate only those that are ambiguous.
  let signalConnectorName = on' ++ ucFirst sn'

  -- We can connect to a signal either before the default handler runs
  -- ("on...") or after the default handler runs (after...). We
  -- provide convenient wrappers for both cases.
  group $ do
    line $ "on" ++ signalConnectorName ++ " obj cb = connect"
             ++ signalConnectorName ++ " obj cb False"
    line $ "after" ++ signalConnectorName ++ " obj cb = connect"
             ++ signalConnectorName ++ " obj cb True"

  group $ do
    let fullName = "connect" ++ signalConnectorName
        signatureConstraints =
          "(ManagedPtr a, GObject a) =>"
        signatureArgs = "a -> " ++ cbType ++ " -> Bool -> IO Word32"
    line $ fullName ++ " :: " ++ signatureConstraints
    line $ replicate (4 + length fullName) ' ' ++ signatureArgs
    line $ fullName ++ " obj cb after = "
    indent $ do
        line $ "connectSignal obj \"" ++ (name sn) ++ "\" cb' after"
        line "where"
    indent $ indent $ do
        line $ "cb' :: Ptr () ->"
        indent $ do forM_ (args cb) $ \arg -> do
                       ft <- marshallFType $ argType arg
                       line $ show ft ++ " ->"
                    ret <- io <$> case returnType cb of
                                    TBasicType TVoid -> return $ typeOf ()
                                    _ -> marshallFType $ returnType cb
                    line $ show ret
        let allNames = map (escapeReserved . argName) (args cb)
        line $ "cb' _ " ++ (concatMap (++ " ") allNames) ++ "= do"
        indent $ do
          inNames <- forM hInArgs $ \arg -> do
                       let name = escapeReserved $ argName arg
                       case argType arg of
                         t@(TCArray False fixed length _) -> do
                           if fixed > -1
                           then convert name $ unpackCArray (show fixed) t (transfer arg)
                           else do
                             let lname = escapeReserved $ argName $
                                         (args cb)!!length
                             convert name $ unpackCArray lname t (transfer arg)
                         _ -> convertFMarshall name (argType arg) (transfer arg)
          let hOutArgNames = map (escapeReserved . argName) hOutArgs
              hRetval = case (returnType cb, hOutArgNames) of
                          (TBasicType TVoid, []) -> ""
                          (TBasicType TVoid, out:[]) -> out ++ " <- "
                          (TBasicType TVoid, _) -> "(" ++ intercalate ", " hOutArgNames ++ ") <- "
                          (_, []) -> "ret <- "
                          _ -> "(" ++ intercalate ", " ("ret":hOutArgNames) ++ ") <- "
          line $ hRetval ++ "cb" ++ (concatMap (" " ++) inNames)
          {-
          -- XXX non-basic type out values are not written back yet.

             forM_ (zip outArgNames outArgs) $ \(name, arg) ->
             case argType arg of
             TBasicType t -> line $ "poke " ++
             n' <- convert (Var name) (hToF $ argType arg)
           -}
          case returnType cb of
            TBasicType TVoid -> line $ "return ()"
            _ -> do
               retval <- convertHMarshall "ret" (returnType cb)
                                                (returnTransfer cb)
               line $ "return " ++ retval
