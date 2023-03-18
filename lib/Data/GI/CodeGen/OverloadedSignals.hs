module Data.GI.CodeGen.OverloadedSignals
    ( genObjectSignals
    , genInterfaceSignals
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T

import Data.GI.CodeGen.API
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.Inheritance (fullObjectSignalList, fullInterfaceSignalList)
import Data.GI.CodeGen.GObject (apiIsGObject)
import Data.GI.CodeGen.SymbolNaming (upperName, hyphensToCamelCase,
                                     signalInfoName)
import Data.GI.CodeGen.Util (lcFirst)

-- | Signal instances for (GObject-derived) objects.
genObjectSignals :: Name -> Object -> CodeGen e ()
genObjectSignals n o = do
  let name = upperName n
  isGO <- apiIsGObject n (APIObject o)
  when isGO $ do
       infos <- fullObjectSignalList n o >>=
                mapM (\(owner, signal) -> do
                      si <- signalInfoName owner signal
                      return $ "'(\"" <> (lcFirst . hyphensToCamelCase . sigName) signal
                                 <> "\", " <> si <> ")")
       group $ do
         let signalListType = name <> "SignalList"
         line $ "type instance O.SignalList " <> name <> " = " <> signalListType
         line $ "type " <> signalListType <> " = ('[ "
                  <> T.intercalate ", " infos <> "] :: [(Symbol, DK.Type)])"

-- | Signal instances for interfaces.
genInterfaceSignals :: Name -> Interface -> CodeGen e ()
genInterfaceSignals n iface = do
  let name = upperName n
  infos <- fullInterfaceSignalList n iface >>=
           mapM (\(owner, signal) -> do
                   si <- signalInfoName owner signal
                   return $ "'(\"" <> (lcFirst . hyphensToCamelCase . sigName) signal
                              <> "\", " <> si <> ")")
  group $ do
    let signalListType = name <> "SignalList"
    line $ "type instance O.SignalList " <> name <> " = " <> signalListType
    line $ "type " <> signalListType <> " = ('[ "
             <> T.intercalate ", " infos <> "] :: [(Symbol, DK.Type)])"
