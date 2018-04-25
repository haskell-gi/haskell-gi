

{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)

/No description available in the introspection data./
-}

#define ENABLE_OVERLOADING (MIN_VERSION_haskell_gi_overloading(1,0,0) \
       && !defined(__HADDOCK_VERSION__))

module GI.Cairo.Structs.Path
    ( 

-- * Exported types
    Path(..)                                ,
    noPath                                  ,


    ) where

import Data.GI.Base.ShortPrelude
import qualified Data.GI.Base.ShortPrelude as SP
import qualified Data.GI.Base.Overloading as O
import qualified Prelude as P

import qualified Data.GI.Base.Attributes as GI.Attributes
import qualified Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified Data.GI.Base.GError as B.GError
import qualified Data.GI.Base.GVariant as B.GVariant
import qualified Data.GI.Base.GValue as B.GValue
import qualified Data.GI.Base.GParamSpec as B.GParamSpec
import qualified Data.GI.Base.CallStack as B.CallStack
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Foreign.Ptr as FP


-- | Memory-managed wrapper type.
newtype Path = Path (ManagedPtr Path)
-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance WrappedPtr Path where
    wrappedPtrCalloc = return nullPtr
    wrappedPtrCopy = return
    wrappedPtrFree = Nothing

-- | A convenience alias for `Nothing` :: `Maybe` `Path`.
noPath :: Maybe Path
noPath = Nothing


#if ENABLE_OVERLOADING
instance O.HasAttributeList Path
type instance O.AttributeList Path = PathAttributeList
type PathAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if ENABLE_OVERLOADING
type family ResolvePathMethod (t :: Symbol) (o :: *) :: * where
    ResolvePathMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePathMethod t Path, O.MethodInfo info Path p) => O.IsLabelProxy t (Path -> p) where
    fromLabelProxy _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)

#if MIN_VERSION_base(4,9,0)
instance (info ~ ResolvePathMethod t Path, O.MethodInfo info Path p) => O.IsLabel t (Path -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#else
    fromLabel _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#endif
#endif

#endif


