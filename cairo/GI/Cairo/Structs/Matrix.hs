

{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)

/No description available in the introspection data./
-}

#define ENABLE_OVERLOADING (MIN_VERSION_haskell_gi_overloading(1,0,0) \
       && !defined(__HADDOCK_VERSION__))

module GI.Cairo.Structs.Matrix
    ( 

-- * Exported types
    Matrix(..)                              ,
    noMatrix                                ,


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
newtype Matrix = Matrix (ManagedPtr Matrix)
foreign import ccall "cairo_gobject_matrix_get_type" c_cairo_gobject_matrix_get_type :: 
    IO GType

instance BoxedObject Matrix where
    boxedType _ = c_cairo_gobject_matrix_get_type

-- | A convenience alias for `Nothing` :: `Maybe` `Matrix`.
noMatrix :: Maybe Matrix
noMatrix = Nothing


#if ENABLE_OVERLOADING
instance O.HasAttributeList Matrix
type instance O.AttributeList Matrix = MatrixAttributeList
type MatrixAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if ENABLE_OVERLOADING
type family ResolveMatrixMethod (t :: Symbol) (o :: *) :: * where
    ResolveMatrixMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMatrixMethod t Matrix, O.MethodInfo info Matrix p) => O.IsLabelProxy t (Matrix -> p) where
    fromLabelProxy _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)

#if MIN_VERSION_base(4,9,0)
instance (info ~ ResolveMatrixMethod t Matrix, O.MethodInfo info Matrix p) => O.IsLabel t (Matrix -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#else
    fromLabel _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#endif
#endif

#endif


