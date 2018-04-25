

{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)

/No description available in the introspection data./
-}

#define ENABLE_OVERLOADING (MIN_VERSION_haskell_gi_overloading(1,0,0) \
       && !defined(__HADDOCK_VERSION__))

module GI.Cairo.Structs.ScaledFont
    ( 

-- * Exported types
    ScaledFont(..)                          ,
    noScaledFont                            ,


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
newtype ScaledFont = ScaledFont (ManagedPtr ScaledFont)
foreign import ccall "cairo_gobject_scaled_font_get_type" c_cairo_gobject_scaled_font_get_type :: 
    IO GType

instance BoxedObject ScaledFont where
    boxedType _ = c_cairo_gobject_scaled_font_get_type

-- | A convenience alias for `Nothing` :: `Maybe` `ScaledFont`.
noScaledFont :: Maybe ScaledFont
noScaledFont = Nothing


#if ENABLE_OVERLOADING
instance O.HasAttributeList ScaledFont
type instance O.AttributeList ScaledFont = ScaledFontAttributeList
type ScaledFontAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if ENABLE_OVERLOADING
type family ResolveScaledFontMethod (t :: Symbol) (o :: *) :: * where
    ResolveScaledFontMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveScaledFontMethod t ScaledFont, O.MethodInfo info ScaledFont p) => O.IsLabelProxy t (ScaledFont -> p) where
    fromLabelProxy _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)

#if MIN_VERSION_base(4,9,0)
instance (info ~ ResolveScaledFontMethod t ScaledFont, O.MethodInfo info ScaledFont p) => O.IsLabel t (ScaledFont -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#else
    fromLabel _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#endif
#endif

#endif


