

{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)

/No description available in the introspection data./
-}

#define ENABLE_OVERLOADING (MIN_VERSION_haskell_gi_overloading(1,0,0) \
       && !defined(__HADDOCK_VERSION__))

module GI.Cairo.Structs.FontFace
    ( 

-- * Exported types
    FontFace(..)                            ,
    noFontFace                              ,


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
newtype FontFace = FontFace (ManagedPtr FontFace)
foreign import ccall "cairo_gobject_font_face_get_type" c_cairo_gobject_font_face_get_type :: 
    IO GType

instance BoxedObject FontFace where
    boxedType _ = c_cairo_gobject_font_face_get_type

-- | A convenience alias for `Nothing` :: `Maybe` `FontFace`.
noFontFace :: Maybe FontFace
noFontFace = Nothing


#if ENABLE_OVERLOADING
instance O.HasAttributeList FontFace
type instance O.AttributeList FontFace = FontFaceAttributeList
type FontFaceAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if ENABLE_OVERLOADING
type family ResolveFontFaceMethod (t :: Symbol) (o :: *) :: * where
    ResolveFontFaceMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFontFaceMethod t FontFace, O.MethodInfo info FontFace p) => O.IsLabelProxy t (FontFace -> p) where
    fromLabelProxy _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)

#if MIN_VERSION_base(4,9,0)
instance (info ~ ResolveFontFaceMethod t FontFace, O.MethodInfo info FontFace p) => O.IsLabel t (FontFace -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#else
    fromLabel _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#endif
#endif

#endif


