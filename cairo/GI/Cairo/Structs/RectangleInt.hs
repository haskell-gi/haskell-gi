

{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)

/No description available in the introspection data./
-}

#define ENABLE_OVERLOADING (MIN_VERSION_haskell_gi_overloading(1,0,0) \
       && !defined(__HADDOCK_VERSION__))

module GI.Cairo.Structs.RectangleInt
    ( 

-- * Exported types
    RectangleInt(..)                        ,
    newZeroRectangleInt                     ,
    noRectangleInt                          ,


 -- * Properties
-- ** height #attr:height#
{- | /No description available in the introspection data./
-}
    getRectangleIntHeight                   ,
#if ENABLE_OVERLOADING
    rectangleInt_height                     ,
#endif
    setRectangleIntHeight                   ,


-- ** width #attr:width#
{- | /No description available in the introspection data./
-}
    getRectangleIntWidth                    ,
#if ENABLE_OVERLOADING
    rectangleInt_width                      ,
#endif
    setRectangleIntWidth                    ,


-- ** x #attr:x#
{- | /No description available in the introspection data./
-}
    getRectangleIntX                        ,
#if ENABLE_OVERLOADING
    rectangleInt_x                          ,
#endif
    setRectangleIntX                        ,


-- ** y #attr:y#
{- | /No description available in the introspection data./
-}
    getRectangleIntY                        ,
#if ENABLE_OVERLOADING
    rectangleInt_y                          ,
#endif
    setRectangleIntY                        ,




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
newtype RectangleInt = RectangleInt (ManagedPtr RectangleInt)
foreign import ccall "cairo_gobject_rectangle_int_get_type" c_cairo_gobject_rectangle_int_get_type :: 
    IO GType

instance BoxedObject RectangleInt where
    boxedType _ = c_cairo_gobject_rectangle_int_get_type

-- | Construct a `RectangleInt` struct initialized to zero.
newZeroRectangleInt :: MonadIO m => m RectangleInt
newZeroRectangleInt = liftIO $ callocBoxedBytes 16 >>= wrapBoxed RectangleInt

instance tag ~ 'AttrSet => Constructible RectangleInt tag where
    new _ attrs = do
        o <- newZeroRectangleInt
        GI.Attributes.set o attrs
        return o


-- | A convenience alias for `Nothing` :: `Maybe` `RectangleInt`.
noRectangleInt :: Maybe RectangleInt
noRectangleInt = Nothing

{- |
Get the value of the “@x@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.get' rectangleInt #x
@
-}
getRectangleIntX :: MonadIO m => RectangleInt -> m Int32
getRectangleIntX s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO Int32
    return val

{- |
Set the value of the “@x@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.set' rectangleInt [ #x 'Data.GI.Base.Attributes.:=' value ]
@
-}
setRectangleIntX :: MonadIO m => RectangleInt -> Int32 -> m ()
setRectangleIntX s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Int32)

#if ENABLE_OVERLOADING
data RectangleIntXFieldInfo
instance AttrInfo RectangleIntXFieldInfo where
    type AttrAllowedOps RectangleIntXFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RectangleIntXFieldInfo = (~) Int32
    type AttrBaseTypeConstraint RectangleIntXFieldInfo = (~) RectangleInt
    type AttrGetType RectangleIntXFieldInfo = Int32
    type AttrLabel RectangleIntXFieldInfo = "x"
    type AttrOrigin RectangleIntXFieldInfo = RectangleInt
    attrGet _ = getRectangleIntX
    attrSet _ = setRectangleIntX
    attrConstruct = undefined
    attrClear _ = undefined

rectangleInt_x :: AttrLabelProxy "x"
rectangleInt_x = AttrLabelProxy

#endif


{- |
Get the value of the “@y@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.get' rectangleInt #y
@
-}
getRectangleIntY :: MonadIO m => RectangleInt -> m Int32
getRectangleIntY s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 4) :: IO Int32
    return val

{- |
Set the value of the “@y@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.set' rectangleInt [ #y 'Data.GI.Base.Attributes.:=' value ]
@
-}
setRectangleIntY :: MonadIO m => RectangleInt -> Int32 -> m ()
setRectangleIntY s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 4) (val :: Int32)

#if ENABLE_OVERLOADING
data RectangleIntYFieldInfo
instance AttrInfo RectangleIntYFieldInfo where
    type AttrAllowedOps RectangleIntYFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RectangleIntYFieldInfo = (~) Int32
    type AttrBaseTypeConstraint RectangleIntYFieldInfo = (~) RectangleInt
    type AttrGetType RectangleIntYFieldInfo = Int32
    type AttrLabel RectangleIntYFieldInfo = "y"
    type AttrOrigin RectangleIntYFieldInfo = RectangleInt
    attrGet _ = getRectangleIntY
    attrSet _ = setRectangleIntY
    attrConstruct = undefined
    attrClear _ = undefined

rectangleInt_y :: AttrLabelProxy "y"
rectangleInt_y = AttrLabelProxy

#endif


{- |
Get the value of the “@width@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.get' rectangleInt #width
@
-}
getRectangleIntWidth :: MonadIO m => RectangleInt -> m Int32
getRectangleIntWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Int32
    return val

{- |
Set the value of the “@width@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.set' rectangleInt [ #width 'Data.GI.Base.Attributes.:=' value ]
@
-}
setRectangleIntWidth :: MonadIO m => RectangleInt -> Int32 -> m ()
setRectangleIntWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Int32)

#if ENABLE_OVERLOADING
data RectangleIntWidthFieldInfo
instance AttrInfo RectangleIntWidthFieldInfo where
    type AttrAllowedOps RectangleIntWidthFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RectangleIntWidthFieldInfo = (~) Int32
    type AttrBaseTypeConstraint RectangleIntWidthFieldInfo = (~) RectangleInt
    type AttrGetType RectangleIntWidthFieldInfo = Int32
    type AttrLabel RectangleIntWidthFieldInfo = "width"
    type AttrOrigin RectangleIntWidthFieldInfo = RectangleInt
    attrGet _ = getRectangleIntWidth
    attrSet _ = setRectangleIntWidth
    attrConstruct = undefined
    attrClear _ = undefined

rectangleInt_width :: AttrLabelProxy "width"
rectangleInt_width = AttrLabelProxy

#endif


{- |
Get the value of the “@height@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.get' rectangleInt #height
@
-}
getRectangleIntHeight :: MonadIO m => RectangleInt -> m Int32
getRectangleIntHeight s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Int32
    return val

{- |
Set the value of the “@height@” field.
When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to

@
'Data.GI.Base.Attributes.set' rectangleInt [ #height 'Data.GI.Base.Attributes.:=' value ]
@
-}
setRectangleIntHeight :: MonadIO m => RectangleInt -> Int32 -> m ()
setRectangleIntHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Int32)

#if ENABLE_OVERLOADING
data RectangleIntHeightFieldInfo
instance AttrInfo RectangleIntHeightFieldInfo where
    type AttrAllowedOps RectangleIntHeightFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RectangleIntHeightFieldInfo = (~) Int32
    type AttrBaseTypeConstraint RectangleIntHeightFieldInfo = (~) RectangleInt
    type AttrGetType RectangleIntHeightFieldInfo = Int32
    type AttrLabel RectangleIntHeightFieldInfo = "height"
    type AttrOrigin RectangleIntHeightFieldInfo = RectangleInt
    attrGet _ = getRectangleIntHeight
    attrSet _ = setRectangleIntHeight
    attrConstruct = undefined
    attrClear _ = undefined

rectangleInt_height :: AttrLabelProxy "height"
rectangleInt_height = AttrLabelProxy

#endif



#if ENABLE_OVERLOADING
instance O.HasAttributeList RectangleInt
type instance O.AttributeList RectangleInt = RectangleIntAttributeList
type RectangleIntAttributeList = ('[ '("x", RectangleIntXFieldInfo), '("y", RectangleIntYFieldInfo), '("width", RectangleIntWidthFieldInfo), '("height", RectangleIntHeightFieldInfo)] :: [(Symbol, *)])
#endif

#if ENABLE_OVERLOADING
type family ResolveRectangleIntMethod (t :: Symbol) (o :: *) :: * where
    ResolveRectangleIntMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRectangleIntMethod t RectangleInt, O.MethodInfo info RectangleInt p) => O.IsLabelProxy t (RectangleInt -> p) where
    fromLabelProxy _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)

#if MIN_VERSION_base(4,9,0)
instance (info ~ ResolveRectangleIntMethod t RectangleInt, O.MethodInfo info RectangleInt p) => O.IsLabel t (RectangleInt -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#else
    fromLabel _ = O.overloadedMethod (O.MethodProxy :: O.MethodProxy info)
#endif
#endif

#endif


