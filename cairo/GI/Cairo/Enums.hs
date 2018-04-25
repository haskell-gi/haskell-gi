

{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)
-}

#define ENABLE_OVERLOADING (MIN_VERSION_haskell_gi_overloading(1,0,0) \
       && !defined(__HADDOCK_VERSION__))

module GI.Cairo.Enums
    ( 

 -- * Enumerations
-- ** Antialias #enum:Antialias#

    Antialias(..)                           ,


-- ** Content #enum:Content#

    Content(..)                             ,


-- ** DeviceType #enum:DeviceType#

    DeviceType(..)                          ,


-- ** Extend #enum:Extend#

    Extend(..)                              ,


-- ** FillRule #enum:FillRule#

    FillRule(..)                            ,


-- ** Filter #enum:Filter#

    Filter(..)                              ,


-- ** FontSlant #enum:FontSlant#

    FontSlant(..)                           ,


-- ** FontType #enum:FontType#

    FontType(..)                            ,


-- ** FontWeight #enum:FontWeight#

    FontWeight(..)                          ,


-- ** Format #enum:Format#

    Format(..)                              ,


-- ** HintMetrics #enum:HintMetrics#

    HintMetrics(..)                         ,


-- ** HintStyle #enum:HintStyle#

    HintStyle(..)                           ,


-- ** LineCap #enum:LineCap#

    LineCap(..)                             ,


-- ** LineJoin #enum:LineJoin#

    LineJoin(..)                            ,


-- ** Operator #enum:Operator#

    Operator(..)                            ,


-- ** PathDataType #enum:PathDataType#

    PathDataType(..)                        ,


-- ** PatternType #enum:PatternType#

    PatternType(..)                         ,


-- ** RegionOverlap #enum:RegionOverlap#

    RegionOverlap(..)                       ,


-- ** Status #enum:Status#

    Status(..)                              ,


-- ** SubpixelOrder #enum:SubpixelOrder#

    SubpixelOrder(..)                       ,


-- ** SurfaceType #enum:SurfaceType#

    SurfaceType(..)                         ,


-- ** TextClusterFlags #enum:TextClusterFlags#

    TextClusterFlags(..)                    ,




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


-- Enum TextClusterFlags
{- |
/No description available in the introspection data./
-}
data TextClusterFlags = 
      TextClusterFlagsBackward
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherTextClusterFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextClusterFlags where
    fromEnum TextClusterFlagsBackward = 1
    fromEnum (AnotherTextClusterFlags k) = k

    toEnum 1 = TextClusterFlagsBackward
    toEnum k = AnotherTextClusterFlags k

instance P.Ord TextClusterFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_text_cluster_flags_get_type" c_cairo_gobject_text_cluster_flags_get_type :: 
    IO GType

instance BoxedEnum TextClusterFlags where
    boxedEnumType _ = c_cairo_gobject_text_cluster_flags_get_type

-- Enum SurfaceType
{- |
/No description available in the introspection data./
-}
data SurfaceType = 
      SurfaceTypeImage
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypePdf
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypePs
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeXlib
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeXcb
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeGlitz
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeQuartz
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeWin32
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeBeos
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeDirectfb
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeSvg
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeOs2
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeWin32Printing
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeQuartzImage
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeScript
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeQt
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeRecording
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeVg
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeGl
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeDrm
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeTee
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeXml
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeSkia
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeSubsurface
    {- ^
    /No description available in the introspection data./
    -}
    | SurfaceTypeCogl
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherSurfaceType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SurfaceType where
    fromEnum SurfaceTypeImage = 0
    fromEnum SurfaceTypePdf = 1
    fromEnum SurfaceTypePs = 2
    fromEnum SurfaceTypeXlib = 3
    fromEnum SurfaceTypeXcb = 4
    fromEnum SurfaceTypeGlitz = 5
    fromEnum SurfaceTypeQuartz = 6
    fromEnum SurfaceTypeWin32 = 7
    fromEnum SurfaceTypeBeos = 8
    fromEnum SurfaceTypeDirectfb = 9
    fromEnum SurfaceTypeSvg = 10
    fromEnum SurfaceTypeOs2 = 11
    fromEnum SurfaceTypeWin32Printing = 12
    fromEnum SurfaceTypeQuartzImage = 13
    fromEnum SurfaceTypeScript = 14
    fromEnum SurfaceTypeQt = 15
    fromEnum SurfaceTypeRecording = 16
    fromEnum SurfaceTypeVg = 17
    fromEnum SurfaceTypeGl = 18
    fromEnum SurfaceTypeDrm = 19
    fromEnum SurfaceTypeTee = 20
    fromEnum SurfaceTypeXml = 21
    fromEnum SurfaceTypeSkia = 22
    fromEnum SurfaceTypeSubsurface = 23
    fromEnum SurfaceTypeCogl = 24
    fromEnum (AnotherSurfaceType k) = k

    toEnum 0 = SurfaceTypeImage
    toEnum 1 = SurfaceTypePdf
    toEnum 2 = SurfaceTypePs
    toEnum 3 = SurfaceTypeXlib
    toEnum 4 = SurfaceTypeXcb
    toEnum 5 = SurfaceTypeGlitz
    toEnum 6 = SurfaceTypeQuartz
    toEnum 7 = SurfaceTypeWin32
    toEnum 8 = SurfaceTypeBeos
    toEnum 9 = SurfaceTypeDirectfb
    toEnum 10 = SurfaceTypeSvg
    toEnum 11 = SurfaceTypeOs2
    toEnum 12 = SurfaceTypeWin32Printing
    toEnum 13 = SurfaceTypeQuartzImage
    toEnum 14 = SurfaceTypeScript
    toEnum 15 = SurfaceTypeQt
    toEnum 16 = SurfaceTypeRecording
    toEnum 17 = SurfaceTypeVg
    toEnum 18 = SurfaceTypeGl
    toEnum 19 = SurfaceTypeDrm
    toEnum 20 = SurfaceTypeTee
    toEnum 21 = SurfaceTypeXml
    toEnum 22 = SurfaceTypeSkia
    toEnum 23 = SurfaceTypeSubsurface
    toEnum 24 = SurfaceTypeCogl
    toEnum k = AnotherSurfaceType k

instance P.Ord SurfaceType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_surface_type_get_type" c_cairo_gobject_surface_type_get_type :: 
    IO GType

instance BoxedEnum SurfaceType where
    boxedEnumType _ = c_cairo_gobject_surface_type_get_type

-- Enum SubpixelOrder
{- |
/No description available in the introspection data./
-}
data SubpixelOrder = 
      SubpixelOrderDefault
    {- ^
    /No description available in the introspection data./
    -}
    | SubpixelOrderRgb
    {- ^
    /No description available in the introspection data./
    -}
    | SubpixelOrderBgr
    {- ^
    /No description available in the introspection data./
    -}
    | SubpixelOrderVrgb
    {- ^
    /No description available in the introspection data./
    -}
    | SubpixelOrderVbgr
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherSubpixelOrder Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SubpixelOrder where
    fromEnum SubpixelOrderDefault = 0
    fromEnum SubpixelOrderRgb = 1
    fromEnum SubpixelOrderBgr = 2
    fromEnum SubpixelOrderVrgb = 3
    fromEnum SubpixelOrderVbgr = 4
    fromEnum (AnotherSubpixelOrder k) = k

    toEnum 0 = SubpixelOrderDefault
    toEnum 1 = SubpixelOrderRgb
    toEnum 2 = SubpixelOrderBgr
    toEnum 3 = SubpixelOrderVrgb
    toEnum 4 = SubpixelOrderVbgr
    toEnum k = AnotherSubpixelOrder k

instance P.Ord SubpixelOrder where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_subpixel_order_get_type" c_cairo_gobject_subpixel_order_get_type :: 
    IO GType

instance BoxedEnum SubpixelOrder where
    boxedEnumType _ = c_cairo_gobject_subpixel_order_get_type

-- Enum Status
{- |
/No description available in the introspection data./
-}
data Status = 
      StatusSuccess
    {- ^
    /No description available in the introspection data./
    -}
    | StatusNoMemory
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidRestore
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidPopGroup
    {- ^
    /No description available in the introspection data./
    -}
    | StatusNoCurrentPoint
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidMatrix
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidStatus
    {- ^
    /No description available in the introspection data./
    -}
    | StatusNullPointer
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidString
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidPathData
    {- ^
    /No description available in the introspection data./
    -}
    | StatusReadError
    {- ^
    /No description available in the introspection data./
    -}
    | StatusWriteError
    {- ^
    /No description available in the introspection data./
    -}
    | StatusSurfaceFinished
    {- ^
    /No description available in the introspection data./
    -}
    | StatusSurfaceTypeMismatch
    {- ^
    /No description available in the introspection data./
    -}
    | StatusPatternTypeMismatch
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidContent
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidFormat
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidVisual
    {- ^
    /No description available in the introspection data./
    -}
    | StatusFileNotFound
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidDash
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidDscComment
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidIndex
    {- ^
    /No description available in the introspection data./
    -}
    | StatusClipNotRepresentable
    {- ^
    /No description available in the introspection data./
    -}
    | StatusTempFileError
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidStride
    {- ^
    /No description available in the introspection data./
    -}
    | StatusFontTypeMismatch
    {- ^
    /No description available in the introspection data./
    -}
    | StatusUserFontImmutable
    {- ^
    /No description available in the introspection data./
    -}
    | StatusUserFontError
    {- ^
    /No description available in the introspection data./
    -}
    | StatusNegativeCount
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidClusters
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidSlant
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidWeight
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidSize
    {- ^
    /No description available in the introspection data./
    -}
    | StatusUserFontNotImplemented
    {- ^
    /No description available in the introspection data./
    -}
    | StatusDeviceTypeMismatch
    {- ^
    /No description available in the introspection data./
    -}
    | StatusDeviceError
    {- ^
    /No description available in the introspection data./
    -}
    | StatusInvalidMeshConstruction
    {- ^
    /No description available in the introspection data./
    -}
    | StatusDeviceFinished
    {- ^
    /No description available in the introspection data./
    -}
    | StatusJbig2GlobalMissing
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherStatus Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Status where
    fromEnum StatusSuccess = 0
    fromEnum StatusNoMemory = 1
    fromEnum StatusInvalidRestore = 2
    fromEnum StatusInvalidPopGroup = 3
    fromEnum StatusNoCurrentPoint = 4
    fromEnum StatusInvalidMatrix = 5
    fromEnum StatusInvalidStatus = 6
    fromEnum StatusNullPointer = 7
    fromEnum StatusInvalidString = 8
    fromEnum StatusInvalidPathData = 9
    fromEnum StatusReadError = 10
    fromEnum StatusWriteError = 11
    fromEnum StatusSurfaceFinished = 12
    fromEnum StatusSurfaceTypeMismatch = 13
    fromEnum StatusPatternTypeMismatch = 14
    fromEnum StatusInvalidContent = 15
    fromEnum StatusInvalidFormat = 16
    fromEnum StatusInvalidVisual = 17
    fromEnum StatusFileNotFound = 18
    fromEnum StatusInvalidDash = 19
    fromEnum StatusInvalidDscComment = 20
    fromEnum StatusInvalidIndex = 21
    fromEnum StatusClipNotRepresentable = 22
    fromEnum StatusTempFileError = 23
    fromEnum StatusInvalidStride = 24
    fromEnum StatusFontTypeMismatch = 25
    fromEnum StatusUserFontImmutable = 26
    fromEnum StatusUserFontError = 27
    fromEnum StatusNegativeCount = 28
    fromEnum StatusInvalidClusters = 29
    fromEnum StatusInvalidSlant = 30
    fromEnum StatusInvalidWeight = 31
    fromEnum StatusInvalidSize = 32
    fromEnum StatusUserFontNotImplemented = 33
    fromEnum StatusDeviceTypeMismatch = 34
    fromEnum StatusDeviceError = 35
    fromEnum StatusInvalidMeshConstruction = 36
    fromEnum StatusDeviceFinished = 37
    fromEnum StatusJbig2GlobalMissing = 38
    fromEnum (AnotherStatus k) = k

    toEnum 0 = StatusSuccess
    toEnum 1 = StatusNoMemory
    toEnum 2 = StatusInvalidRestore
    toEnum 3 = StatusInvalidPopGroup
    toEnum 4 = StatusNoCurrentPoint
    toEnum 5 = StatusInvalidMatrix
    toEnum 6 = StatusInvalidStatus
    toEnum 7 = StatusNullPointer
    toEnum 8 = StatusInvalidString
    toEnum 9 = StatusInvalidPathData
    toEnum 10 = StatusReadError
    toEnum 11 = StatusWriteError
    toEnum 12 = StatusSurfaceFinished
    toEnum 13 = StatusSurfaceTypeMismatch
    toEnum 14 = StatusPatternTypeMismatch
    toEnum 15 = StatusInvalidContent
    toEnum 16 = StatusInvalidFormat
    toEnum 17 = StatusInvalidVisual
    toEnum 18 = StatusFileNotFound
    toEnum 19 = StatusInvalidDash
    toEnum 20 = StatusInvalidDscComment
    toEnum 21 = StatusInvalidIndex
    toEnum 22 = StatusClipNotRepresentable
    toEnum 23 = StatusTempFileError
    toEnum 24 = StatusInvalidStride
    toEnum 25 = StatusFontTypeMismatch
    toEnum 26 = StatusUserFontImmutable
    toEnum 27 = StatusUserFontError
    toEnum 28 = StatusNegativeCount
    toEnum 29 = StatusInvalidClusters
    toEnum 30 = StatusInvalidSlant
    toEnum 31 = StatusInvalidWeight
    toEnum 32 = StatusInvalidSize
    toEnum 33 = StatusUserFontNotImplemented
    toEnum 34 = StatusDeviceTypeMismatch
    toEnum 35 = StatusDeviceError
    toEnum 36 = StatusInvalidMeshConstruction
    toEnum 37 = StatusDeviceFinished
    toEnum 38 = StatusJbig2GlobalMissing
    toEnum k = AnotherStatus k

instance P.Ord Status where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_status_get_type" c_cairo_gobject_status_get_type :: 
    IO GType

instance BoxedEnum Status where
    boxedEnumType _ = c_cairo_gobject_status_get_type

-- Enum RegionOverlap
{- |
/No description available in the introspection data./
-}
data RegionOverlap = 
      RegionOverlapIn
    {- ^
    /No description available in the introspection data./
    -}
    | RegionOverlapOut
    {- ^
    /No description available in the introspection data./
    -}
    | RegionOverlapPart
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherRegionOverlap Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RegionOverlap where
    fromEnum RegionOverlapIn = 0
    fromEnum RegionOverlapOut = 1
    fromEnum RegionOverlapPart = 2
    fromEnum (AnotherRegionOverlap k) = k

    toEnum 0 = RegionOverlapIn
    toEnum 1 = RegionOverlapOut
    toEnum 2 = RegionOverlapPart
    toEnum k = AnotherRegionOverlap k

instance P.Ord RegionOverlap where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_region_overlap_get_type" c_cairo_gobject_region_overlap_get_type :: 
    IO GType

instance BoxedEnum RegionOverlap where
    boxedEnumType _ = c_cairo_gobject_region_overlap_get_type

-- Enum PatternType
{- |
/No description available in the introspection data./
-}
data PatternType = 
      PatternTypeSolid
    {- ^
    /No description available in the introspection data./
    -}
    | PatternTypeSurface
    {- ^
    /No description available in the introspection data./
    -}
    | PatternTypeLinear
    {- ^
    /No description available in the introspection data./
    -}
    | PatternTypeRadial
    {- ^
    /No description available in the introspection data./
    -}
    | PatternTypeMesh
    {- ^
    /No description available in the introspection data./
    -}
    | PatternTypeRasterSource
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherPatternType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PatternType where
    fromEnum PatternTypeSolid = 0
    fromEnum PatternTypeSurface = 1
    fromEnum PatternTypeLinear = 2
    fromEnum PatternTypeRadial = 3
    fromEnum PatternTypeMesh = 4
    fromEnum PatternTypeRasterSource = 5
    fromEnum (AnotherPatternType k) = k

    toEnum 0 = PatternTypeSolid
    toEnum 1 = PatternTypeSurface
    toEnum 2 = PatternTypeLinear
    toEnum 3 = PatternTypeRadial
    toEnum 4 = PatternTypeMesh
    toEnum 5 = PatternTypeRasterSource
    toEnum k = AnotherPatternType k

instance P.Ord PatternType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_pattern_type_get_type" c_cairo_gobject_pattern_type_get_type :: 
    IO GType

instance BoxedEnum PatternType where
    boxedEnumType _ = c_cairo_gobject_pattern_type_get_type

-- Enum PathDataType
{- |
/No description available in the introspection data./
-}
data PathDataType = 
      PathDataTypeMoveTo
    {- ^
    /No description available in the introspection data./
    -}
    | PathDataTypeLineTo
    {- ^
    /No description available in the introspection data./
    -}
    | PathDataTypeCurveTo
    {- ^
    /No description available in the introspection data./
    -}
    | PathDataTypeClosePath
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherPathDataType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PathDataType where
    fromEnum PathDataTypeMoveTo = 0
    fromEnum PathDataTypeLineTo = 1
    fromEnum PathDataTypeCurveTo = 2
    fromEnum PathDataTypeClosePath = 3
    fromEnum (AnotherPathDataType k) = k

    toEnum 0 = PathDataTypeMoveTo
    toEnum 1 = PathDataTypeLineTo
    toEnum 2 = PathDataTypeCurveTo
    toEnum 3 = PathDataTypeClosePath
    toEnum k = AnotherPathDataType k

instance P.Ord PathDataType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_path_data_type_get_type" c_cairo_gobject_path_data_type_get_type :: 
    IO GType

instance BoxedEnum PathDataType where
    boxedEnumType _ = c_cairo_gobject_path_data_type_get_type

-- Enum Operator
{- |
/No description available in the introspection data./
-}
data Operator = 
      OperatorClear
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorSource
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorOver
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorIn
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorOut
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorAtop
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDest
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDestOver
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDestIn
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDestOut
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDestAtop
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorXor
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorAdd
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorSaturate
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorMultiply
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorScreen
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorOverlay
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDarken
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorLighten
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorColorDodge
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorColorBurn
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorHardLight
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorSoftLight
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorDifference
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorExclusion
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorHslHue
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorHslSaturation
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorHslColor
    {- ^
    /No description available in the introspection data./
    -}
    | OperatorHslLuminosity
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherOperator Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Operator where
    fromEnum OperatorClear = 0
    fromEnum OperatorSource = 1
    fromEnum OperatorOver = 2
    fromEnum OperatorIn = 3
    fromEnum OperatorOut = 4
    fromEnum OperatorAtop = 5
    fromEnum OperatorDest = 6
    fromEnum OperatorDestOver = 7
    fromEnum OperatorDestIn = 8
    fromEnum OperatorDestOut = 9
    fromEnum OperatorDestAtop = 10
    fromEnum OperatorXor = 11
    fromEnum OperatorAdd = 12
    fromEnum OperatorSaturate = 13
    fromEnum OperatorMultiply = 14
    fromEnum OperatorScreen = 15
    fromEnum OperatorOverlay = 16
    fromEnum OperatorDarken = 17
    fromEnum OperatorLighten = 18
    fromEnum OperatorColorDodge = 19
    fromEnum OperatorColorBurn = 20
    fromEnum OperatorHardLight = 21
    fromEnum OperatorSoftLight = 22
    fromEnum OperatorDifference = 23
    fromEnum OperatorExclusion = 24
    fromEnum OperatorHslHue = 25
    fromEnum OperatorHslSaturation = 26
    fromEnum OperatorHslColor = 27
    fromEnum OperatorHslLuminosity = 28
    fromEnum (AnotherOperator k) = k

    toEnum 0 = OperatorClear
    toEnum 1 = OperatorSource
    toEnum 2 = OperatorOver
    toEnum 3 = OperatorIn
    toEnum 4 = OperatorOut
    toEnum 5 = OperatorAtop
    toEnum 6 = OperatorDest
    toEnum 7 = OperatorDestOver
    toEnum 8 = OperatorDestIn
    toEnum 9 = OperatorDestOut
    toEnum 10 = OperatorDestAtop
    toEnum 11 = OperatorXor
    toEnum 12 = OperatorAdd
    toEnum 13 = OperatorSaturate
    toEnum 14 = OperatorMultiply
    toEnum 15 = OperatorScreen
    toEnum 16 = OperatorOverlay
    toEnum 17 = OperatorDarken
    toEnum 18 = OperatorLighten
    toEnum 19 = OperatorColorDodge
    toEnum 20 = OperatorColorBurn
    toEnum 21 = OperatorHardLight
    toEnum 22 = OperatorSoftLight
    toEnum 23 = OperatorDifference
    toEnum 24 = OperatorExclusion
    toEnum 25 = OperatorHslHue
    toEnum 26 = OperatorHslSaturation
    toEnum 27 = OperatorHslColor
    toEnum 28 = OperatorHslLuminosity
    toEnum k = AnotherOperator k

instance P.Ord Operator where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_operator_get_type" c_cairo_gobject_operator_get_type :: 
    IO GType

instance BoxedEnum Operator where
    boxedEnumType _ = c_cairo_gobject_operator_get_type

-- Enum LineJoin
{- |
/No description available in the introspection data./
-}
data LineJoin = 
      LineJoinMiter
    {- ^
    /No description available in the introspection data./
    -}
    | LineJoinRound
    {- ^
    /No description available in the introspection data./
    -}
    | LineJoinBevel
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherLineJoin Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum LineJoin where
    fromEnum LineJoinMiter = 0
    fromEnum LineJoinRound = 1
    fromEnum LineJoinBevel = 2
    fromEnum (AnotherLineJoin k) = k

    toEnum 0 = LineJoinMiter
    toEnum 1 = LineJoinRound
    toEnum 2 = LineJoinBevel
    toEnum k = AnotherLineJoin k

instance P.Ord LineJoin where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_line_join_get_type" c_cairo_gobject_line_join_get_type :: 
    IO GType

instance BoxedEnum LineJoin where
    boxedEnumType _ = c_cairo_gobject_line_join_get_type

-- Enum LineCap
{- |
/No description available in the introspection data./
-}
data LineCap = 
      LineCapButt
    {- ^
    /No description available in the introspection data./
    -}
    | LineCapRound
    {- ^
    /No description available in the introspection data./
    -}
    | LineCapSquare
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherLineCap Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum LineCap where
    fromEnum LineCapButt = 0
    fromEnum LineCapRound = 1
    fromEnum LineCapSquare = 2
    fromEnum (AnotherLineCap k) = k

    toEnum 0 = LineCapButt
    toEnum 1 = LineCapRound
    toEnum 2 = LineCapSquare
    toEnum k = AnotherLineCap k

instance P.Ord LineCap where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_line_cap_get_type" c_cairo_gobject_line_cap_get_type :: 
    IO GType

instance BoxedEnum LineCap where
    boxedEnumType _ = c_cairo_gobject_line_cap_get_type

-- Enum HintStyle
{- |
/No description available in the introspection data./
-}
data HintStyle = 
      HintStyleDefault
    {- ^
    /No description available in the introspection data./
    -}
    | HintStyleNone
    {- ^
    /No description available in the introspection data./
    -}
    | HintStyleSlight
    {- ^
    /No description available in the introspection data./
    -}
    | HintStyleMedium
    {- ^
    /No description available in the introspection data./
    -}
    | HintStyleFull
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherHintStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum HintStyle where
    fromEnum HintStyleDefault = 0
    fromEnum HintStyleNone = 1
    fromEnum HintStyleSlight = 2
    fromEnum HintStyleMedium = 3
    fromEnum HintStyleFull = 4
    fromEnum (AnotherHintStyle k) = k

    toEnum 0 = HintStyleDefault
    toEnum 1 = HintStyleNone
    toEnum 2 = HintStyleSlight
    toEnum 3 = HintStyleMedium
    toEnum 4 = HintStyleFull
    toEnum k = AnotherHintStyle k

instance P.Ord HintStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_hint_style_get_type" c_cairo_gobject_hint_style_get_type :: 
    IO GType

instance BoxedEnum HintStyle where
    boxedEnumType _ = c_cairo_gobject_hint_style_get_type

-- Enum HintMetrics
{- |
/No description available in the introspection data./
-}
data HintMetrics = 
      HintMetricsDefault
    {- ^
    /No description available in the introspection data./
    -}
    | HintMetricsOff
    {- ^
    /No description available in the introspection data./
    -}
    | HintMetricsOn
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherHintMetrics Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum HintMetrics where
    fromEnum HintMetricsDefault = 0
    fromEnum HintMetricsOff = 1
    fromEnum HintMetricsOn = 2
    fromEnum (AnotherHintMetrics k) = k

    toEnum 0 = HintMetricsDefault
    toEnum 1 = HintMetricsOff
    toEnum 2 = HintMetricsOn
    toEnum k = AnotherHintMetrics k

instance P.Ord HintMetrics where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_hint_metrics_get_type" c_cairo_gobject_hint_metrics_get_type :: 
    IO GType

instance BoxedEnum HintMetrics where
    boxedEnumType _ = c_cairo_gobject_hint_metrics_get_type

-- Enum Format
{- |
/No description available in the introspection data./
-}
data Format = 
      FormatInvalid
    {- ^
    /No description available in the introspection data./
    -}
    | FormatArgb32
    {- ^
    /No description available in the introspection data./
    -}
    | FormatRgb24
    {- ^
    /No description available in the introspection data./
    -}
    | FormatA8
    {- ^
    /No description available in the introspection data./
    -}
    | FormatA1
    {- ^
    /No description available in the introspection data./
    -}
    | FormatRgb16565
    {- ^
    /No description available in the introspection data./
    -}
    | FormatRgb30
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherFormat Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Format where
    fromEnum FormatInvalid = -1
    fromEnum FormatArgb32 = 0
    fromEnum FormatRgb24 = 1
    fromEnum FormatA8 = 2
    fromEnum FormatA1 = 3
    fromEnum FormatRgb16565 = 4
    fromEnum FormatRgb30 = 5
    fromEnum (AnotherFormat k) = k

    toEnum -1 = FormatInvalid
    toEnum 0 = FormatArgb32
    toEnum 1 = FormatRgb24
    toEnum 2 = FormatA8
    toEnum 3 = FormatA1
    toEnum 4 = FormatRgb16565
    toEnum 5 = FormatRgb30
    toEnum k = AnotherFormat k

instance P.Ord Format where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_format_get_type" c_cairo_gobject_format_get_type :: 
    IO GType

instance BoxedEnum Format where
    boxedEnumType _ = c_cairo_gobject_format_get_type

-- Enum FontWeight
{- |
/No description available in the introspection data./
-}
data FontWeight = 
      FontWeightNormal
    {- ^
    /No description available in the introspection data./
    -}
    | FontWeightBold
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherFontWeight Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FontWeight where
    fromEnum FontWeightNormal = 0
    fromEnum FontWeightBold = 1
    fromEnum (AnotherFontWeight k) = k

    toEnum 0 = FontWeightNormal
    toEnum 1 = FontWeightBold
    toEnum k = AnotherFontWeight k

instance P.Ord FontWeight where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_font_weight_get_type" c_cairo_gobject_font_weight_get_type :: 
    IO GType

instance BoxedEnum FontWeight where
    boxedEnumType _ = c_cairo_gobject_font_weight_get_type

-- Enum FontType
{- |
/No description available in the introspection data./
-}
data FontType = 
      FontTypeToy
    {- ^
    /No description available in the introspection data./
    -}
    | FontTypeFt
    {- ^
    /No description available in the introspection data./
    -}
    | FontTypeWin32
    {- ^
    /No description available in the introspection data./
    -}
    | FontTypeQuartz
    {- ^
    /No description available in the introspection data./
    -}
    | FontTypeUser
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherFontType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FontType where
    fromEnum FontTypeToy = 0
    fromEnum FontTypeFt = 1
    fromEnum FontTypeWin32 = 2
    fromEnum FontTypeQuartz = 3
    fromEnum FontTypeUser = 4
    fromEnum (AnotherFontType k) = k

    toEnum 0 = FontTypeToy
    toEnum 1 = FontTypeFt
    toEnum 2 = FontTypeWin32
    toEnum 3 = FontTypeQuartz
    toEnum 4 = FontTypeUser
    toEnum k = AnotherFontType k

instance P.Ord FontType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_font_type_get_type" c_cairo_gobject_font_type_get_type :: 
    IO GType

instance BoxedEnum FontType where
    boxedEnumType _ = c_cairo_gobject_font_type_get_type

-- Enum FontSlant
{- |
/No description available in the introspection data./
-}
data FontSlant = 
      FontSlantNormal
    {- ^
    /No description available in the introspection data./
    -}
    | FontSlantItalic
    {- ^
    /No description available in the introspection data./
    -}
    | FontSlantOblique
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherFontSlant Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FontSlant where
    fromEnum FontSlantNormal = 0
    fromEnum FontSlantItalic = 1
    fromEnum FontSlantOblique = 2
    fromEnum (AnotherFontSlant k) = k

    toEnum 0 = FontSlantNormal
    toEnum 1 = FontSlantItalic
    toEnum 2 = FontSlantOblique
    toEnum k = AnotherFontSlant k

instance P.Ord FontSlant where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_font_slant_get_type" c_cairo_gobject_font_slant_get_type :: 
    IO GType

instance BoxedEnum FontSlant where
    boxedEnumType _ = c_cairo_gobject_font_slant_get_type

-- Enum Filter
{- |
/No description available in the introspection data./
-}
data Filter = 
      FilterFast
    {- ^
    /No description available in the introspection data./
    -}
    | FilterGood
    {- ^
    /No description available in the introspection data./
    -}
    | FilterBest
    {- ^
    /No description available in the introspection data./
    -}
    | FilterNearest
    {- ^
    /No description available in the introspection data./
    -}
    | FilterBilinear
    {- ^
    /No description available in the introspection data./
    -}
    | FilterGaussian
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherFilter Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Filter where
    fromEnum FilterFast = 0
    fromEnum FilterGood = 1
    fromEnum FilterBest = 2
    fromEnum FilterNearest = 3
    fromEnum FilterBilinear = 4
    fromEnum FilterGaussian = 5
    fromEnum (AnotherFilter k) = k

    toEnum 0 = FilterFast
    toEnum 1 = FilterGood
    toEnum 2 = FilterBest
    toEnum 3 = FilterNearest
    toEnum 4 = FilterBilinear
    toEnum 5 = FilterGaussian
    toEnum k = AnotherFilter k

instance P.Ord Filter where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_filter_get_type" c_cairo_gobject_filter_get_type :: 
    IO GType

instance BoxedEnum Filter where
    boxedEnumType _ = c_cairo_gobject_filter_get_type

-- Enum FillRule
{- |
/No description available in the introspection data./
-}
data FillRule = 
      FillRuleWinding
    {- ^
    /No description available in the introspection data./
    -}
    | FillRuleEvenOdd
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherFillRule Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FillRule where
    fromEnum FillRuleWinding = 0
    fromEnum FillRuleEvenOdd = 1
    fromEnum (AnotherFillRule k) = k

    toEnum 0 = FillRuleWinding
    toEnum 1 = FillRuleEvenOdd
    toEnum k = AnotherFillRule k

instance P.Ord FillRule where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_fill_rule_get_type" c_cairo_gobject_fill_rule_get_type :: 
    IO GType

instance BoxedEnum FillRule where
    boxedEnumType _ = c_cairo_gobject_fill_rule_get_type

-- Enum Extend
{- |
/No description available in the introspection data./
-}
data Extend = 
      ExtendNone
    {- ^
    /No description available in the introspection data./
    -}
    | ExtendRepeat
    {- ^
    /No description available in the introspection data./
    -}
    | ExtendReflect
    {- ^
    /No description available in the introspection data./
    -}
    | ExtendPad
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherExtend Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Extend where
    fromEnum ExtendNone = 0
    fromEnum ExtendRepeat = 1
    fromEnum ExtendReflect = 2
    fromEnum ExtendPad = 3
    fromEnum (AnotherExtend k) = k

    toEnum 0 = ExtendNone
    toEnum 1 = ExtendRepeat
    toEnum 2 = ExtendReflect
    toEnum 3 = ExtendPad
    toEnum k = AnotherExtend k

instance P.Ord Extend where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_extend_get_type" c_cairo_gobject_extend_get_type :: 
    IO GType

instance BoxedEnum Extend where
    boxedEnumType _ = c_cairo_gobject_extend_get_type

-- Enum DeviceType
{- |
/No description available in the introspection data./
-}
data DeviceType = 
      DeviceTypeDrm
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeGl
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeScript
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeXcb
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeXlib
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeXml
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeCogl
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeWin32
    {- ^
    /No description available in the introspection data./
    -}
    | DeviceTypeInvalid
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherDeviceType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DeviceType where
    fromEnum DeviceTypeDrm = 0
    fromEnum DeviceTypeGl = 1
    fromEnum DeviceTypeScript = 2
    fromEnum DeviceTypeXcb = 3
    fromEnum DeviceTypeXlib = 4
    fromEnum DeviceTypeXml = 5
    fromEnum DeviceTypeCogl = 6
    fromEnum DeviceTypeWin32 = 7
    fromEnum DeviceTypeInvalid = -1
    fromEnum (AnotherDeviceType k) = k

    toEnum 0 = DeviceTypeDrm
    toEnum 1 = DeviceTypeGl
    toEnum 2 = DeviceTypeScript
    toEnum 3 = DeviceTypeXcb
    toEnum 4 = DeviceTypeXlib
    toEnum 5 = DeviceTypeXml
    toEnum 6 = DeviceTypeCogl
    toEnum 7 = DeviceTypeWin32
    toEnum -1 = DeviceTypeInvalid
    toEnum k = AnotherDeviceType k

instance P.Ord DeviceType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_device_type_get_type" c_cairo_gobject_device_type_get_type :: 
    IO GType

instance BoxedEnum DeviceType where
    boxedEnumType _ = c_cairo_gobject_device_type_get_type

-- Enum Content
{- |
/No description available in the introspection data./
-}
data Content = 
      ContentColor
    {- ^
    /No description available in the introspection data./
    -}
    | ContentAlpha
    {- ^
    /No description available in the introspection data./
    -}
    | ContentColorAlpha
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherContent Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Content where
    fromEnum ContentColor = 4096
    fromEnum ContentAlpha = 8192
    fromEnum ContentColorAlpha = 12288
    fromEnum (AnotherContent k) = k

    toEnum 4096 = ContentColor
    toEnum 8192 = ContentAlpha
    toEnum 12288 = ContentColorAlpha
    toEnum k = AnotherContent k

instance P.Ord Content where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_content_get_type" c_cairo_gobject_content_get_type :: 
    IO GType

instance BoxedEnum Content where
    boxedEnumType _ = c_cairo_gobject_content_get_type

-- Enum Antialias
{- |
/No description available in the introspection data./
-}
data Antialias = 
      AntialiasDefault
    {- ^
    /No description available in the introspection data./
    -}
    | AntialiasNone
    {- ^
    /No description available in the introspection data./
    -}
    | AntialiasGray
    {- ^
    /No description available in the introspection data./
    -}
    | AntialiasSubpixel
    {- ^
    /No description available in the introspection data./
    -}
    | AntialiasFast
    {- ^
    /No description available in the introspection data./
    -}
    | AntialiasGood
    {- ^
    /No description available in the introspection data./
    -}
    | AntialiasBest
    {- ^
    /No description available in the introspection data./
    -}
    | AnotherAntialias Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Antialias where
    fromEnum AntialiasDefault = 0
    fromEnum AntialiasNone = 1
    fromEnum AntialiasGray = 2
    fromEnum AntialiasSubpixel = 3
    fromEnum AntialiasFast = 4
    fromEnum AntialiasGood = 5
    fromEnum AntialiasBest = 6
    fromEnum (AnotherAntialias k) = k

    toEnum 0 = AntialiasDefault
    toEnum 1 = AntialiasNone
    toEnum 2 = AntialiasGray
    toEnum 3 = AntialiasSubpixel
    toEnum 4 = AntialiasFast
    toEnum 5 = AntialiasGood
    toEnum 6 = AntialiasBest
    toEnum k = AnotherAntialias k

instance P.Ord Antialias where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

foreign import ccall "cairo_gobject_antialias_get_type" c_cairo_gobject_antialias_get_type :: 
    IO GType

instance BoxedEnum Antialias where
    boxedEnumType _ = c_cairo_gobject_antialias_get_type


