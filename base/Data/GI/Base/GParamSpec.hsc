{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Management of `GParamSpec`s.
module Data.GI.Base.GParamSpec
  ( -- * Memory management
    wrapGParamSpecPtr
  , newGParamSpecFromPtr
  , unrefGParamSpec
  , disownGParamSpec

  -- * GParamSpec building
  , PropertyInfo(..)
  , gParamSpecValue
  , CStringPropertyInfo(..)
  , gParamSpecCString
  , CIntPropertyInfo(..)
  , gParamSpecCInt
  , GParamFlag(..)

  -- * Get\/Set
  , PropGetSetter(..)
  , getGParamSpecGetterSetter
  ) where

import Foreign.C (CInt(..), CString)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.StablePtr (newStablePtr, deRefStablePtr,
                          castStablePtrToPtr, castPtrToStablePtr)
import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.GI.Base.ManagedPtr (newManagedPtr', withManagedPtr,
                                disownManagedPtr,
                                newObject, withTransient)
import Data.GI.Base.BasicConversions (gflagsToWord, withTextCString)
import Data.GI.Base.BasicTypes (GObject, GParamSpec(..),
                                GType(..), IsGFlag, ManagedPtr)
import Data.GI.Base.GQuark (GQuark(..), gQuarkFromString)
import Data.GI.Base.GType (gtypeStablePtr)
import qualified Data.GI.Base.GValue as GV
import Data.GI.Base.GValue (GValue(..), IsGValue(..), take_stablePtr)

#include <glib-object.h>

foreign import ccall "g_param_spec_ref_sink" g_param_spec_ref_sink ::
    Ptr GParamSpec -> IO (Ptr GParamSpec)
foreign import ccall "g_param_spec_ref" g_param_spec_ref ::
    Ptr GParamSpec -> IO (Ptr GParamSpec)
foreign import ccall "g_param_spec_unref" g_param_spec_unref ::
    Ptr GParamSpec -> IO ()
foreign import ccall "&g_param_spec_unref" ptr_to_g_param_spec_unref ::
    FunPtr (Ptr GParamSpec -> IO ())

-- | Take ownership of a ParamSpec passed in 'Ptr'.
wrapGParamSpecPtr :: Ptr GParamSpec -> IO GParamSpec
wrapGParamSpecPtr ptr = do
  void $ g_param_spec_ref_sink ptr
  fPtr <- newManagedPtr' ptr_to_g_param_spec_unref ptr
  return $! GParamSpec fPtr

-- | Construct a Haskell wrapper for the given 'GParamSpec', without
-- assuming ownership.
newGParamSpecFromPtr :: Ptr GParamSpec -> IO GParamSpec
newGParamSpecFromPtr ptr = do
  fPtr <- g_param_spec_ref ptr >>= newManagedPtr' ptr_to_g_param_spec_unref
  return $! GParamSpec fPtr

-- | Remove a reference to the given 'GParamSpec'.
unrefGParamSpec :: GParamSpec -> IO ()
unrefGParamSpec ps = withManagedPtr ps g_param_spec_unref

-- | Disown a `GParamSpec`, i.e. do not longer unref the associated
-- foreign `GParamSpec` when the Haskell `GParamSpec` gets garbage
-- collected.
disownGParamSpec :: GParamSpec -> IO (Ptr GParamSpec)
disownGParamSpec = disownManagedPtr

{- | Flags controlling the behaviour of the the parameters. -}
data GParamFlag = GParamReadable
                 {- ^ the parameter is readable -}
                 | GParamWritable
                 {- ^ the parameter is writable -}
                 | GParamConstruct
                 {- ^ the parameter will be set upon object construction -}
                 | GParamConstructOnly
                 {- ^ the parameter can only be set upon object construction -}
                 | GParamExplicitNotify
                 {- ^ calls to 'GI.GObject.Objects.Object.objectSetProperty' for this
property will not automatically result in a \"notify\" signal being
emitted: the implementation must call
'GI.GObject.Objects.Object.objectNotify' themselves in case the
property actually changes. -}
                 | AnotherGParamFlag Int
                 -- ^ Catch-all for unknown values
                 deriving (Show, Eq)

instance Enum GParamFlag where
    fromEnum GParamReadable = #const G_PARAM_READABLE
    fromEnum GParamWritable = #const G_PARAM_WRITABLE
    fromEnum GParamConstruct = #const G_PARAM_CONSTRUCT
    fromEnum GParamConstructOnly = #const G_PARAM_CONSTRUCT_ONLY
    fromEnum GParamExplicitNotify = #const G_PARAM_EXPLICIT_NOTIFY
    fromEnum (AnotherGParamFlag k) = k

    toEnum (#const G_PARAM_READABLE) = GParamReadable
    toEnum (#const G_PARAM_WRITABLE) = GParamWritable
    toEnum (#const G_PARAM_CONSTRUCT) = GParamConstruct
    toEnum (#const G_PARAM_CONSTRUCT_ONLY) = GParamConstructOnly
    toEnum (#const G_PARAM_EXPLICIT_NOTIFY) = GParamExplicitNotify
    toEnum k = AnotherGParamFlag k

instance Ord GParamFlag where
    compare a b = compare (fromEnum a) (fromEnum b)

instance IsGFlag GParamFlag

-- | Default set of flags when constructing properties.
defaultFlags :: Num a => a
defaultFlags = gflagsToWord [GParamReadable, GParamWritable,
                             GParamExplicitNotify]

-- | Low-level getter and setter for the property.
data PropGetSetter o = PropGetSetter
  { propGetter :: Ptr o -> Ptr GValue -> IO ()
  , propSetter :: Ptr o -> Ptr GValue -> IO ()
  }

-- | The `GQuark` pointing to the setter and getter of the property.
pspecQuark :: IO (GQuark (PropGetSetter o))
pspecQuark = gQuarkFromString "haskell-gi-get-set"

-- | The basic constructor for a GObject. They are all isomorphic.
newtype GObjectConstructor = GObjectConstructor (ManagedPtr GObjectConstructor)

-- | Construct a copy of the object from the given pointer.
objectFromPtr :: forall a o. GObject o => Ptr a -> IO o
objectFromPtr objPtr = newObject @o @o (coerce @_ @(ManagedPtr o -> o) GObjectConstructor) (castPtr objPtr)

-- | Wrap a Haskell getter/setter into a lower level one.
wrapGetSet :: forall o a. (GObject o, IsGValue a) =>
              (o -> IO a)       -- ^ Haskell side getter
           -> (o -> a -> IO ()) -- ^ Haskell side setter
           -> (Ptr GValue -> a -> IO ()) -- ^ Setter for the `GValue`
           -> PropGetSetter o
wrapGetSet getter setter gvalueSetter = PropGetSetter {
  propGetter = \objPtr destPtr -> do
      value <- objectFromPtr objPtr >>= getter
      gvalueSetter destPtr value
  , propSetter = \objPtr newGValuePtr ->
      withTransient GValue newGValuePtr $ \newGValue -> do
        obj <- objectFromPtr objPtr
        value <- GV.fromGValue newGValue
        setter obj value
  }

-- | Information on a property encoding a Haskell value. Note that
-- from the C side this property will appear as an opaque pointer. Use
-- the specialized constructors below for creating properties
-- meaningful from the C side.
--
-- A property name consists of segments consisting of ASCII letters
-- and digits, separated by either the \'-\' or \'_\' character. The
-- first character of a property name must be a letter. Names which
-- violate these rules lead to undefined behaviour.
--
-- When creating and looking up a property, either separator can be
-- used, but they cannot be mixed. Using \'-\' is considerably more
-- efficient and in fact required when using property names as detail
-- strings for signals.
--
-- Beyond the name, properties have two more descriptive strings
-- associated with them, the @nick@, which should be suitable for use
-- as a label for the property in a property editor, and the @blurb@,
-- which should be a somewhat longer description, suitable for e.g. a
-- tooltip. The @nick@ and @blurb@ should ideally be localized.
data PropertyInfo o a = PropertyInfo
  { name    :: Text              -- ^ Identifier for the property.
  , nick    :: Text              -- ^ Identifier for display to the user.
  , blurb   :: Text              -- ^ Description of the property.
  , setter :: o -> a -> IO ()    -- ^ Handler invoked when the
                                 -- property is being set.
  , getter :: o -> IO a          -- ^ Handler that returns the current
                                 -- value of the property.
  , flags   :: Maybe [GParamFlag] -- ^ Set of flags, or `Nothing` for
                                 -- the default set of flags.
  }

foreign import ccall g_param_spec_boxed ::
  CString -> CString -> CString -> GType -> CInt -> IO (Ptr GParamSpec)

-- | Create a `GParamSpec` for a Haskell value.
gParamSpecValue :: forall o a. GObject o => PropertyInfo o a -> IO GParamSpec
gParamSpecValue (PropertyInfo {..}) =
  withTextCString name $ \cname ->
    withTextCString nick $ \cnick ->
      withTextCString blurb $ \cblurb -> do
        pspecPtr <- g_param_spec_boxed cname cnick cblurb
                       gtypeStablePtr
                       (maybe defaultFlags gflagsToWord flags)
        quark <- pspecQuark @o
        gParamSpecSetQData pspecPtr quark
          (PropGetSetter { propGetter = getter', propSetter = setter'})
        wrapGParamSpecPtr pspecPtr
  where
    getter' :: Ptr o -> Ptr GValue -> IO ()
    getter' objPtr destPtr = do
      stablePtr <- objectFromPtr objPtr >>= getter >>= newStablePtr
      take_stablePtr destPtr stablePtr

    setter' :: Ptr o -> (Ptr GValue) -> IO ()
    setter' objPtr gvPtr = withTransient GValue gvPtr $ \gv -> do
      obj <- objectFromPtr objPtr
      val <- GV.fromGValue gv >>= deRefStablePtr
      setter obj val

-- | Information on a property of type `CInt` to be registered. A
-- property name consists of segments consisting of ASCII letters and
-- digits, separated by either the \'-\' or \'_\' character. The first
-- character of a property name must be a letter. Names which violate
-- these rules lead to undefined behaviour.
--
-- When creating and looking up a property, either separator can be
-- used, but they cannot be mixed. Using \'-\' is considerably more
-- efficient and in fact required when using property names as detail
-- strings for signals.
--
-- Beyond the name, properties have two more descriptive strings
-- associated with them, the @nick@, which should be suitable for use
-- as a label for the property in a property editor, and the @blurb@,
-- which should be a somewhat longer description, suitable for e.g. a
-- tooltip. The @nick@ and @blurb@ should ideally be localized.
data CIntPropertyInfo o = CIntPropertyInfo
  { name    :: Text              -- ^ Identifier for the property.
  , nick    :: Text              -- ^ Identifier for display to the user.
  , blurb   :: Text              -- ^ Description of the property.
  , defaultValue :: CInt         -- ^ Default value.
  , setter :: o -> CInt -> IO () -- ^ Handler invoked when the
                                 -- property is being set.
  , getter :: o -> IO CInt       -- ^ Handler that returns the current
                                 -- value of the property.
  , flags   :: Maybe [GParamFlag] -- ^ Set of flags, or `Nothing` for
                                 -- the default set of flags.
  , minValue :: Maybe CInt       -- ^ Minimum value, or `Nothing`,
                                 -- which would be replaced by
                                 -- @MININT@.
  , maxValue :: Maybe CInt       -- ^ Maximum value, or `Nothing`,
                                 -- which would be replaced by
                                 -- @MAXINT@.
  }

foreign import ccall g_param_spec_int ::
   CString -> CString -> CString -> CInt -> CInt -> CInt -> CInt
        -> IO (Ptr GParamSpec)

-- | Create a `GParamSpec` for an integer param.
gParamSpecCInt :: GObject o => CIntPropertyInfo o -> IO GParamSpec
gParamSpecCInt (CIntPropertyInfo {..}) =
  withTextCString name $ \cname ->
    withTextCString nick $ \cnick ->
      withTextCString blurb $ \cblurb -> do
        pspecPtr <- g_param_spec_int cname cnick cblurb
                                     (fromMaybe minBound minValue)
                                     (fromMaybe maxBound maxValue)
                                     defaultValue
                                     (maybe defaultFlags gflagsToWord flags)
        quark <- pspecQuark
        gParamSpecSetQData pspecPtr quark (wrapGetSet getter setter gvalueSet_)
        wrapGParamSpecPtr pspecPtr

-- | Information on a property of type `Text` to be registered. A
-- property name consists of segments consisting of ASCII letters and
-- digits, separated by either the \'-\' or \'_\' character. The first
-- character of a property name must be a letter. Names which violate
-- these rules lead to undefined behaviour.
--
-- When creating and looking up a property, either separator can be
-- used, but they cannot be mixed. Using \'-\' is considerably more
-- efficient and in fact required when using property names as detail
-- strings for signals.
--
-- Beyond the name, properties have two more descriptive strings
-- associated with them, the @nick@, which should be suitable for use
-- as a label for the property in a property editor, and the @blurb@,
-- which should be a somewhat longer description, suitable for e.g. a
-- tooltip. The @nick@ and @blurb@ should ideally be localized.
data CStringPropertyInfo o = CStringPropertyInfo
  { name   :: Text
  , nick   :: Text
  , blurb  :: Text
  , defaultValue :: Maybe Text
  , flags  :: Maybe [GParamFlag]
  , setter :: o -> Maybe Text -> IO ()
  , getter :: o -> IO (Maybe Text)
  }

foreign import ccall g_param_spec_string ::
  CString -> CString -> CString -> CString -> CInt -> IO (Ptr GParamSpec)

-- | Create a `GParamSpec` for a string param.
gParamSpecCString :: GObject o => CStringPropertyInfo o -> IO GParamSpec
gParamSpecCString (CStringPropertyInfo {..}) =
  withTextCString name $ \cname ->
    withTextCString nick $ \cnick ->
      withTextCString blurb $ \cblurb -> do
        pspecPtr <- case defaultValue of
          Nothing -> g_param_spec_string cname cnick cblurb nullPtr
                          (maybe defaultFlags gflagsToWord flags)
          Just value ->
            withTextCString value $ \cdefault ->
              g_param_spec_string cname cnick cblurb cdefault
                    (maybe defaultFlags gflagsToWord flags)
        quark <- pspecQuark
        gParamSpecSetQData pspecPtr quark (wrapGetSet getter setter gvalueSet_)
        wrapGParamSpecPtr pspecPtr

foreign import ccall g_param_spec_set_qdata_full ::
  Ptr GParamSpec -> GQuark a -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()

foreign import ccall "&hs_free_stable_ptr" ptr_to_hs_free_stable_ptr ::
        FunPtr (Ptr a -> IO ())

-- | Set the given user data on the `GParamSpec`.
gParamSpecSetQData :: Ptr GParamSpec -> GQuark a -> a -> IO ()
gParamSpecSetQData pspecPtr quark d = do
  ptr <- newStablePtr d
  g_param_spec_set_qdata_full pspecPtr quark
                              (castStablePtrToPtr ptr)
                              ptr_to_hs_free_stable_ptr

foreign import ccall g_param_spec_get_qdata ::
  Ptr GParamSpec -> GQuark a -> IO (Ptr b)

-- | Get the user data for the given `GQuark` on the `GParamSpec`.
gParamSpecGetQData :: Ptr GParamSpec -> GQuark a -> IO (Maybe a)
gParamSpecGetQData pspecPtr quark = do
  ptr <- g_param_spec_get_qdata pspecPtr quark
  if ptr /= nullPtr
    then Just <$> deRefStablePtr (castPtrToStablePtr ptr)
    else return Nothing

-- | Attempt to get the Haskell setter and getter for the given
-- `GParamSpec`. This will only be possible if the `GParamSpec` was
-- created with one of the functions above, if this is not the case
-- the function will return `Nothing`.
getGParamSpecGetterSetter :: forall o. Ptr GParamSpec ->
                              IO (Maybe (PropGetSetter o))
getGParamSpecGetterSetter pspecPtr = do
  quark <- pspecQuark @o
  gParamSpecGetQData pspecPtr quark
