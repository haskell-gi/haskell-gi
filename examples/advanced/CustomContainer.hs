-- | Example of creating a new type, ported from
-- http://ptomato.name/advanced-gtk-techniques/html/custom-container.html
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE CPP #-}

-- Haskell-gi embeds much of the subclassing information on the type
-- level. The following extensions allow us to do the needful.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

module CustomContainer
  ( CustomContainer(..)
  , IsCustomContainer
  , toCustomContainer
  ) where

import Data.GI.Base (GObject, TypedObject(glibType), ManagedPtr(..),
                     unsafeCastTo, withTransient,
                     get, set, new, AttrOp((:=), (:&=)), GType, toGValue)
import Data.GI.Base.Attributes (AttrInfo(..), AttrOpTag(..))
import Data.GI.Base.GValue (GValueConstruct(..))

import Data.GI.Base.GObject (DerivedGObject(..), GObjectClass(..),
                             registerGType, gobjectInstallCIntProperty,
                             gobjectModifyPrivateData, gobjectGetPrivateData )
import Data.GI.Base.GParamSpec (CIntPropertyInfo(..))
import qualified Data.GI.Base.Overloading as O

import Control.Monad (when, forM, forM_, filterM, foldM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Int (Int32)
import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Foreign.Ptr (Ptr)
import Foreign.C (CInt(..))

import GHC.OverloadedLabels as OL

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

-- The basic type definition. This should always have the form
--
-- > newtype X = X (ManagedPtr X)
--
-- for any type that you wish to define.
newtype CustomContainer = CustomContainer (ManagedPtr CustomContainer)

-- Declare that the new type is a GObject, with a type to be
-- registered at runtime. The information on the type will be declared
-- in the 'DerivedGObject' instance below.
instance TypedObject CustomContainer where
  glibType = registerGType CustomContainer

instance GObject CustomContainer

-- We keep the private data of the CustomContainer here.
data CustomContainerPrivate =
  CustomContainerPrivate { ccChildren  :: [Gtk.Widget]
                           -- ^ List of children of the custom container.
                         , ccNColumns  :: Maybe Int
                           -- ^ Number of columns to use when laying
                           -- out children.
                         }

-- Information for the type system. This will be picked up by
-- 'registerGType' above.
instance DerivedGObject CustomContainer where
  -- The parent type.
  type GObjectParentType  CustomContainer = Gtk.Container
  -- Every custom type has associated private data, which can be of
  -- any type.
  type GObjectPrivateData CustomContainer = CustomContainerPrivate

  -- Name of the type we are about to register. Make sure that it does
  -- not clash with an existing type name. See
  -- https://developer.gnome.org/gobject/stable/gtype-conventions.html
  -- for the naming conventions.
  objectTypeName = "HaskellGI-Example-CustomContainer"

  -- This is run when the class is instantiated for the first time,
  -- typically when the first object of this type is created.
  --
  -- This is the place to register custom properties and signals for
  -- the object.
  objectClassInit = customContainerClassInit

  -- This is run for each instance of the type (each new object of
  -- this type being created). The main goal of this function is to
  -- prepare the private data for each object.
  objectInstanceInit = customContainerInstanceInit

-- Our type descends from a parent type, and implements various
-- interfaces (by virtue of descending from its parent type), make
-- sure that the type system knows about that. First we make our type
-- an instance of O.HasParentTypes (having this class leads to better
-- error messages).
instance O.HasParentTypes CustomContainer

-- We declare what our parent types are here. This is a type-level
-- list of every type that we can be safely cast to. This is our
-- direct parent and its ancestors, and includes implemented
-- interfaces too.
type instance O.ParentTypes CustomContainer = Gtk.Container ': O.ParentTypes Gtk.Container

-- The following is a typeclass that encodes whether a given type
-- descends from us.
class (GObject o, O.IsDescendantOf CustomContainer o) => IsCustomContainer o
instance (GObject o, O.IsDescendantOf CustomContainer o) => IsCustomContainer o

-- If we have a type that descends from us, it can be safely cast to
-- our type.
toCustomContainer :: (MonadIO m, IsCustomContainer o) => o -> m CustomContainer
toCustomContainer = liftIO . unsafeCastTo CustomContainer

-- A CInt-valued property allowing to (optionally) fix the number of
-- columns used for layout).
numColumnsProperty :: CIntPropertyInfo CustomContainer
numColumnsProperty =
  CIntPropertyInfo { name   = "num-columns"
                   , nick   = "Number of columns"
                   , defaultValue = 0
                   , minValue = Just 0
                   , maxValue = Nothing
                   , blurb  = "Number of columns to use when laying out the children, or 0 for choosing an automatic value"
                   , setter = setNumColumnsCInt
                   , getter = getNumColumnsCInt
                   , flags  = Nothing
                   }

-- Set the value of the "num-columns" property using a CInt.
setNumColumnsCInt :: CustomContainer -> CInt -> IO ()
setNumColumnsCInt container n =
  setNumColumns container $ if n <= 0
                            then Nothing
                            else Just (fromIntegral n)

-- Set the value of "num-columns" using a Maybe Int.
setNumColumns :: CustomContainer -> Maybe Int -> IO ()
setNumColumns container newValue = do
  oldValue <- ccNColumns <$> gobjectGetPrivateData container
  if newValue == oldValue
    then return ()
    else do
      gobjectModifyPrivateData container
                     (\priv -> priv {ccNColumns = newValue})
      #queueAllocate container

-- Get the value of the "num-columns" property.
getNumColumnsCInt :: CustomContainer -> IO CInt
getNumColumnsCInt container = do
  nCol <- ccNColumns <$> gobjectGetPrivateData container
  case nCol of
    Just n -> return (fromIntegral n)
    Nothing -> return 0

-- Tell the type system about the "num-columns" property, so we can
-- use the overloading syntax.
data NumColumnsAttrInfo

-- This instance encodes the information for the property at the type
-- level.
instance AttrInfo NumColumnsAttrInfo where
  -- This is a list of the actions allowed on the attribute: we can
  -- get, set, and create it when constructing the object.
  type AttrAllowedOps NumColumnsAttrInfo = '[ 'AttrGet, 'AttrSet,
                                              'AttrConstruct ]

  -- For which types can we get/set the attribute. Anything deriving
  -- from CustomContainer will do.
  type AttrBaseTypeConstraint NumColumnsAttrInfo = IsCustomContainer

  -- Which type does 'get' on the property return. By default this is
  -- also the type that 'set' and 'new' accept.
  type AttrGetType NumColumnsAttrInfo = Maybe Int

  -- Text description for the attribute, for use in error messages.
  type AttrLabel NumColumnsAttrInfo = "num-columns"

  -- Type defining the attribute, for use in error messages.
  type AttrOrigin NumColumnsAttrInfo = CustomContainer

  -- Get the value of the attribute.
  attrGet container = ccNColumns <$>
                      gobjectGetPrivateData (coerce container :: CustomContainer)

  -- Set the value of the argument.
  attrSet container val = setNumColumns (coerce container :: CustomContainer) val

  -- Construct a 'GValue' containing the argument, tagged by the
  -- associated property name.
  attrConstruct val = do
    newValue <- case val of
      Just n -> toGValue (fromIntegral n :: CInt)
      Nothing -> toGValue (0 :: CInt)
    return $ GValueConstruct "num-columns" newValue

-- Allow the overloaded attribute syntax to work. In this case we
-- inherit all attributes of our parent type, and add the numColumns
-- property.
instance O.HasAttributeList CustomContainer
type instance O.AttributeList CustomContainer =
  '("numColumns", NumColumnsAttrInfo) ': O.AttributeList Gtk.Container

-- Support overloaded signals. The following says that we support all
-- signals of our parent type.
type instance O.SignalList CustomContainer = O.SignalList Gtk.Container

-- Support for overloaded methods. We support the same as our parent
-- type.
type family ResolveCustomContainerMethod t o where
  ResolveCustomContainerMethod t o = Gtk.ResolveContainerMethod t o

-- Make overloaded labels applied to CustomContainers resolve to
-- methods.
instance (info ~ ResolveCustomContainerMethod t CustomContainer,
          O.MethodInfo info CustomContainer p)
         => OL.IsLabel t (CustomContainer -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

-- This method is run at class creation time.
customContainerClassInit :: GObjectClass -> IO ()
customContainerClassInit klass = do
  -- Override some methods in the widget class...
  withTransient Gtk.WidgetClass (coerce klass) $ \widgetClass -> do
    set widgetClass [ #getPreferredWidth  :&= customContainerGetPreferredWidth
                    , #getPreferredHeight :&= customContainerGetPreferredHeight
                    , #sizeAllocate       :&= customContainerSizeAllocate ]

  -- ... and in the container class.
  withTransient Gtk.ContainerClass (coerce klass) $ \containerClass -> do
    set containerClass [ #childType :&= customContainerChildType
                       , #add       :&= customContainerAdd
                       , #remove    :&= customContainerRemove
                       , #forall    :&= customContainerForall ]

  -- Install the "num-columns" property.
  gobjectInstallCIntProperty klass numColumnsProperty

  return ()

-- This method is run for each created instance.
customContainerInstanceInit :: GObjectClass -> CustomContainer
                            -> IO CustomContainerPrivate
customContainerInstanceInit _klass custom = do
  -- We do not create any window ourselves, let Gtk know this.
  #setHasWindow custom False

  -- We have no children yet, and the we will set the number of
  -- columns dynamically..
  return $ CustomContainerPrivate { ccChildren = []
                                  , ccNColumns = Nothing }

-- Get the children of the container.
getChildren :: CustomContainer -> IO [Gtk.Widget]
getChildren container = ccChildren <$> gobjectGetPrivateData container

getNColumnsAndRows :: CustomContainer -> IO (Int, Int)
getNColumnsAndRows container = do
  visibleChildren <- getChildren container >>= filterM (`get` #visible)
  maybeNCols <- ccNColumns <$> gobjectGetPrivateData container

  let nVisibleChildren = length visibleChildren
  if nVisibleChildren > 0
    then let nColumns = case maybeNCols of
               Nothing -> ceiling . sqrt . fromIntegral @_ @Double $ nVisibleChildren
               Just n -> n
             nRows = ceiling (fromIntegral nVisibleChildren / fromIntegral nColumns :: Double)
         in return (nColumns, nRows)
    else return (0,0)

data RequestedSize = RequestedSize { rMinimal :: Int32
                                   , rNatural :: Int32 }

instance Semigroup RequestedSize where
  (RequestedSize rm rn) <> (RequestedSize sm sn) = RequestedSize (rm+sm) (rn+sn)

getGroupSizes :: CustomContainer -> Gtk.Orientation
              -> Maybe (V.Vector RequestedSize) -> Int
              -> IO (V.Vector RequestedSize)
getGroupSizes container orientation maybePerpSizes nGroups = do
  visibleChildren <- getChildren container >>= filterM (`get` #visible)
  childrenSizes <- forM (zip [0..] visibleChildren) $ \(count, child) ->
    if orientation == Gtk.OrientationHorizontal
    then let perpGroupNum = count `div` nGroups
         in case maybePerpSizes of
              Nothing -> #getPreferredWidth child
              Just perpSizes -> #getPreferredWidthForHeight child
                                (rMinimal $ perpSizes!perpGroupNum)
    else let perpGroupNum = count `mod` nGroups
         in case maybePerpSizes of
              Nothing -> #getPreferredHeight child
              Just perpSizes -> #getPreferredHeightForWidth child
                                (rMinimal $ perpSizes!perpGroupNum)

  let initialSizes = V.replicate nGroups (0,0)
      indexedSizes = zip [0..] childrenSizes
      adjustSizes :: Ord a => V.Vector (a,a) -> (Int, (a,a)) -> V.Vector (a,a)
      adjustSizes acc (count, (childMinimal, childNatural)) =
        let groupNum = if orientation ==  Gtk.OrientationHorizontal
                       then count `mod` nGroups
                       else count `div` nGroups
            (prevMin, prevNatural) = acc!groupNum
        in acc // [(groupNum,
                    (max prevMin childMinimal, max prevNatural childNatural))]
      sizes = foldl adjustSizes initialSizes indexedSizes

  return $ V.map (uncurry RequestedSize) sizes

getSize :: CustomContainer -> Gtk.Orientation -> IO RequestedSize
getSize container orientation = do
  borderWidth <- fromIntegral <$> container `get` #borderWidth

  let borderSize = RequestedSize (borderWidth * 2) (borderWidth * 2)

  (nGroups, _) <- getNColumnsAndRows container
  if nGroups == 0
    then return borderSize
    else do
      sizes <- getGroupSizes container orientation Nothing nGroups
      return $ foldl (<>) borderSize sizes

distributeExtraSpace :: CustomContainer -> V.Vector RequestedSize
                     -> Int32 -> Int -> IO (V.Vector RequestedSize)
distributeExtraSpace _container startingSizes extraSpace' nGroups = do
  (extraSpace, sizes) <-
    if extraSpace' > 0
    then do
      requestedSizes <- V.forM startingSizes $ \r ->
        new Gtk.RequestedSize [ #minimumSize := rMinimal r
                              , #naturalSize := rNatural r ]
      (result, resized) <- Gtk.distributeNaturalAllocation extraSpace'
                           (V.toList requestedSizes)
      -- The call to Gtk.distributeNaturalAllocation modifies
      -- 'requestedSizes', convert the results back to
      -- `RequestedSize`.
      newSizes <- forM resized $ \request -> do
        minimal <- request `get` #minimumSize
        natural <- request `get` #naturalSize
        return $ RequestedSize {rMinimal = minimal, rNatural = natural}
      return (result, V.fromList newSizes)
    else return (extraSpace', startingSizes)

  let extraPerGroup = extraSpace `div` fromIntegral nGroups

  return $ foldl (adjustSize extraPerGroup) sizes [0 .. nGroups-1]

  where adjustSize :: Int32 -> V.Vector RequestedSize -> Int
                   -> V.Vector RequestedSize
        adjustSize extraPerGroup sizes count =
          let r = sizes!count
              newMin = (rMinimal r + extraPerGroup)
              r' = r {rMinimal = newMin}
              sizes' = sizes // [(count, r')]
          in if newMin >= 0
             then sizes'
             else -- If the above results in a negative width,
                  -- redistribute pixels from other non-zero columns
                  -- to this one.
               redistribute count (count+1) sizes'

        redistribute :: Int -> Int -> V.Vector RequestedSize
                     -> V.Vector RequestedSize
        redistribute count count2 sizes
          | rMinimal (sizes!count) >= 0 = sizes
          | otherwise = let modCount2 = count2 `mod` fromIntegral nGroups
                in if count2 == count || rMinimal (sizes!count) < 0
                   then redistribute count (modCount2+1) sizes
                   else let r = sizes!count
                            r2 = sizes!count2
                            sizes' = sizes //
                                     [(count, r {rMinimal = rMinimal r  + 1}),
                                      (count2, r2{rMinimal = rMinimal r2 - 1})]
                        in redistribute count (modCount2+1) sizes'


customContainerGetPreferredWidth :: Gtk.Widget -> IO (Int32, Int32)
customContainerGetPreferredWidth container = do
  RequestedSize minimal natural <- getSize (coerce container)
                                   Gtk.OrientationHorizontal
  return (minimal, natural)

customContainerGetPreferredHeight :: Gtk.Widget -> IO (Int32, Int32)
customContainerGetPreferredHeight container = do
  RequestedSize minimal natural <- getSize (coerce container)
                                   Gtk.OrientationVertical
  return (minimal, natural)

customContainerSizeAllocate :: Gtk.Widget -> Gdk.Rectangle -> IO ()
customContainerSizeAllocate containerAsWidget allocation = do
  let container = coerce containerAsWidget :: CustomContainer

  #setAllocation container allocation

  -- Compute the number of columns and rows
  (nColumns, nRows) <- getNColumnsAndRows container
  if nColumns == 0
    then return ()
    else do
      -- Compute how much extra space we need
      borderWidth <- fromIntegral <$> container `get` #borderWidth
      allocWidth <- allocation `get` #width
      allocHeight <- allocation `get` #height

      let extraWidth = allocWidth - 2*borderWidth
          extraHeight = allocHeight - 2*borderWidth

      -- Get the ideal sizes of each column
      widths <- getGroupSizes container Gtk.OrientationHorizontal Nothing
                nColumns

      -- Distribute the extra space per columns
      let remainingWidth = foldl (-) extraWidth (V.toList $ V.map rMinimal widths)
      resizedWidths <- distributeExtraSpace container widths remainingWidth
                       nColumns

      -- Do the same for heights, now that we know widths
      heights <- getGroupSizes container Gtk.OrientationVertical (Just widths)
                 nColumns

      -- Distribute the extra space per rows
      let remainingHeight = foldl (-) extraHeight (V.toList $ V.map rMinimal heights)
      resizedHeights <- distributeExtraSpace container heights remainingHeight
                        nRows

      -- Start positioning the items at the container's origin, less
      -- the border width.
      allocX <- allocation `get` #x
      allocY <- allocation `get` #y
      let x0 = allocX + borderWidth
          y0 = allocY + borderWidth

      visibleChildren <- getChildren container >>= filterM (`get` #visible)

      let allocChild :: (Int32, Int32) -> (Int, Gtk.Widget) -> IO (Int32, Int32)
          allocChild (x,y) (count, child) = do
            let childWidth = rMinimal $ resizedWidths!(count `mod` nColumns)
                childHeight = rMinimal $ resizedHeights!(count `div` nColumns)
            childAllocation <- new Gdk.Rectangle [ #x := x
                                                 , #y := y
                                                 , #width := childWidth
                                                 , #height := childHeight ]
            #sizeAllocate child childAllocation

            if (count+1) `mod` nColumns /= 0
              then return (x + childWidth, y)
              else return (x0, y + childHeight)

      foldM_ allocChild (x0,y0) (zip [0..] visibleChildren)

-- Call the given function for all the container's children.
customContainerForall :: Gtk.Container -> Bool
                      -> Gtk.Callback_WithClosures -> Ptr () -> IO ()
customContainerForall container _includeInternals callback callback_data = do
  children <- getChildren (coerce container)

  forM_ children $ \child -> do
    callback child callback_data

-- Remove a child from the container. The real work is done by
-- widgetUnparent.
customContainerRemove :: Gtk.Container -> Gtk.Widget -> IO ()
customContainerRemove container widget = do
  children <- getChildren (coerce container)
  remaining <- flip filterM children $ \child -> do
    if child /= widget
      then return True
      else do
        was_visible <- get widget #visible
        #unparent widget
        when was_visible (#queueResize container)
        return False
  gobjectModifyPrivateData (coerce container :: CustomContainer)
                  (\private -> private {ccChildren = remaining})

-- Add a child to the container
customContainerAdd :: Gtk.Container -> Gtk.Widget -> IO ()
customContainerAdd container child = do
  gobjectModifyPrivateData (coerce @Gtk.Container @CustomContainer container)
                           (\private -> private {
                               ccChildren = ccChildren private ++ [child]
                               })

  -- Add the child to our list of children. All the real work is done
  -- in widgetSetParent.
  #setParent child container
  visible <- get child #visible
  when visible (#queueResize container)

-- Return the type of children that this container accepts
customContainerChildType :: Gtk.Container -> IO GType
customContainerChildType _ = glibType @Gtk.Widget
