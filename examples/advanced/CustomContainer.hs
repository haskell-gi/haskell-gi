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

import Data.GI.Base (GObject(..), ManagedPtr(..), unsafeCastTo, withTransient,
                     get, set, new, AttrOp((:=), (:&=)), GType)

import Data.GI.Base.GObject (DerivedGObject(..), GObjectClass(..),
                             registerGType,
                             gobjectModifyPrivateData, gobjectGetPrivateData,
                             gobjectSetPrivateData )
import qualified Data.GI.Base.Overloading as O

import Control.Monad (when, forM_, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)

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
instance GObject CustomContainer where
  gobjectType = registerGType CustomContainer

-- Information for the type system. This will be picked up by
-- 'registerGType' above.
instance DerivedGObject CustomContainer where
  -- The parent type.
  type GObjectParentType  CustomContainer = Gtk.Container
  -- Every custom type has associated private data, which can be of
  -- any type.
  type GObjectPrivateData CustomContainer = [Gtk.Widget]

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

-- Allow the overloaded attribute syntax to work. In this case inherit
-- all attributes of our parent type.
instance O.HasAttributeList CustomContainer
type instance O.AttributeList CustomContainer = O.AttributeList Gtk.Container

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

{-
/* Initialize the PSquare class */
static void
p_square_class_init(PSquareClass *klass)
{
	/* Override GtkWidget methods */
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);
	widget_class->get_preferred_width = p_square_get_preferred_width;
	widget_class->get_preferred_height = p_square_get_preferred_height;
	widget_class->size_allocate = p_square_size_allocate;

	/* Override GtkContainer methods */
	GtkContainerClass *container_class = GTK_CONTAINER_CLASS(klass);
	container_class->child_type = p_square_child_type;
	container_class->add = p_square_add;
	container_class->remove = p_square_remove;
	container_class->forall = p_square_forall;

	/* Add private indirection member */
	g_type_class_add_private(klass, sizeof(PSquarePrivate));
}
-}

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

  return ()

{-
/* Initialize a new PSquare instance */
static void
p_square_init(PSquare *square)
{
	/* This means that PSquare doesn't supply its own GdkWindow */
	gtk_widget_set_has_window(GTK_WIDGET(square), FALSE);
	/* Set redraw on allocate to FALSE if the top left corner of your widget
	 * doesn't change when it's resized; this saves time */
	/*gtk_widget_set_redraw_on_allocate(GTK_WIDGET(square), FALSE);*/

	/* Initialize private members */
	PSquarePrivate *priv = P_SQUARE_PRIVATE(square);
	priv->children = NULL;
}
-}

-- This method is run for each created instance.
customContainerInstanceInit :: GObjectClass -> CustomContainer
                            -> IO [Gtk.Widget]
customContainerInstanceInit _klass custom = do
  -- We do not create any window ourselves, let Gtk know this.
  #setHasWindow custom False

  -- We have no children yet.
  return []

customContainerGetPreferredWidth :: Gtk.Widget -> IO (Int32, Int32)
customContainerGetPreferredWidth _container = return (200, 400)

customContainerGetPreferredHeight :: Gtk.Widget -> IO (Int32, Int32)
customContainerGetPreferredHeight _container = return (200, 400)

customContainerSizeAllocate :: Gtk.Widget -> Gdk.Rectangle -> IO ()
customContainerSizeAllocate container allocation = do
  #setAllocation container allocation

  childAlloc <- new Gdk.Rectangle [ #x := 0, #y := 0,
                                    #width := 100, #height := 100 ]
  children <- gobjectGetPrivateData (coerce @_ @CustomContainer container)

  forM_ children $ \child -> do
    #sizeAllocate child childAlloc

  return ()

{-
/* Call the function for all the container's children. This function
 * ignores the include_internals argument, because there are no
 * "internal" children. */
static void
p_square_forall(GtkContainer *container, gboolean include_internals,
	GtkCallback callback, gpointer callback_data)
{
	PSquarePrivate *priv = P_SQUARE_PRIVATE(container);
	g_list_foreach(priv->children, (GFunc)callback, callback_data);
}
-}

-- Call the given function for all the container's children.
customContainerForall :: Gtk.Container -> Bool
                      -> Gtk.Callback_WithClosures -> Ptr () -> IO ()
customContainerForall container _includeInternals callback callback_data = do
  children <- gobjectGetPrivateData (coerce @_ @CustomContainer container)
  forM_ children $ \child -> do
    callback child callback_data

{-
/* Remove a child from the container */
static void
p_square_remove(GtkContainer *container, GtkWidget *widget)
{
	g_return_if_fail(container || P_IS_SQUARE(container));
	g_return_if_fail(widget || GTK_IS_WIDGET(widget));

	PSquarePrivate *priv = P_SQUARE_PRIVATE(container);

	/* Remove the child from our list of children. 
	 * Again, all the real work is done in gtk_widget_unparent(). */
	GList *link = g_list_find(priv->children, widget);
	if(link) {
		gboolean was_visible = gtk_widget_get_visible(widget);
		gtk_widget_unparent(widget);

		priv->children = g_list_delete_link(priv->children, link);

		/* Queue redraw */
		if(was_visible)
			gtk_widget_queue_resize(GTK_WIDGET(container));
	}
}
-}

-- Remove a child from the container. The real work is done by
-- widgetUnparent.
customContainerRemove :: Gtk.Container -> Gtk.Widget -> IO ()
customContainerRemove container widget = do
  children <- gobjectGetPrivateData (coerce @_ @CustomContainer container)
  remaining <- flip filterM children $ \child -> do
    if child /= widget
      then return True
      else do
        was_visible <- get widget #visible
        #unparent widget
        when was_visible (#queueResize container)
        return False
  gobjectSetPrivateData (coerce @_ @CustomContainer container) remaining

{-
/* Add a child to the container */
static void
p_square_add(GtkContainer *container, GtkWidget *widget)
{
	g_return_if_fail(container || P_IS_SQUARE(container));
	g_return_if_fail(widget || GTK_IS_WIDGET(widget));
	g_return_if_fail(gtk_widget_get_parent(widget) == NULL);

	PSquarePrivate *priv = P_SQUARE_PRIVATE(container);

	/* Add the child to our list of children. 
	 * All the real work is done in gtk_widget_set_parent(). */
	priv->children = g_list_append(priv->children, widget);
	gtk_widget_set_parent(widget, GTK_WIDGET(container));

	/* Queue redraw */
	if(gtk_widget_get_visible(widget))
		gtk_widget_queue_resize(GTK_WIDGET(container));
}
-}

-- Add a child to the container
customContainerAdd :: Gtk.Container -> Gtk.Widget -> IO ()
customContainerAdd container child = do
  gobjectModifyPrivateData (coerce @Gtk.Container @CustomContainer container)
                           (++ [child])

  -- Add the child to our list of children. All the real work is done
  -- in widgetSetParent.
  #setParent child container
  visible <- get child #visible
  when visible (#queueResize container)

{-
/* Return the type of children this container accepts */
static GType
p_square_child_type(GtkContainer *container)
{
	return GTK_TYPE_WIDGET;
}
-}

-- Return the type of children that this container accepts
customContainerChildType :: Gtk.Container -> IO GType
customContainerChildType _ = gobjectType @Gtk.Widget
