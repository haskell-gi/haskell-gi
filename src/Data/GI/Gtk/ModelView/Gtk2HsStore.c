#include "Graphics/UI/Gtk/ModelView/Gtk2HsStore.h"

/* #define DEBUG */

#ifdef DEBUG
#define WHEN_DEBUG(a) a
#else
#define WHEN_DEBUG(a)
#endif

static void         gtk2hs_store_init            (Gtk2HsStore      *pkg_tree);
static void         gtk2hs_store_class_init      (Gtk2HsStoreClass *klass);
static void         gtk2hs_store_tree_model_init (GtkTreeModelIface *iface);
static void         gtk2hs_store_tree_sortable_init (GtkTreeSortableIface *iface);
static void         gtk2hs_store_tree_drag_source_init (GtkTreeDragSourceIface *iface);
static void         gtk2hs_store_tree_drag_dest_init (GtkTreeDragDestIface *iface);
static void         gtk2hs_store_finalize        (GObject           *object);
static GtkTreeModelFlags gtk2hs_store_get_flags  (GtkTreeModel      *tree_model);
static gint         gtk2hs_store_get_n_columns   (GtkTreeModel      *tree_model);
static GType        gtk2hs_store_get_column_type (GtkTreeModel      *tree_model,
                                                  gint               index);
static gboolean     gtk2hs_store_get_iter        (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreePath       *path);
static GtkTreePath *gtk2hs_store_get_path        (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static void         gtk2hs_store_get_value       (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  gint               column,
                                                  GValue            *value);
static gboolean     gtk2hs_store_iter_next       (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static gboolean     gtk2hs_store_iter_children   (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreeIter       *parent);
static gboolean     gtk2hs_store_iter_has_child  (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static gint         gtk2hs_store_iter_n_children (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static gboolean     gtk2hs_store_iter_nth_child  (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreeIter       *parent,
                                                  gint               n);
static gboolean     gtk2hs_store_iter_parent     (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreeIter       *child);
static void         gtk2hs_store_ref_node        (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static void         gtk2hs_store_unref_node      (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
/* The TreeSortable interface is currently not implemented and may never be. */
static gboolean     gtk2hs_store_get_sort_column_id     (GtkTreeSortable        *sortable,
                                                         gint                   *sort_column_id,
                                                         GtkSortType            *order);
static void         gtk2hs_store_set_sort_column_id     (GtkTreeSortable        *sortable,
                                                         gint                   sort_column_id,
                                                         GtkSortType            order);
#if GTK_MAJOR_VERSION < 3
static void	    gtk2hs_store_set_sort_func		(GtkTreeSortable        *sortable,
                                                         gint                   sort_column_id,
                                                         GtkTreeIterCompareFunc func,
                                                         gpointer               data,
                                                         GtkDestroyNotify       destroy);
static void         gtk2hs_store_set_default_sort_func  (GtkTreeSortable        *sortable,
                                                         GtkTreeIterCompareFunc func,
                                                         gpointer               data,
                                                         GtkDestroyNotify       destroy);
#endif
static gboolean     gtk2hs_store_has_default_sort_func  (GtkTreeSortable        *sortable);



static gboolean     gtk2hs_store_row_draggable          (GtkTreeDragSource      *drag_source,
                                                         GtkTreePath            *path);
static gboolean     gtk2hs_store_drag_data_get          (GtkTreeDragSource      *drag_source,
                                                         GtkTreePath            *path,
                                                         GtkSelectionData       *selection_data);
static gboolean     gtk2hs_store_drag_data_delete       (GtkTreeDragSource      *drag_source,
                                                         GtkTreePath            *path);
static gboolean     gtk2hs_store_drag_data_received     (GtkTreeDragDest        *drag_dest,
                                                         GtkTreePath            *dest_path,
                                                         GtkSelectionData       *selection_data);
static gboolean     gtk2hs_store_row_drop_possible      (GtkTreeDragDest        *drag_dest,
                                                         GtkTreePath            *dest_path,
                                                         GtkSelectionData       *selection_data);
                                                                                          
static GObjectClass *parent_class = NULL;


/**
 *
 *  gtk2hs_store_get_type: here we register our new type and its interfaces
 *                         with the type system. If you want to implement
 *                         additional interfaces like GtkTreeSortable, you
 *                         will need to do it here.
 *
 **/
GType
gtk2hs_store_get_type (void)
{
  static GType gtk2hs_store_type = 0;

  if (!gtk2hs_store_type)
  {
    static const GTypeInfo gtk2hs_store_info =
    {
      sizeof (Gtk2HsStoreClass),
      NULL,              /* base_init */
      NULL,              /* base_finalize */
      (GClassInitFunc) gtk2hs_store_class_init,
      NULL,              /* class finalize */
      NULL,              /* class_data */
      sizeof (Gtk2HsStore),
      0,                 /* n_preallocs */
      (GInstanceInitFunc) gtk2hs_store_init
    };

    static const GInterfaceInfo tree_model_info =
    {
      (GInterfaceInitFunc) gtk2hs_store_tree_model_init,
      NULL,
      NULL
    };

#if GTK_MAJOR_VERSION < 3
/* The TreeSortable interface is currently not implemented. */
    static const GInterfaceInfo tree_sortable_info =
    {
      (GInterfaceInitFunc) gtk2hs_store_tree_sortable_init,
      NULL,
      NULL
    };
#endif
    static const GInterfaceInfo tree_drag_source_info =
    {
      (GInterfaceInitFunc) gtk2hs_store_tree_drag_source_init,
      NULL,
      NULL
    };

    static const GInterfaceInfo tree_drag_dest_info =
    {
      (GInterfaceInitFunc) gtk2hs_store_tree_drag_dest_init,
      NULL,
      NULL
    };

    gtk2hs_store_type = g_type_register_static (G_TYPE_OBJECT, "Gtk2HsStore",
                                               &gtk2hs_store_info,
                                               (GTypeFlags) 0);

    g_type_add_interface_static (gtk2hs_store_type, GTK_TYPE_TREE_MODEL,
    &tree_model_info);
    
    /* The TreeSortable interface is currently not implemented. Uncomment to
    add it. */
/*    g_type_add_interface_static (gtk2hs_store_type,
                                 GTK_TYPE_TREE_SORTABLE,
                                 &tree_sortable_info);
*/                                 
    g_type_add_interface_static (gtk2hs_store_type,
                                 GTK_TYPE_TREE_DRAG_SOURCE,
                                 &tree_drag_source_info);
                                 
    g_type_add_interface_static (gtk2hs_store_type,
                                 GTK_TYPE_TREE_DRAG_DEST,
                                 &tree_drag_dest_info);
  }

  return gtk2hs_store_type;
}


/**
 *
 *  gtk2hs_store_class_init: more boilerplate GObject/GType stuff.
 *                           Init callback for the type system,
 *                           called once when our new class is created.
 *
 **/
static void
gtk2hs_store_class_init (Gtk2HsStoreClass *class)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_class_init\t\t(%p)\n", class));
  GObjectClass *object_class;

  parent_class = g_type_class_peek_parent (class);
  object_class = (GObjectClass*) class;

  object_class->finalize = gtk2hs_store_finalize;
}

/**
 *
 *  gtk2hs_store_tree_model_init: init callback for the interface registration
 *                                in gtk2hs_store_get_type. Here we override
 *                                the GtkTreeModel interface functions that
 *                                we implement.
 *
 **/
static void
gtk2hs_store_tree_model_init (GtkTreeModelIface *iface)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_tree_model_init\t(%p)\n", iface));
  iface->get_flags       = gtk2hs_store_get_flags;
  iface->get_n_columns   = gtk2hs_store_get_n_columns;
  iface->get_column_type = gtk2hs_store_get_column_type;
  iface->get_iter        = gtk2hs_store_get_iter;
  iface->get_path        = gtk2hs_store_get_path;
  iface->get_value       = gtk2hs_store_get_value;
  iface->iter_next       = gtk2hs_store_iter_next;
  iface->iter_children   = gtk2hs_store_iter_children;
  iface->iter_has_child  = gtk2hs_store_iter_has_child;
  iface->iter_n_children = gtk2hs_store_iter_n_children;
  iface->iter_nth_child  = gtk2hs_store_iter_nth_child;
  iface->iter_parent     = gtk2hs_store_iter_parent;
  iface->ref_node        = gtk2hs_store_ref_node;
  iface->unref_node      = gtk2hs_store_unref_node;
}

#if GTK_MAJOR_VERSION < 3
/**
 *
 *  gtk2hs_store_tree_sortable_init: init callback for the interface registration
 *                                   in gtk2hs_store_get_type. Here we override
 *                                   the GtkTreeSortable interface functions that
 *                                   we implement.
 *
 **/
/* The TreeSortable interface is currently not implemented. */
static void
gtk2hs_store_tree_sortable_init (GtkTreeSortableIface *iface)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_tree_sortable_init\t(%p)\n", iface));
  iface->get_sort_column_id     = gtk2hs_store_get_sort_column_id;
  iface->set_sort_column_id     = gtk2hs_store_set_sort_column_id;
  iface->set_sort_func          = gtk2hs_store_set_sort_func;
  iface->set_default_sort_func  = gtk2hs_store_set_default_sort_func;
  iface->has_default_sort_func  = gtk2hs_store_has_default_sort_func;
}
#endif
/**
 *
 *  gtk2hs_store_tree_drag_source_init: init callback for the interface registration
 *                                      in gtk2hs_store_get_type. Here we override
 *                                      the GtkTreeDragSource interface functions that
 *                                      we implement.
 *
 **/
static void
gtk2hs_store_tree_drag_source_init (GtkTreeDragSourceIface *iface)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_tree_drag_source_init\t(%p)\n", iface));
  iface->row_draggable          = gtk2hs_store_row_draggable;
  iface->drag_data_get          = gtk2hs_store_drag_data_get;
  iface->drag_data_delete       = gtk2hs_store_drag_data_delete;
}

/**
 *
 *  gtk2hs_store_tree_drag_dest_init: init callback for the interface registration
 *                                    in gtk2hs_store_get_type. Here we override
 *                                    the GtkTreeDragDest interface functions that
 *                                    we implement.
 *
 **/
static void
gtk2hs_store_tree_drag_dest_init (GtkTreeDragDestIface *iface)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_tree_drag_dest_init\t(%p)\n", iface));
  iface->drag_data_received     = gtk2hs_store_drag_data_received;
  iface->row_drop_possible      = gtk2hs_store_row_drop_possible;
}

/**
 *
 *  gtk2hs_store_init: this is called everytime a new custom list object
 *                     instance is created (we do that in gtk2hs_store_new).
 *                     Initialise the list structure's fields here.
 *
 **/
static void
gtk2hs_store_init (Gtk2HsStore *store)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_init\t\t(%p)\n", store));

  store->stamp = g_random_int();  /* Random int to check whether an iter belongs to our model */
}


/**
 *
 *  gtk2hs_store_finalize: this is called just before a custom list is
 *                         destroyed. Free dynamically allocated memory here.
 *
 **/
static void
gtk2hs_store_finalize (GObject *object)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_finalize\t(%p)\n", object));
  Gtk2HsStore *store = (Gtk2HsStore *) object;
  g_return_if_fail(GTK2HS_IS_STORE (object));

  /* free all memory used by the store */
  hs_free_stable_ptr(store->impl);
  hs_free_stable_ptr(store->priv);

  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}


/**
 *
 *  gtk2hs_store_get_flags: tells the rest of the world whether our tree model
 *                          has any special characteristics. In our case,
 *                          we have a list model (instead of a tree), and each
 *                          tree iter is valid as long as the row in question
 *                          exists, as it only contains a pointer to our struct.
 *
 **/
static GtkTreeModelFlags
gtk2hs_store_get_flags (GtkTreeModel *tree_model)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_get_flags\t\t(%p)\n", tree_model));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), 0);

  GtkTreeModelFlags result = gtk2hs_store_get_flags_impl(store->impl);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_get_flags\t\t=%#x\n", result));
  return result;
}


/**
 *
 *  gtk2hs_store_get_n_columns: tells the rest of the world how many data
 *                              columns we export via the tree model interface
 *
 **/
static gint
gtk2hs_store_get_n_columns (GtkTreeModel *tree_model)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_get_n_columns\t(%p)\n", tree_model));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), 0);

  gint result = gtk2hs_store_get_n_columns_impl(store->impl);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_get_n_columns\t=%d\n", result));
  return result;
}


/**
 *
 *  gtk2hs_store_get_column_type: tells the rest of the world which type of
 *                                data an exported model column contains
 *
 **/
static GType
gtk2hs_store_get_column_type (GtkTreeModel *tree_model,
                              gint          index)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_get_column_type\t(%p, %d)\n", tree_model, index));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), G_TYPE_INVALID);
  
  GType result = gtk2hs_store_get_column_type_impl(store->impl, index);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_get_column_type\t=%s\n", g_type_name(result)));
  return result;
}


/**
 *
 *  gtk2hs_store_get_iter: converts a tree path (physical position) into a
 *                         tree iter structure (the content of the iter
 *                         fields will only be used internally by our model).
 *
 **/
static gboolean
gtk2hs_store_get_iter (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter,       /* out */
                       GtkTreePath  *path        /* in  */)
{
  WHEN_DEBUG(
    gchar *path_str = gtk_tree_path_to_string(path);
    g_debug("calling gtk2hs_store_get_iter\t\t(%p, %p, \"%s\")\n", tree_model, iter, path_str);
    g_free(path_str);
  )
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), FALSE);

  gboolean result = gtk2hs_store_get_iter_impl(store->impl, iter, path);
  if (result) iter->stamp = store->stamp;
  WHEN_DEBUG(g_debug("return  gtk2hs_store_get_iter\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_get_path: converts a tree iter into a tree path (ie. the
 *                         physical position of that row in the list).
 *
 **/
static GtkTreePath *
gtk2hs_store_get_path (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter        /* in */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_get_path\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), NULL);
  g_return_val_if_fail (iter->stamp == store->stamp, NULL);
  
  GtkTreePath * result = gtk2hs_store_get_path_impl(store->impl, iter);
  WHEN_DEBUG(
    gchar *result_str = gtk_tree_path_to_string(result);
    g_debug("return  gtk2hs_store_get_path\t\t=\"%s\"\n", result_str);
    g_free(result_str);
  )
  return result;
}


/**
 *
 *  gtk2hs_store_get_value: Returns a row's exported data columns
 *                          (_get_value is what gtk_tree_model_get uses)
 *
 **/
static void
gtk2hs_store_get_value (GtkTreeModel *tree_model,
                        GtkTreeIter  *iter,       /* in */
                        gint          column,
                        GValue       *value       /* out */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_get_value\t\t(%p, %p, %d, %p)\n", tree_model, iter, column, value));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_if_fail (GTK2HS_IS_STORE (tree_model));
  g_return_if_fail (iter->stamp == store->stamp);

  gtk2hs_store_get_value_impl(store->impl, iter, column, value);
    WHEN_DEBUG(
      gchar *result = g_strdup_value_contents(value);
      g_debug("return  gtk2hs_store_get_value\t\t=%s\n", result);
      g_free(result);
    )
}


/**
 *
 *  gtk2hs_store_iter_next: Takes an iter structure and sets it to point
 *                          to the next row.
 *
 **/
static gboolean
gtk2hs_store_iter_next (GtkTreeModel  *tree_model,
                        GtkTreeIter   *iter        /* in+out */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_iter_next\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), FALSE);

  gboolean result = gtk2hs_store_iter_next_impl(store->impl, iter);
  if (result) iter->stamp = store->stamp;
  WHEN_DEBUG(g_debug("return  gtk2hs_store_iter_next\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_children: Returns TRUE or FALSE depending on whether
 *                              the row specified by 'parent' has any children.
 *                              If it has children, then 'iter' is set to
 *                              point to the first child. Special case: if
 *                              'parent' is NULL, then the first top-level
 *                              row should be returned if it exists.
 *
 **/
static gboolean
gtk2hs_store_iter_children (GtkTreeModel *tree_model,
                            GtkTreeIter  *iter,       /* out */
                            GtkTreeIter  *parent      /* in, maybe NULL */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_iter_children\t(%p, %p, %p)\n", tree_model, iter, parent));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), FALSE);

  gboolean result = gtk2hs_store_iter_children_impl(store->impl, iter, parent);
  if (result) iter->stamp = store->stamp;
  WHEN_DEBUG(g_debug("return  gtk2hs_store_iter_children\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_has_child: Returns TRUE or FALSE depending on whether
 *                               the row specified by 'iter' has any children.
 *
 **/
static gboolean
gtk2hs_store_iter_has_child (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter        /* in */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_iter_has_child\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), FALSE);
  /* don't check if iter->stamp == store->stamp; see the thread culminating in
   * http://sourceforge.net/p/gtk2hs/mailman/message/31887332/ for details */
  
  gboolean result = gtk2hs_store_iter_has_child_impl(store->impl, iter);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_iter_has_child\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_n_children: Returns the number of children the row
 *                                specified by 'iter' has. This is usually 0,
 *                                as we only have a list and thus do not have
 *                                any children to any rows. A special case is
 *                                when 'iter' is NULL, in which case we need
 *                                to return the number of top-level nodes,
 *                                ie. the number of rows in our list.
 *
 **/
static gint
gtk2hs_store_iter_n_children (GtkTreeModel *tree_model,
                              GtkTreeIter  *iter        /* in, maybe NULL */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_iter_n_children\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), 0);
  g_return_val_if_fail (iter == NULL || iter->stamp == store->stamp, 0);

  gboolean result = gtk2hs_store_iter_n_children_impl(store->impl, iter);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_iter_n_children\t=%d\n", result));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_nth_child: If the row specified by 'parent' has any
 *                               children, set 'iter' to the n-th child and
 *                               return TRUE if it exists, otherwise FALSE.
 *                               A special case is when 'parent' is NULL, in
 *                               which case we need to set 'iter' to the n-th
 *                               row if it exists.
 *
 **/
static gboolean
gtk2hs_store_iter_nth_child (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter,       /* out */
                             GtkTreeIter  *parent,     /* in, maybe NULL */
                             gint          n)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_iter_nth_child\t(%p, %p, %p, %d)\n", tree_model, iter, parent, n));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), FALSE);
  g_return_val_if_fail (parent == NULL || parent->stamp == store->stamp, FALSE);

  gboolean result = gtk2hs_store_iter_nth_child_impl(store->impl, iter, parent, n);
  if (result) iter->stamp = store->stamp;
  WHEN_DEBUG(g_debug("return  gtk2hs_store_iter_nth_child\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_parent: Point 'iter' to the parent node of 'child'.
 *
 **/
static gboolean
gtk2hs_store_iter_parent (GtkTreeModel *tree_model,
                          GtkTreeIter  *iter,       /* out */
                          GtkTreeIter  *child       /* in  */)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_iter_parent\t\t(%p, %p, %p)\n", tree_model, iter, child));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_val_if_fail (GTK2HS_IS_STORE (tree_model), FALSE);
  g_return_val_if_fail (child != NULL, FALSE);
  g_return_val_if_fail (child->stamp == store->stamp, FALSE);
 
  gboolean result = gtk2hs_store_iter_parent_impl(store->impl, iter, child);
  if (result) iter->stamp = store->stamp;
  WHEN_DEBUG(g_debug("return  gtk2hs_store_iter_parent\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


static void
gtk2hs_store_ref_node (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_ref_node\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_if_fail (GTK2HS_IS_STORE (tree_model));

  gtk2hs_store_ref_node_impl(store->impl, iter);
}

static void
gtk2hs_store_unref_node (GtkTreeModel *tree_model,
                         GtkTreeIter  *iter)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_unref_node\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_return_if_fail (GTK2HS_IS_STORE (tree_model));
  g_return_if_fail (iter->stamp == store->stamp);

  gtk2hs_store_unref_node_impl(store->impl, iter);
}

static gboolean
gtk2hs_store_get_sort_column_id     (GtkTreeSortable        *sortable,
                                     gint                   *sort_column_id,
                                     GtkSortType            *order)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_get_sort_column_id\t\t(%p)\n", sortable));
  return 0;
}
         
static void
gtk2hs_store_set_sort_column_id (GtkTreeSortable        *sortable,
                                 gint                   sort_column_id,
                                 GtkSortType            order)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_set_sort_column_id\t\t(%p)\n", sortable));
  return;
}

#if GTK_MAJOR_VERSION < 3
static void
gtk2hs_store_set_sort_func (GtkTreeSortable        *sortable,
                            gint                   sort_column_id,
                            GtkTreeIterCompareFunc func,
                            gpointer               data,
                            GtkDestroyNotify       destroy)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_set_sort_func\t\t(%p)\n", sortable));
  return;  
}

static void
gtk2hs_store_set_default_sort_func (GtkTreeSortable        *sortable,
                                    GtkTreeIterCompareFunc func,
                                    gpointer               data,
                                    GtkDestroyNotify       destroy)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_set_default_sort_func\t\t(%p)\n", sortable));
  return;
}
#endif
static gboolean
gtk2hs_store_has_default_sort_func (GtkTreeSortable        *sortable)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_has_default_sort_func\t\t(%p)\n", sortable));
  return 0;
}

static gboolean
gtk2hs_store_row_draggable (GtkTreeDragSource *drag_source,
                            GtkTreePath       *path) {
  WHEN_DEBUG(
    gchar *path_str = gtk_tree_path_to_string(path);
    g_debug("calling gtk2hs_store_row_draggable\t\t(%p, \"%s\")\n", drag_source, path_str);
    g_free(path_str);
  )
  Gtk2HsStore *store = (Gtk2HsStore *) drag_source;
  g_return_val_if_fail (GTK2HS_IS_STORE (drag_source), FALSE);
                    
  gboolean result = gtk2hs_store_row_draggable_impl(drag_source, store->impl, path);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_row_draggable\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}

static gboolean
gtk2hs_store_drag_data_get (GtkTreeDragSource *drag_source,
                            GtkTreePath       *path,
                            GtkSelectionData  *selection_data) {
  WHEN_DEBUG(
    gchar *path_str = gtk_tree_path_to_string(path);
    g_debug("calling gtk2hs_store_drag_data_get\t\t(%p, \"%s\", %p)\n", drag_source, path_str, selection_data);
    g_free(path_str);
  )
  Gtk2HsStore *store = (Gtk2HsStore *) drag_source;
  g_return_val_if_fail (GTK2HS_IS_STORE (drag_source), FALSE);
  g_return_val_if_fail (selection_data!=NULL, FALSE);
  
  gboolean result = gtk2hs_store_drag_data_get_impl(drag_source, store->impl, path, selection_data);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_drag_data_get\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}

static gboolean
gtk2hs_store_drag_data_delete (GtkTreeDragSource *drag_source,
                               GtkTreePath       *path) {
  WHEN_DEBUG(
    gchar *path_str = gtk_tree_path_to_string(path);
    g_debug("calling gtk2hs_store_drag_data_delete\t\t(%p, \"%s\")\n", drag_source, path_str);
    g_free(path_str);
  )
  Gtk2HsStore *store = (Gtk2HsStore *) drag_source;
  g_return_val_if_fail (GTK2HS_IS_STORE (drag_source), FALSE);

  gboolean result = gtk2hs_store_drag_data_delete_impl(drag_source, store->impl, path);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_drag_data_delete\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}

static gboolean
gtk2hs_store_drag_data_received (GtkTreeDragDest  *drag_dest,
                                 GtkTreePath      *dest_path,
                                 GtkSelectionData *selection_data) {
  WHEN_DEBUG(
    gchar *path_str = gtk_tree_path_to_string(dest_path);
    g_debug("calling gtk2hs_store_drag_data_received\t\t(%p, \"%s\", %p)\n", drag_dest, path_str, selection_data);
    g_free(path_str);
  )
  Gtk2HsStore *store = (Gtk2HsStore *) drag_dest;
  g_return_val_if_fail (GTK2HS_IS_STORE (drag_dest), FALSE);
  g_return_val_if_fail (selection_data!=NULL, FALSE);

  gboolean result = gtk2hs_store_drag_data_received_impl(drag_dest, store->impl, dest_path, selection_data);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_drag_data_received\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}

static gboolean
gtk2hs_store_row_drop_possible (GtkTreeDragDest  *drag_dest,
                                GtkTreePath      *dest_path,
                                GtkSelectionData *selection_data) {
  WHEN_DEBUG(
    gchar *path_str = gtk_tree_path_to_string(dest_path);
    g_debug("calling gtk2hs_store_row_drop_possible\t\t(%p, \"%s\", %p)\n", drag_dest, path_str, selection_data);
    g_free(path_str);
  )
  Gtk2HsStore *store = (Gtk2HsStore *) drag_dest;
  g_return_val_if_fail (GTK2HS_IS_STORE (drag_dest), FALSE);
  g_return_val_if_fail (selection_data!=NULL, FALSE);

  gboolean result = gtk2hs_store_row_drop_possible_impl(drag_dest, store->impl, dest_path, selection_data);
  WHEN_DEBUG(g_debug("return  gtk2hs_store_row_drop_possible\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}

/**
 *
 *  gtk2hs_store_new:  Create a new custom tree model which delegates to a
 *                     Haskell implementation.
 *
 **/
Gtk2HsStore *
gtk2hs_store_new (HsStablePtr impl, HsStablePtr priv)
{
  WHEN_DEBUG(g_debug("calling gtk2hs_store_new\t\t(%p)\n", impl));
  Gtk2HsStore *newstore = (Gtk2HsStore*) g_object_new (GTK2HS_TYPE_STORE, NULL);

  newstore->impl = impl;
  newstore->priv = priv;

  WHEN_DEBUG(g_debug("return  gtk2hs_store_new\t\t=%p\n", newstore));
  return newstore;
}

HsStablePtr gtk2hs_store_get_impl (Gtk2HsStore *store)
{
  g_return_val_if_fail(GTK2HS_IS_STORE(store), NULL);
  return store->impl;
}

HsStablePtr gtk2hs_store_get_priv (Gtk2HsStore *store)
{
  g_return_val_if_fail(GTK2HS_IS_STORE(store), NULL);
  return store->priv;
}

gint
gtk2hs_store_get_stamp (Gtk2HsStore *store)
{
  g_return_val_if_fail(GTK2HS_IS_STORE(store), 0);
  return store->stamp;
}

void
gtk2hs_store_increment_stamp (Gtk2HsStore *store)
{
  g_return_if_fail(GTK2HS_IS_STORE(store));
  do
    {
      store->stamp++;
    }
  while (store->stamp == 0);
}
