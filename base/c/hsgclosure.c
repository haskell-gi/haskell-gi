#define _GNU_SOURCE

/* GHC's semi-public Rts API */
#include <Rts.h>

#include <stdlib.h>

#include <glib-object.h>

int check_object_type(void *instance, GType type)
{
  int result;

  if (instance != NULL) {
     result = !!G_TYPE_CHECK_INSTANCE_TYPE(instance, type);
  } else {
    result = 0;
    fprintf(stderr, "Check failed: got a null pointer\n");
  }

  return result;
}

static int print_debug_info ()
{
  static int __print_debug_info = -1;

  if (__print_debug_info == -1) {
    __print_debug_info = getenv ("HASKELL_GI_DEBUG_MEM") != NULL;
  }

  return __print_debug_info;
}

/* Auxiliary function for freeing boxed types */
void boxed_free_helper (GType gtype, void *boxed)
{
  if (print_debug_info()) {
    fprintf(stderr, "Freeing a boxed object at %p\n", boxed);
    fprintf(stderr, "\tIt is of type %s\n", g_type_name(gtype));
  }

  g_boxed_free (gtype, boxed);

  if (print_debug_info()) {
    fprintf(stderr, "\tdone\n");
  }
}

void dbg_g_object_disown (GObject *obj)
{
  GType gtype;

  if (print_debug_info()) {
    fprintf(stderr, "Disowning a GObject at %p\n", obj);
    gtype = G_TYPE_FROM_INSTANCE (obj);
    fprintf(stderr, "\tIt is of type %s\n", g_type_name(gtype));
    fprintf(stderr, "\tIts refcount before disowning is %d\n",
            (int)obj->ref_count);
  }
}

void dbg_g_object_unref (GObject *obj)
{
  GType gtype;

  if (print_debug_info()) {
    fprintf(stderr, "Freeing a GObject at %p\n", obj);
    gtype = G_TYPE_FROM_INSTANCE (obj);
    fprintf(stderr, "\tIt is of type %s\n", g_type_name(gtype));
    fprintf(stderr, "\tIts refcount before unref is %d\n",
            (int)obj->ref_count);
  }

  g_object_unref(obj);

  if (print_debug_info()) {
    fprintf(stderr, "\tdone\n");
  }
}

/**
 * dbg_g_object_new:
 * @gtype: #GType for the object to construct.
 * @n_params: Number of parameters for g_object_newv().
 * @params: (array length=n_params) Parameters for g_object_newv().
 *
 * Allocate a #GObject of #GType @gtype, with the given @params. The
 * returned object is never floating, and we always own a reference to
 * it. (It might not be the only existing to the object, but it is in
 * any case safe to call g_object_unref() when we are not wrapping the
 * object ourselves anymore.)
 *
 * Returns: A new #GObject.
 */
gpointer dbg_g_object_newv (GType gtype, guint n_params, GParameter *params)
{
  gpointer result;

  if (print_debug_info()) {
    fprintf(stderr, "Creating a new GObject of type %s\n",
            g_type_name(gtype));
  }

  result = g_object_newv (gtype, n_params, params);

  /*
    Initially unowned GObjects can be either floating or not after
    construction. They are generally floating, but GtkWindow for
    instance is not floating after construction.

    In either case we want to call g_object_ref_sink(): if the object
    is floating to take ownership of the reference, and otherwise to
    add a reference that we own.

    If the object is not initially unowned we simply take control of
    the initial reference (implicitly).
   */
  if (G_IS_INITIALLY_UNOWNED (result)) {
    g_object_ref_sink (result);
  }

  if (print_debug_info()) {
    fprintf(stderr, "\tdone, got a pointer at %p\n", result);
  }

  return result;
}

/* Same as freeHaskellFunctionPtr, but it does nothing when given a
   null pointer, instead of crashing */
void safeFreeFunPtr(void *ptr)
{
  if (ptr != NULL)
    freeHaskellFunctionPtr(ptr);
}
