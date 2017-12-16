#define _GNU_SOURCE

/* GHC's semi-public Rts API */
#include <Rts.h>

#include <stdlib.h>
#include <string.h>

#include <glib-object.h>
#include <glib.h>

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

/* Information about a boxed type to free */
typedef struct {
  GType gtype;
  gpointer boxed;
} BoxedFreeInfo;

/* Auxiliary function for freeing boxed types in the main loop. See
   the annotation in g_object_unref_in_main_loop() below. */
static gboolean main_loop_boxed_free_helper (gpointer _info)
{
  BoxedFreeInfo *info = (BoxedFreeInfo*)_info;

  if (print_debug_info()) {
    GThread *self = g_thread_self ();

    fprintf(stderr, "Freeing a boxed object at %p from idle callback [thread: %p]\n",
            info->boxed, self);
    fprintf(stderr, "\tIt is of type %s\n", g_type_name(info->gtype));
  }

  g_boxed_free (info->gtype, info->boxed);

  if (print_debug_info()) {
    fprintf(stderr, "\tdone\n");
  }

  g_free(info);

  return FALSE; /* Do not invoke again */
}

void boxed_free_helper (GType gtype, void *boxed)
{
  BoxedFreeInfo *info = g_malloc(sizeof(BoxedFreeInfo));

  info->gtype = gtype;
  info->boxed = boxed;

  g_idle_add (main_loop_boxed_free_helper, info);
}

void dbg_g_object_disown (GObject *obj)
{
  GType gtype;

  if (print_debug_info()) {
    GThread *self = g_thread_self();
    fprintf(stderr, "Disowning a GObject at %p [thread: %p]\n", obj, self);
    gtype = G_TYPE_FROM_INSTANCE (obj);
    fprintf(stderr, "\tIt is of type %s\n", g_type_name(gtype));
    fprintf(stderr, "\tIts refcount before disowning is %d\n",
            (int)obj->ref_count);
  }
}

static void print_object_dbg_info (GObject *obj)
{
  GThread *self = g_thread_self();
  GType gtype;

  fprintf(stderr, "Unref of %p from idle callback [thread: %p]\n", obj, self);
  gtype = G_TYPE_FROM_INSTANCE (obj);
  fprintf(stderr, "\tIt is of type %s\n", g_type_name(gtype));
  fprintf(stderr, "\tIts refcount before unref is %d\n",
          (int)obj->ref_count);
}

/*
  We schedule all GObject deletions to happen in the main loop. The
  reason is that for some types the destructor is not thread safe, and
  assumes that it is being run from the same thread as the main loop
  that created the object.
 */
static gboolean
g_object_unref_in_main_loop (gpointer obj)
{
  if (print_debug_info()) {
    print_object_dbg_info ((GObject*)obj);
  }

  g_object_unref (obj);

  if (print_debug_info()) {
    fprintf(stderr, "\tUnref done\n");
  }

  return FALSE; /* Do not invoke again */
}

void dbg_g_object_unref (GObject *obj)
{
  g_idle_add(g_object_unref_in_main_loop, obj);
}

/**
 * dbg_g_object_new:
 * @gtype: #GType for the object to construct.
 * @n_props: Number of parameters for g_object_new_with_properties().
 * @names: Names of the properties to be set.
 * @values: Parameters for g_object_new_with_properties().
 *
 * Allocate a #GObject of #GType @gtype, with the given @params. The
 * returned object is never floating, and we always own a reference to
 * it. (It might not be the only existing to the object, but it is in
 * any case safe to call g_object_unref() when we are not wrapping the
 * object ourselves anymore.)
 *
 * Returns: A new #GObject.
 */
gpointer dbg_g_object_new (GType gtype, guint n_props,
                           const char *names[], const GValue values[])
{
  gpointer result;

  if (print_debug_info()) {
    GThread *self = g_thread_self();

    fprintf(stderr, "Creating a new GObject of type %s [thread: %p]\n",
            g_type_name(gtype), self);
  }

#if GLIB_CHECK_VERSION(2,54,0)
  result = g_object_new_with_properties (gtype, n_props, names, values);
#else
  { GParameter params[n_props];
    int i;

    for (i=0; i<n_props; i++) {
      memcpy (&params[i].value, &values[i], sizeof(GValue));
      params[i].name = names[i];
    }

    result = g_object_newv (gtype, n_props, params);
  }
#endif

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
