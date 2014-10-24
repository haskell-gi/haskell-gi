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

     if (result == 0) {
       GType instance_type = G_TYPE_FROM_INSTANCE (instance);
       fprintf(stderr, "Check failed: %s is not a %s\n",
	       g_type_name (instance_type),
	       g_type_name (type));
     }
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
    __print_debug_info = secure_getenv ("HASKELL_GI_DEBUG_MEM") != NULL;
  }

  return __print_debug_info;
}

/* Auxiliary function for freeing boxed types */
void boxed_free_helper (GType *gtype, void *boxed)
{
  if (print_debug_info()) {
    fprintf(stderr, "Freeing a boxed object at %p\n", boxed);
    fprintf(stderr, "\tIt is of type %s\n", g_type_name(*gtype));
  }

  g_boxed_free (*gtype, boxed);
  g_free (gtype);

  if (print_debug_info()) {
    fprintf(stderr, "\tdone\n");
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

gpointer dbg_g_object_newv (GType gtype, guint n_params, GParameter *params)
{
  gpointer result;

  if (print_debug_info()) {
    fprintf(stderr, "Creating a new GObject of type %s\n",
            g_type_name(gtype));
  }

  result = g_object_newv (gtype, n_params, params);

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
