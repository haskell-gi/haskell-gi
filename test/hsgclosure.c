/* GHC's semi-public Rts API */
#include <Rts.h>

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

/* Auxiliary function for freeing boxed types */
void boxed_free_helper (GType *gtype, void *boxed)
{
  g_boxed_free (*gtype, boxed);
  g_free (gtype);
}

/* Same as freeHaskellFunctionPtr, but it does nothing when given a
   null pointer, instead of crashing */
void safeFreeFunPtr(void *ptr)
{
  if (ptr != NULL)
    freeHaskellFunctionPtr(ptr);
}
