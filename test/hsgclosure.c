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
  free (gtype);
}

/* Same as freeHaskellFunctionPtr, but it does nothing when given a
   null pointer, instead of crashing */
void safeFreeFunPtr(void *ptr)
{
  if (ptr != NULL)
    freeHaskellFunctionPtr(ptr);
}

/*
  We are not supposed to call freeHaskellFunPtr from the Haskell code
  references by the FunPtr itself, but for callbacks of
  ScopeTypeTypeAsync we only know that we can release the FunPtr from
  _inside_ the FunPtr (the semantics is that the callback will be
  called exactly once, and there's no associated GDestroyNotify to
  separately free resources).

  The following is a bit of a hack to deal with this, we simply
  schedule the freeing of resources one second from now with
  g_timeout_add_seconds. Notice that this implies that if the GLib
  main loop is not running we may leak resources. A more robust
  solution would probably be preferable, but I could not come up with
  anything that was not much more complicated than this.
*/
static gboolean do_free_the_FunPtr(gpointer ptr)
{
  fprintf(stderr, "Freeing async pointer: %p\n", ptr);
  freeHaskellFunctionPtr(ptr);
  return FALSE;
}

void releaseFunPtr(void *ptr)
{
  g_timeout_add_seconds(1, do_free_the_FunPtr, ptr);
}
