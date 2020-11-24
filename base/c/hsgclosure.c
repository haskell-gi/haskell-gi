#define _GNU_SOURCE

/* GHC's semi-public Rts API */
#include <Rts.h>

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include <glib-object.h>
#include <glib.h>

static int print_debug_info ()
{
  static int __print_debug_info = -1;

  if (__print_debug_info == -1) {
    __print_debug_info = getenv ("HASKELL_GI_DEBUG_MEM") != NULL;
  }

  return __print_debug_info;
}

/*
  A mutex protecting the log file handle. We make it recursive,
  i.e. refcounted, so it is OK to lock repeatedly in the same thread.
*/
static pthread_mutex_t log_mutex
#if defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
  = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#elif defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER)
  = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
  ;
__attribute__ ((constructor)) static void init_log_mutex()
{
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&log_mutex, &attr);
  pthread_mutexattr_destroy(&attr);
}
#endif

/* Give the current thread exclusive access to the log */
static void lock_log()
{
  pthread_mutex_lock(&log_mutex);
}

/* Decrease the refcount of the mutex protecting access to the log
   from other threads */
static void unlock_log()
{
  pthread_mutex_unlock(&log_mutex);
}

/* Print the given message to the log. The passed in string does not
   need to be zero-terminated. The message is only printed if the
   HASKELL_GI_DEBUG_MEM variable is set. */
void dbg_log_with_len (const char *msg, int len)
{
  if (print_debug_info()) {
    lock_log();
    fwrite(msg, len, 1, stderr);
    unlock_log();
  }
}

/* Print the given printf-style message to the log. The message is
   only printed if the HASKELL_GI_DEBUG_MEM variable is set. */
__attribute__ ((format (printf, 1, 2)))
static void dbg_log (const char *msg, ...)
{
  va_list args;

  va_start(args, msg);

  if (print_debug_info()) {
    lock_log();
    vfprintf(stderr, msg, args);
    unlock_log();
  }

  va_end(args);
}

int check_object_type (void *instance, GType type)
{
  int result;

  if (instance != NULL) {
     result = !!G_TYPE_CHECK_INSTANCE_TYPE(instance, type);
  } else {
    result = 0;
    dbg_log("Check failed: got a null pointer\n");
  }

  return result;
}

GType _haskell_gi_g_value_get_type (GValue *gvalue)
{
  return G_VALUE_TYPE (gvalue);
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
    lock_log();
    dbg_log("Freeing a boxed object at %p from idle callback [thread: %p]\n",
            info->boxed, self);
    dbg_log("\tIt is of type %s\n", g_type_name(info->gtype));
  }

  g_boxed_free (info->gtype, info->boxed);

  if (print_debug_info()) {
    dbg_log("\tdone freeing %p.\n", info->boxed);
    unlock_log();
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
    lock_log();
    GThread *self = g_thread_self();
    dbg_log("Disowning a GObject at %p [thread: %p]\n", obj, self);
    gtype = G_TYPE_FROM_INSTANCE (obj);
    dbg_log("\tIt is of type %s\n", g_type_name(gtype));
    dbg_log("\tIts refcount before disowning is %d\n", (int)obj->ref_count);
    unlock_log();
  }
}

static void print_object_dbg_info (GObject *obj)
{
  GThread *self = g_thread_self();
  GType gtype;

  dbg_log("Unref of %p from idle callback [thread: %p]\n", obj, self);
  gtype = G_TYPE_FROM_INSTANCE (obj);
  dbg_log("\tIt is of type %s\n", g_type_name(gtype));
  dbg_log("\tIts refcount before unref is %d\n", (int)obj->ref_count);
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
    lock_log();
    print_object_dbg_info ((GObject*)obj);
  }

  g_object_unref (obj);

  if (print_debug_info()) {
    fprintf(stderr, "\tUnref done\n");
    unlock_log();
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

    lock_log();
    dbg_log("Creating a new GObject of type %s [thread: %p]\n",
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
    dbg_log("\tdone, got a pointer at %p\n", result);
    unlock_log();
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

/* Same as safeFreeFunPtr, but it accepts (but ignores) an extra argument */
void safeFreeFunPtr2(void *ptr, void *unused)
{
  safeFreeFunPtr(ptr);
}

/* Returns the GType associated to a class instance */
GType haskell_gi_gtype_from_class (gpointer klass)
{
  return G_TYPE_FROM_CLASS (klass);
}

/* Returns the GType associated to a given instance */
GType haskell_gi_gtype_from_instance (gpointer instance)
{
  return G_TYPE_FROM_INSTANCE (instance);
}

static pthread_mutex_t gtypes_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Register a new type into the GObject class hierarchy, if it has not
   been registered already */
GType haskell_gi_register_gtype (GType parent, const char *name,
                                 GClassInitFunc class_init,
                                 GInstanceInitFunc instance_init)
{
  GType result;

  /* We lock here in order to make sure that we don't try to register
     the same type twice. */
  pthread_mutex_lock(&gtypes_mutex);
  result = g_type_from_name (name);

  if (result == 0) {
    /* Note that class_init and instance_init are HsFunPtrs, which we
       keep alive for the duration of the program. */
    GTypeQuery query;
    g_type_query (parent, &query);
    result = g_type_register_static_simple (parent, name,
                                            query.class_size, class_init,
                                            query.instance_size, instance_init,
                                            0);
  } else {
    /* Free the memory associated with the HsFunPtrs that we are
       given, to avoid a (small) memory leak. */
    hs_free_fun_ptr ((HsFunPtr)class_init);
    hs_free_fun_ptr ((HsFunPtr)instance_init);
  }
  pthread_mutex_unlock(&gtypes_mutex);

  return result;
}

static HsStablePtr duplicateStablePtr(HsStablePtr stable_ptr)
{
  return getStablePtr(deRefStablePtr(stable_ptr));
}

GType haskell_gi_StablePtr_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;

  if (g_once_init_enter (&g_define_type_id__volatile))
    {
      GType g_define_type_id =
        g_boxed_type_register_static (g_intern_static_string ("HaskellGIStablePtr"),
                                      duplicateStablePtr,
                                      hs_free_stable_ptr);

      g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
    }

  return g_define_type_id__volatile;
}

/* Release the FunPtr allocated for a Haskell signal handler */
void
haskell_gi_release_signal_closure (gpointer unused,
                                   GCClosure *closure)
{
  lock_log();
  dbg_log("Releasing a signal closure %p\n", closure->callback);

  hs_free_fun_ptr (closure->callback);

  dbg_log("\tDone.\n");
  unlock_log();
}

/* Check whether the given closure is floating */
gboolean
haskell_gi_g_closure_is_floating (GClosure *closure)
{
  return !!(closure->floating);
}

/* GParamSpec* types are registered as GObjects, but they do not have
   an exported type_init function. They only export CPP macros, so
   we have to provide our own. */
#define PARAM_TYPE(CamelCase, UPPERCASE) \
  GType haskell_gi_pspec_type_init_##CamelCase (void) { \
    return G_TYPE_##UPPERCASE;                          \
  }

PARAM_TYPE(ParamSpec, PARAM);
PARAM_TYPE(ParamSpecBoolean, PARAM_BOOLEAN);
PARAM_TYPE(ParamSpecBoxed, PARAM_BOXED);
PARAM_TYPE(ParamSpecChar, PARAM_CHAR);
PARAM_TYPE(ParamSpecDouble, PARAM_DOUBLE);
PARAM_TYPE(ParamSpecEnum, PARAM_ENUM);
PARAM_TYPE(ParamSpecFlags, PARAM_FLAGS);
PARAM_TYPE(ParamSpecFloat, PARAM_FLOAT);
PARAM_TYPE(ParamSpecGType, PARAM_GTYPE);
PARAM_TYPE(ParamSpecInt, PARAM_INT);
PARAM_TYPE(ParamSpecInt64, PARAM_INT64);
PARAM_TYPE(ParamSpecLong, PARAM_LONG);
PARAM_TYPE(ParamSpecObject, PARAM_OBJECT);
PARAM_TYPE(ParamSpecOverride, PARAM_OVERRIDE);
PARAM_TYPE(ParamSpecParam, PARAM_PARAM);
PARAM_TYPE(ParamSpecPointer, PARAM_POINTER);
PARAM_TYPE(ParamSpecString, PARAM_STRING);
PARAM_TYPE(ParamSpecUChar, PARAM_UCHAR);
PARAM_TYPE(ParamSpecUInt, PARAM_UINT);
PARAM_TYPE(ParamSpecUInt64, PARAM_UINT64);
PARAM_TYPE(ParamSpecULong, PARAM_ULONG);
PARAM_TYPE(ParamSpecUnichar, PARAM_UNICHAR);
PARAM_TYPE(ParamSpecVariant, PARAM_VARIANT);
/* The following is deprecated, ignore the warning that GLib raises. */
#undef GLIB_DEPRECATED_MACRO
#define GLIB_DEPRECATED_MACRO
PARAM_TYPE(ParamSpecValueArray, PARAM_VALUE_ARRAY);
