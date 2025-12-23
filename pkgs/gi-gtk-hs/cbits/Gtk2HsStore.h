#ifndef __GTK2HS_STORE_H__
#define __GTK2HS_STORE_H__

#include <gtk/gtk.h>

#include "Data/GI/Gtk/ModelView/CustomStore_stub.h"

G_BEGIN_DECLS

#define GTK2HS_TYPE_STORE                 (gtk2hs_store_get_type ())
#define GTK2HS_STORE(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK2HS_TYPE_STORE, Gtk2HsStore))
#define GTK2HS_STORE_CLASS(klass)         (G_TYPE_CHECK_CLASS_CAST ((klass), GTK2HS_TYPE_STORE, Gtk2HsStoreClass))
#define GTK2HS_IS_STORE(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK2HS_TYPE_STORE))
#define GTK2HS_IS_STORE_CLASS(klass)      (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK2HS_TYPE_STORE))
#define GTK2HS_STORE_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK2HS_TYPE_STORE, Gtk2HsStoreClass))

typedef struct _Gtk2HsStore       Gtk2HsStore;
typedef struct _Gtk2HsStoreClass  Gtk2HsStoreClass;

struct _Gtk2HsStore
{
  GObject parent;

  /*< private >*/
  HsStablePtr     impl;        /* a StablePtr CustomStore */
  HsStablePtr     priv;        /* a StablePtr to private data */

  gint            stamp;       /* Random integer to check whether an iter belongs to our model */
};

struct _Gtk2HsStoreClass
{
  GObjectClass parent_class;
};


GType             gtk2hs_store_get_type (void) G_GNUC_CONST;
Gtk2HsStore      *gtk2hs_store_new (HsStablePtr, HsStablePtr);
HsStablePtr       gtk2hs_store_get_impl  (Gtk2HsStore *);
HsStablePtr       gtk2hs_store_get_priv  (Gtk2HsStore *);
gint              gtk2hs_store_get_stamp (Gtk2HsStore *);
void              gtk2hs_store_increment_stamp (Gtk2HsStore *);

G_END_DECLS

#endif /* __GTK2HS_STORE_H__ */
