
prefixes="
 -pGLib=g
 -pGObject=g
 -pGio=g
 -pGdkPixbuf=gdk
 "
renames="
 -r Array=GArray_
 -r HashTable=HashTable_
 -r List=List_
 -r SList=SList_
 "

set -e
set -x

generate()
{
    haskell-gi $prefixes $renames "$@"
 }

generate \
    GLib > GLib.hs
generate \
    -i GLib \
    GObject > GObject.hs
generate \
    -i GLib \
    -i GObject \
    cairo > Cairo.hs
generate \
    -i GLib \
    -i GObject \
    Gio > Gio.hs
generate \
    -i GLib \
    -i GObject \
    Pango > Pango.hs
generate \
    -i GLib \
    -i GObject \
    Atk > Atk.hs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    GdkPixbuf > GdkPixbuf.hs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Pango \
    Gdk > Gdk.hs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Gdk \
    -i Pango \
    -i Atk \
    Gtk > Gtk.hs
ghc --make Gtk
