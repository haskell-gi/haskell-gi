
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
    GLib > GI/GLib.chs
generate \
    -i GLib \
    GObject > GI/GObject.chs
generate \
    -i GLib \
    -i GObject \
    cairo > GI/Cairo.chs
generate \
    -i GLib \
    -i GObject \
    Gio > GI/Gio.chs
generate \
    -i GLib \
    -i GObject \
    Pango > GI/Pango.chs
generate \
    -i GLib \
    -i GObject \
    Atk > GI/Atk.chs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    GdkPixbuf > GI/GdkPixbuf.chs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Pango \
    Gdk > GI/Gdk.chs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Gdk \
    -i Pango \
    -i Atk \
    Gtk > GI/Gtk.chs
