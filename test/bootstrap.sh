#!/bin/bash

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

mkdir -p GI

generate \
    -m GI.GLib \
    GLib
generate \
    -i GLib \
    -m GI.GObject \
    GObject
generate \
    -i GLib \
    -i GObject \
    -m GI.Cairo \
    cairo
generate \
    -i GLib \
    -i GObject \
    -m GI.Gio \
    Gio
generate \
    -i GLib \
    -i GObject \
    -m GI.Pango \
    Pango
generate \
    -i GLib \
    -i GObject \
    -m GI.Atk \
    Atk
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -m GI.GdkPixbuf \
    GdkPixbuf
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Pango \
    -m GI.Gdk \
    Gdk
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Gdk \
    -i Pango \
    -i Atk \
    -m GI.Gtk \
    Gtk

LDFLAGS=$(echo $(pkg-config --libs gobject-2.0) | sed -e "s/-l/--lflag=-l/g")

ghc -c hsgclosure.c $(pkg-config --cflags gobject-2.0)
hsc2hs GI/Utils/BasicTypes.hsc $(pkg-config --cflags gobject-2.0)
hsc2hs GI/Utils/Properties.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS
ghc --make testGtk.hs hsgclosure.o $(pkg-config --libs gtk+-3.0)
