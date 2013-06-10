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
    GLib > GI/GLib.hs
generate \
    -i GLib \
    GObject > GI/GObject.hs
generate \
    -i GLib \
    -i GObject \
    cairo > GI/Cairo.hs
generate \
    -i GLib \
    -i GObject \
    Gio > GI/Gio.hs
generate \
    -i GLib \
    -i GObject \
    Pango > GI/Pango.hs
generate \
    -i GLib \
    -i GObject \
    Atk > GI/Atk.hs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    GdkPixbuf > GI/GdkPixbuf.hs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Pango \
    Gdk > GI/Gdk.hs
generate \
    -i GLib \
    -i GObject \
    -i Gio \
    -i cairo \
    -i GdkPixbuf \
    -i Gdk \
    -i Pango \
    -i Atk \
    Gtk > GI/Gtk.hs

ghc -c hsgclosure.c $(pkg-config --cflags gobject-2.0)
hsc2hs-ghc GI/Utils/BasicTypes.hsc $(pkg-config --cflags gobject-2.0)
ghc --make testGtk.hs hsgclosure.o $(pkg-config --libs gtk+-3.0)
