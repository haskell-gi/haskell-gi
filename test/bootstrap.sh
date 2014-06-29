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

generate Gtk Vte

mkdir -p GI/Utils

if [ x"$HASKELL_GI_BUILD_DIR" = x ]; then
    HASKELL_GI_BUILD_DIR=".."
fi
cp "$HASKELL_GI_BUILD_DIR"/GI/Utils/* GI/Utils

LDFLAGS=$(echo $(pkg-config --libs gobject-2.0) | sed -e "s/-l/--lflag=-l/g")

ghc -c hsgclosure.c $(pkg-config --cflags gobject-2.0)
hsc2hs GI/Utils/BasicTypes.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
hsc2hs GI/Utils/Properties.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
hsc2hs GI/Utils/GValue.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
ghc --make testGtk.hs hsgclosure.o $(pkg-config --libs gtk+-3.0)
