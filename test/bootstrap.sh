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

bash build.sh
