#!/bin/bash

###################################################################
# Make sure to run ./bootstrap.sh first to autogenerate the files #
# being compiled here.                                            #
###################################################################

set -e
set -x

LDFLAGS=$(echo $(pkg-config --libs gobject-2.0) | sed -e "s/-l/--lflag=-l/g")

ghc -Wall -c hsgclosure.c $(pkg-config --cflags gobject-2.0)
hsc2hs GI/Utils/BasicConversions.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
hsc2hs GI/Utils/BasicTypes.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
hsc2hs GI/Utils/Properties.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
hsc2hs GI/Utils/GValue.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
hsc2hs GI/Utils/Utils.hsc $(pkg-config --cflags gobject-2.0) $LDFLAGS -t $PWD/template-hsc.h
ghc --make testGtk.hs hsgclosure.o $(pkg-config --libs gtk+-3.0)

export G_SLICE="debug-blocks"
export MALLOC_CHECK_=2
./testGtk
