#!/bin/bash

###################################################################
# Make sure to run ./bootstrap.sh first to autogenerate the files #
# being compiled here.                                            #
###################################################################

set -e
set -x

ghc --make -Wall testGtk.hs $(pkg-config --libs gtk+-3.0)

export G_SLICE="debug-blocks"
export MALLOC_CHECK_=2
./testGtk
