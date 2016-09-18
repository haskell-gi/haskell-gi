#!/bin/bash

set -e
set -x

cabal clean
haskell-gi -l Gtk WebKit2 Gst GstBase GstAudio GstVideo
haskell-gi -c Gtk WebKit2 Gst GstBase GstAudio GstVideo
cabal configure
time cabal build

export G_SLICE="debug-blocks"
export MALLOC_CHECK_=2
export MALLOC_PERTURB_=$(($RANDOM % 255 + 1))
#export HASKELL_GI_DEBUG_MEM=1
dist/build/SimpleBrowser/SimpleBrowser
dist/build/Cairo/Cairo
