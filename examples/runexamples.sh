#!/bin/bash

set -e
set -x

cabal clean
haskell-gi -a Gtk WebKit
haskell-gi -c Gtk WebKit
cabal configure
time cabal build

export G_SLICE="debug-blocks"
export MALLOC_CHECK_=2
#export HASKELL_GI_DEBUG_MEM=1
dist/build/SimpleBrowser/SimpleBrowser
