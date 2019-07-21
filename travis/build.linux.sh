#!/bin/bash

set -e

export PATH=$HOME/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/alex/3.1.7/bin:/opt/happy/1.19.5/bin:$PATH
export HASKELL_GI_TYPELIB_SEARCH_PATH=/usr/lib/x86_64-linux-gnu/girepository-1.0

# The version of Ubuntu in Travis does not have libhandy, libggit or libostree, and and its gtk version is ancient, so many things do not build anymore
grep -Ev "(Handy|OSTree|haskell-gi-test|Ggit|webkit-example)" cabal.project > cabal.project.new; mv cabal.project.new cabal.project
sed 's/Handy//;s/OSTree//;s/Ggit//;s/Graphene//;s/Gdk-4.0//;s/Gsk//;s/Gtk-4.0//' bindings/PKGS.sh > PKGS.new; mv PKGS.new bindings/PKGS.sh

pushd bindings
cabal new-run genBuildInfo $(bash PKGS.sh)
popd

cabal new-build all
cabal new-test
