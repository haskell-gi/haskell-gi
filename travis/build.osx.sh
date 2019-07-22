#!/bin/bash

set -e

export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig:$PKG_CONFIG_PATH

mv osx.cabal.project cabal.project
echo "documentation: True" > cabal.project.local

pushd bindings
cabal new-run genBuildInfo $(./OSX-PKGS.sh)
popd

cabal new-build all
cabal new-test

cabal new-sdist gi-gtkosxapplication
cabal new-haddock --haddock-for-hackage gi-gtkosxapplication
