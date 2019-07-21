#!/bin/bash

set -e

export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig:$PKG_CONFIG_PATH

pushd bindings
cabal new-run genBuildInfo $(./OSX-PKGS.sh)
popd

cabal new-build --project-file=osx.cabal.project all
cabal new-test --project-file=osx.cabal.project
