#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
        rm -rf dist
        cabal new-build *.cabal
        popd > /dev/null
    done
}

genBuildInfo=$(find ../dist-newstyle -type f -executable -iname genBuildInfo)

$genBuildInfo $(./TravisPKGS.sh)

build $(./TravisPKGS.sh)
