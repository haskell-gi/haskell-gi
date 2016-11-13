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

../dist-newstyle/build/build-tools-0.0.1/build/genBuildInfo/genBuildInfo $(./TravisPKGS.sh)

build $(./TravisPKGS.sh)
