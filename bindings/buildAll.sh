#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
        rm -rf dist
        cabal configure
	cabal build
        cabal install --force-reinstalls
        popd > /dev/null
    done
}

rm -rf sdists

build $(./PKGS.sh)
