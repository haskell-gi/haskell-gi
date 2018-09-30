#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
        rm -rf dist
        cabal v1-configure
	cabal v1-build
        cabal v1-install --force-reinstalls
        popd > /dev/null
    done
}

rm -rf sdists

build $(./PKGS.sh)
