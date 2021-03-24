#!/usr/bin/env bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        cabal sdist
        cp dist/*.tar.gz ../sdists
        popd > /dev/null
    done
}

mkdir -p sdists
rm -f sdists/*

build $(./AllPKGS.sh)
