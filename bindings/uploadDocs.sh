#!/bin/bash

set -e

uploadDocs()
{
    for modName in $@; do
        pushd $modName > /dev/null
        cabal upload -d
        popd > /dev/null
    done
}

uploadDocs $(cat PKGS)
