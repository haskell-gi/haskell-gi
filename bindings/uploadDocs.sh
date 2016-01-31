#!/bin/bash

set -e

uploadDocs()
{
    for modName in $@; do
        pushd $modName > /dev/null
        neil docs -u=$USER
        popd > /dev/null
    done
}

uploadDocs $(cat PKGS)
