#!/bin/bash

set -e

uploadDocs()
{
    for modName in $@; do
        pushd $modName > /dev/null
	rm -rf GI
	cabal clean
	cabal upload --publish -d
        popd > /dev/null
    done
}

uploadDocs $(./PKGS.sh)
