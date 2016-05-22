#!/bin/bash

set -e

clean()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
	rm -rf dist
        rm -f *.cabal
        rm -f Setup.hs
        rm -f LICENSE
	popd > /dev/null
    done
}

clean $(./PKGS.sh)
