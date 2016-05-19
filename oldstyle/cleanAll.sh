#!/bin/bash

set -e

clean()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
	rm -rf dist
        rm -f Setup.hs
        rm -f LICENSE
        rm -f cabal.config
	rm -f $mod.cabal
	rm -f Setup.hs
	popd > /dev/null
    done
}

clean $(./PKGS.sh)
