#!/bin/bash

set -e

clean()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
	rm -rf dist
	popd > /dev/null
    done
}

clean $(./PKGS.sh)
