#!/bin/bash

set -e

dump()
{
    for modName in $@; do
        echo "Dumping $modName"

	mkdir -p $modName
        pushd $modName > /dev/null
        mod=$(echo $modName | sed -e 's/-.*//')
        if test -f $mod*.gir;
        then
            echo "Found locally installed gir, using it."
            SEARCH_PATH="-s."
        fi
        if test -f $mod.overrides;
        then
            haskell-gi -d $SEARCH_PATH -o $mod.overrides $mod > ../dumps/$modName.dump
        else
            haskell-gi -d $SEARCH_PATH $mod > ../dumps/$modName.dump
        fi
        unset SEARCH_PATH
        popd > /dev/null
    done
}

mkdir -p dumps
rm -f dumps/*

dump $(cat PKGS)
