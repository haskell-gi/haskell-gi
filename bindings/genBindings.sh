#!/bin/bash

set -e

generate()
{
    for modName in $@; do
	mkdir -p $modName
        pushd $modName > /dev/null
        rm -rf GI
        rm -f Setup.hs
        rm -f LICENSE
        rm -f cabal.config
        rm -f *.cabal
        mod=$(echo $modName | sed -e 's/-.*//')
        if test -f $mod*.gir;
        then
            echo "Found locally installed gir, using it."
            SEARCH_PATH="-s."
        fi
        if test -f $mod.overrides;
        then
            haskell-gi $SEARCH_PATH -o $mod.overrides $mod
        else
            haskell-gi $SEARCH_PATH $mod
        fi
        unset SEARCH_PATH
        popd > /dev/null
    done
}

generate Atk cairo Gdk GdkPixbuf Gio GIRepository GLib GObject Gtk Notify Pango Soup Vte JavaScriptCore-3.0 JavaScriptCore-4.0 WebKit WebKit2
