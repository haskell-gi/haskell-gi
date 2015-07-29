#!/bin/bash

set -e

generate()
{
    for mod in $@; do
	mkdir -p $mod
        pushd $mod > /dev/null
        rm -rf GI
        rm -f Setup.hs
        rm -f cabal.config
        rm -f *.cabal
        if test -f $mod*.typelib;
        then
            echo "Found locally installed typelib, using it."
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

generate Atk cairo Gdk GdkPixbuf Gio GLib GObject Gtk Notify Pango Soup Vte JavaScriptCore WebKit
