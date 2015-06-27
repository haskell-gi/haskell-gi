#!/bin/bash

set -e

generate()
{
    for mod in $@; do
	mkdir -p $mod
        pushd $mod > /dev/null
        rm -rf GI
        rm -f Setup.hs
        rm -f *.cabal
        if test -f $mod.overrides;
        then
            haskell-gi -o $mod.overrides $mod
        else
            haskell-gi $mod
        fi
        popd > /dev/null
    done
}

generate Atk cairo Gdk GdkPixbuf Gio GLib GObject Gtk Notify Pango Soup Vte JavaScriptCore WebKit
