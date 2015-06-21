#!/bin/bash

# This script generates the bindings and compiles testGtk. If you want
# to use haskell-gi in your program it is probably better to install
# the cabalized bindings in the toplevel "bindings" directory.

set -e

generate()
{
    for mod in $@; do
	OVERRIDES=../bindings/$mod/$mod.overrides
        if test -f $OVERRIDES;
        then
            haskell-gi -n -o $OVERRIDES $mod
        else
            haskell-gi -n $mod
        fi
    done

    haskell-gi -a $@
    haskell-gi -c $@
}

mkdir -p GI

generate Atk cairo Gdk GdkPixbuf Gio GLib GObject Gtk Notify Pango Vte

bash build.sh
