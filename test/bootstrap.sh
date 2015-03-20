#!/bin/bash

set -e

generate()
{
    for mod in $@; do
        if test -f $mod.overrides;
        then
            haskell-gi -o $mod.overrides $mod
        else
            haskell-gi $mod
        fi
    done

    haskell-gi -a $@
}

mkdir -p GI

generate Atk cairo Gdk GdkPixbuf Gio GLib GObject Gtk Notify Pango Vte

bash build.sh
