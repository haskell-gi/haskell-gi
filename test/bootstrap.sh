#!/bin/bash

set -e

generate()
{
    for mod in $@; do
        if test -f $mod.config;
        then
            haskell-gi -c $mod.config $mod
        else
            haskell-gi $mod
        fi
    done

    haskell-gi -a $@
}

mkdir -p GI

generate Atk cairo Gdk GdkPixbuf Gio GLib GObject Gtk Notify Pango Vte

bash build.sh
