#!/bin/bash

set -e

generate()
{
    for mod in $@; do
        if test -f $mod/$mod.overrides;
        then
            haskell-gi -o $mod/$mod.overrides $mod
        else
            haskell-gi $mod
        fi
	mkdir -p $mod
	rm -rf $mod/GI
	mv GI $mod
    done
}

generate Atk cairo Gdk GdkPixbuf Gio GLib GObject Gtk Notify Pango Vte
