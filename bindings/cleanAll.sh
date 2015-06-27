#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        rm -rf GI
	rm -rf dist
	rm -f $mod.cabal
	rm -f Setup.hs
	popd > /dev/null
    done
}

build GLib GObject Atk Gio Soup cairo Pango GdkPixbuf Gdk Gtk Vte Notify
