#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        cabal configure
        cabal install
        popd > /dev/null
    done
}

build GLib JavaScriptCore GObject Atk Gio Soup cairo Pango GdkPixbuf Gdk Gtk WebKit Vte Notify
