#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        cabal configure
        cabal build
        cabal install
        popd > /dev/null
    done
}

build GLib GObject Atk Gio cairo Pango GdkPixbuf Gdk Gtk Vte Notify
