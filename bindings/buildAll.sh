#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        cabal clean
        cabal configure
        cabal install
        cabal sdist
        cp dist/*.tar.gz ../sdists
        popd > /dev/null
    done
}

mkdir -p sdists
rm -f sdists/*

build GLib JavaScriptCore GObject Atk Gio Soup cairo Pango GdkPixbuf Gdk Gtk WebKit Vte Notify
