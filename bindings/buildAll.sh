#!/bin/bash

set -e

build()
{
    for mod in $@; do
        pushd $mod > /dev/null
        cabal clean
        cabal configure
	cabal build
        cabal install
        cabal sdist
        cp dist/*.tar.gz ../sdists
        popd > /dev/null
    done
}

mkdir -p sdists
rm -f sdists/*

build GLib JavaScriptCore-3.0 JavaScriptCore-4.0 GObject GIRepository Atk Gio Soup cairo Pango GdkPixbuf Gdk Gtk WebKit WebKit2 Vte Notify
