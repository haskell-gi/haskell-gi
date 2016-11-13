#!/bin/bash

#################################
# Bindings to test in Travis CI #
#################################

unamestr=$(uname)
basever=$(ghc-pkg list base)

# Packages compatible with base >= 4.7
base47PKGS="GLib GObject Atk Gio cairo Pango PangoCairo GdkPixbuf Gdk Gtk"

# Packages compatible with base >= 4.8
# base48PKGS="Gst GstBase GstAudio GstVideo"

echo $basever | grep -q "4.7"
if [ $? == 0 ]; then
    echo $base47PKGS
else
    echo $base47PKGS # $base48PKGS
fi

