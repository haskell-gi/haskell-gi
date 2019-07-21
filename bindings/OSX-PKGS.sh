#!/bin/bash
basever=$(ghc-pkg list base)
unamestr=$(uname)

# Packages compatible with base >= 4.7
PKGS="GLib GObject Atk Gio cairo Pango PangoCairo GdkPixbuf Gdk-3.0 Gtk-3.0 GtkosxApplication"

echo $PKGS
