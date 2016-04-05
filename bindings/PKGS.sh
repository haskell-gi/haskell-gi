#!/bin/bash
basever=$(ghc-pkg list base)

# Packages compatible with base >= 4.7
base47PKGS="GLib GObject Atk Gio Soup cairo Pango GdkPixbuf Gdk Gtk JavaScriptCore-4.0 WebKit2 WebKit2WebExtension GtkSource GIRepository Poppler JavaScriptCore-3.0 WebKit Vte Notify"
# Packages compatible with base >= 4.8
base48PKGS="Gst GstBase GstAudio GstVideo"

echo $basever | grep -q "4.7"
if [ $? == 0 ]; then
    echo $base47PKGS
else
    echo $base47PKGS $base48PKGS
fi
