#!/bin/bash
unamestr=$(uname)

# Packages compatible with base >= 4.7
base47PKGS="GLib GObject Atk Gio Secret Soup cairo Pango PangoCairo GdkPixbuf Gdk Gtk GtkSource GIRepository Poppler JavaScriptCore-3.0 WebKit Vte Notify OSTree"
# Packages compatible with base >= 4.8
base48PKGS="Gst GstBase GstAudio GstVideo"

# Only present on OS X
osxPKGS="GtkosxApplication"
# WebKit2 is broken for MacPorts with Quartz enabled right now
webkit2PKGS="JavaScriptCore-4.0 WebKit2 WebKit2WebExtension"

echo $base47PKGS $base48PKGS $osxPKGS $webkit2PKGS
