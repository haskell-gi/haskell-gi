#!/bin/bash
basever=$(ghc-pkg list base)
unamestr=$(uname)

# Packages compatible with base >= 4.7
base47PKGS="GLib GObject Atk Gio Secret cairo Pango PangoCairo GdkPixbuf Gdk Gtk GtkSource Soup GIRepository Poppler Vte Notify OSTree Ggit xlib GdkX11 Dbusmenu DbusmenuGtk3 Wnck"
# Packages compatible with base >= 4.8
base48PKGS="Gst GstBase GstAudio GstVideo GstTag GstPbutils"

if [[ "$unamestr" == 'Darwin' ]]; then
    # Only present on OS X
    extraPKGS="GtkosxApplication"
else
    # WebKit2 is broken for MacPorts with Quartz enabled right now
    extraPKGS="JavaScriptCore-4.0 WebKit2 WebKit2WebExtension"
fi

echo $basever | grep -q "4.7"
if [ $? == 0 ]; then
    echo $base47PKGS $extraPKGS
else
    echo $base47PKGS $base48PKGS $extraPKGS
fi
