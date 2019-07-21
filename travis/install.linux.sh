#!/bin/bash

set -e

sudo apt-get update
sudo apt-get install -y libgtk-3-0
sudo apt-get install -y cabal-install-$CABALVER ghc-$GHCVER happy-1.19.5 alex-3.1.7
sudo apt-get install -y libgirepository1.0-dev gir1.2-javascriptcoregtk-4.0 libwebkit2gtk-4.0-dev libpoppler-glib-dev libvte-2.91-dev libgtksourceview-3.0-dev libgstreamer-plugins-base1.0-dev libsecret-1-dev libnotify-dev libdbusmenu-gtk3-dev libwnck-3-dev
