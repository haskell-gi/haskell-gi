#!/usr/bin/env bash

set -e

cd ..

mv cabal.project .cabal.project.bak
ln -s Gtk3.cabal.project cabal.project
cabal sdist all
ln -sf Gtk4.cabal.project cabal.project
cabal sdist all
mv -f .cabal.project.bak cabal.project
