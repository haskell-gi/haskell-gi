#!/bin/bash

set -eu

show_help () {
    echo "USAGE: $0 [fedora|ubuntu|ubuntu-ci]"
    echo "Updates cabal.project for the given distribution"
}

if [ "${1:-}" = "--help" ] ; then
    show_help
    exit 0
fi

TARGET="${1:-}"

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

sed -i.bak -e '/^\s*bindings\//d' cabal.project
./bindings/PKGS.sh cabal-files "$TARGET" | sed 's!^!          bindings/!' >> "$ROOT_DIR/cabal.project"
