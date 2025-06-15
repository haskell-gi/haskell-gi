#!/usr/bin/env bash

set -eu

show_help () {
    echo "USAGE: $0 [fedora|ubuntu|ubuntu-ci] ghc-version"
    echo "Updates cabal.project for the given distribution and ghc version"
}

if [ "${1:-}" = "--help" ] ; then
    show_help
    exit 0
fi

TARGET="${1:-}"
GHC_VERSION="${2:-}"

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

sed -i.bak -E -e '/^\s*bindings\/[a-zA-Z0-9_\.\-]+(\/compat)?\/gi/d' cabal.project
if ! $(printf "9.2\n${GHC_VERSION}" | sort -C -V); then
    # These examples use the OverloadedRecordDot extension, which is not
    # supported by GHC versions earlier than 9.2.
    sed -i.bak2 -E -e '/(gtk4|webkit)-example/d' cabal.project
    sed -i.bak3 -E -e '/HListStore/d' cabal.project
fi
if ! $(printf "9.6\n${GHC_VERSION}" | sort -C -V); then
    # This example uses OverloadedRecordDot, and additionally
    # “generalised” overloaded labels of the type #"duplicated.text",
    # which are only supported starting with GHC 9.6.
    sed -i.bak4 -E -e '/declarative-gtk-example/d' cabal.project
fi
./bindings/PKGS.sh cabal-files "$TARGET" | sed 's!^!          bindings/!' >> "$ROOT_DIR/cabal.project"
