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

sed -i.bak -e '/^\s*bindings\//d' cabal.project
if ! $(printf "9.2\n${GHC_VERSION}" | sort -C -V); then
	# These examples use the OverloadedRecordDot extension, which is not
	# supported by GHC versions earlier than 9.2.
	sed -i.bak2 -E -e '/(gtk4|webkit)-example/d' cabal.project
fi
./bindings/PKGS.sh cabal-files "$TARGET" | sed 's!^!          bindings/!' >> "$ROOT_DIR/cabal.project"
