#!/bin/bash

set -eu

show_help () {
    echo "USAGE: $0 [list|cabal-files|deps] [fedora|ubuntu|ubuntu-ci]"
    echo "Query bindings/cabal files/package dependencies for the given distribution"
}

DEFAULT_TARGET="fedora"

if [ "${1:-}" = "--help" ] ; then
    show_help
    exit 0
fi

BINDINGS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ACTION="${1:-list}"
TARGET="${2:-$DEFAULT_TARGET}"

case "$TARGET" in
    fedora|ubuntu)
        PKG_DEPS=".distributionPackages.$TARGET"
        PKG_COND="$PKG_DEPS != null"
        ;;
    ubuntu-ci)
        PKG_DEPS='.distributionPackages.ubuntu, .distributionPackages."ubuntu-ci"'
        PKG_COND='.distributionPackages.ubuntu != null or .distributionPackages."ubuntu-ci" != null'
        ;;
    *)
        echo "Unknown target" >&2
        show_help >&2
        exit 1
esac

case "$ACTION" in
    list)
        cd "$BINDINGS_DIR"
        jq -r "select( $PKG_COND )"'| input_filename | split("/")[0]' */pkg.info 
        ;;
    cabal-files)
        cd "$BINDINGS_DIR"
        jq -r "select( $PKG_COND )"'| ( input_filename | split("/")[0] ) + "/" + .name + ".cabal"' */pkg.info
        ;;
    deps)
        cd "$BINDINGS_DIR"
        jq -sr "[ .[] | ( $PKG_DEPS ) // [] ] | add | .[]" */pkg.info | sort | uniq
        ;;
    *)
        echo "Unknown action" >&2
        show_help >&2
        exit 1
esac
