#!/bin/bash

show_help () {
    echo "USAGE: $0 (list|deps) (fedora|ubuntu|ubuntu-ci)"
}

DEFAULT_TARGET="fedora"

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
        echo "Unknown target"
        show_help
        exit 1
esac

case "$ACTION" in
    list)
        cd "$BINDINGS_DIR"
        jq -r "select( $PKG_COND )"'| input_filename | split("/")[0]' */pkg.info 
        ;;
    deps)
        cd "$BINDINGS_DIR"
        jq -sr "[ .[] | ( $PKG_DEPS ) // [] ] | add | .[]" */pkg.info | sort | uniq
        ;;
    *)
        echo "Unknown action"
        show_help
        exit 1
esac
