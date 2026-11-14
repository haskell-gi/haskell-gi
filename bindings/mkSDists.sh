#!/usr/bin/env bash

# Generate source distributions for the bindings in the "sdists"
# directory, ready to be uploaded to Hackage with upload.sh.
#
# USAGE: ./mkSDists.sh [binding-dir ...]
#
# By default all the bindings listed by PKGS.sh are packaged.

set -euo pipefail

BINDINGS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
SDIST_DIR="$BINDINGS_DIR/sdists"

cd "$BINDINGS_DIR"

if [ $# -gt 0 ]; then
    PKGS=("$@")
else
    mapfile -t PKGS < <(./PKGS.sh list)
fi

# Regenerate the .cabal files (and Setup.hs, LICENSE, ...) so that
# they agree with the current pkg.info files.
echo "Generating cabal files for ${#PKGS[@]} bindings..."
cabal run -v0 genBuildInfo -- "${PKGS[@]}"

rm -rf "$SDIST_DIR"
mkdir -p "$SDIST_DIR"

for pkg in "${PKGS[@]}"; do
    name=$(jq -r .name "$pkg/pkg.info")
    version=$(jq -r .version "$pkg/pkg.info")
    tarball="$SDIST_DIR/$name-$version.tar.gz"

    # Ignore the project, so that cabal only looks at the package in
    # the current directory.
    (cd "$pkg" && cabal sdist -v0 --ignore-project --output-directory="$SDIST_DIR")

    if [ ! -f "$tarball" ]; then
        echo "Error: expected $tarball to be created for $pkg" >&2
        exit 1
    fi
    echo "Created $(basename "$tarball")"
done

echo "Created ${#PKGS[@]} source distributions in $SDIST_DIR"
