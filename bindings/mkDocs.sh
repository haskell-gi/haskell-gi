#!/usr/bin/env bash

# Build the Hackage documentation tarballs for the bindings in the
# "docs" directory, ready to be uploaded with uploadDocs.sh.
#
# USAGE: ./mkDocs.sh [binding-dir ...]
#
# By default the documentation for all the bindings listed by PKGS.sh
# is built. The documentation is built using the cabal.project at the
# root of the repository, so the bindings need to be listed there.

set -euo pipefail

BINDINGS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ROOT_DIR="$( dirname "$BINDINGS_DIR" )"
DOCS_DIR="$BINDINGS_DIR/docs"

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

NAMES=()
TARBALLS=()
for pkg in "${PKGS[@]}"; do
    NAMES+=("$(jq -r .name "$pkg/pkg.info")")
    TARBALLS+=("$(jq -r '.name + "-" + .version' "$pkg/pkg.info")-docs.tar.gz")
done

rm -rf "$DOCS_DIR"
mkdir -p "$DOCS_DIR"

# cabal writes the documentation tarballs to dist-newstyle. Remove
# any stale copies, so that we do not upload them by mistake.
for tarball in "${TARBALLS[@]}"; do
    rm -f "$ROOT_DIR/dist-newstyle/$tarball"
done

cd "$ROOT_DIR"

# Haddock can only link to identifiers in other packages if it can
# find their interface (.haddock) files. For the local packages, cabal
# looks for them where a regular haddock build puts them, while
# --haddock-for-hackage writes them somewhere else. So we first do a
# regular build for haskell-gi-base, haskell-gi and the bindings, and
# then the build for Hackage. Otherwise links to other packages end up
# as dangling links relative to the package's own documentation.
cabal haddock haskell-gi-base haskell-gi "${NAMES[@]}"
cabal haddock --haddock-for-hackage "${NAMES[@]}"

for i in "${!PKGS[@]}"; do
    tarball="$ROOT_DIR/dist-newstyle/${TARBALLS[$i]}"
    if [ ! -f "$tarball" ]; then
        echo "Error: expected $tarball to be created for ${PKGS[$i]}" >&2
        exit 1
    fi
    cp "$tarball" "$DOCS_DIR/"
done

echo "Created ${#PKGS[@]} documentation tarballs in $DOCS_DIR"
