#!/bin/bash

set -e

if [ $TRAVIS_OS_NAME = 'osx' ]; then
    ./travis/install.osx.sh
else
    export PATH=$HOME/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/alex/3.1.7/bin:/opt/happy/1.19.5/bin:$PATH
    ./travis/install.linux.sh
fi

cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"

pushd bindings
cabal new-update
popd
