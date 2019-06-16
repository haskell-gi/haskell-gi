#!/bin/bash

set -e

cd ..

DOCSDIR=dist-newstyle

rm -rf $DOCSDIR/*-docs.tar.gz
cabal new-haddock --haddock-for-hackage all
cabal upload --publish -d $DOCSDIR/haskell-gi-0.*-docs.tar.gz
cabal upload --publish -d $DOCSDIR/haskell-gi-base-0.*-docs.tar.gz
for doc in $DOCSDIR/gi-*-docs.tar.gz; do
    cabal upload --publish -d $doc
done
