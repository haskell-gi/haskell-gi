#!/bin/bash

set -e

cd ..

DOCSDIR=dist-newstyle

rm $DOCSDIR/*-docs.tar.gz
cabal new-haddock --haddock-for-hackage all
for doc in $DOCSDIR/gi-*-docs.tar.gz; do
    cabal upload --publish -d $doc
done
