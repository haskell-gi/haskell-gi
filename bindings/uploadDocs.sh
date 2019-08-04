#!/bin/bash

set -e

cd ..

DOCSDIR=dist-newstyle
#TMP=$(mktemp)

rm -rf $DOCSDIR/*-docs.tar.gz
cabal new-haddock --haddock-for-hackage --haddock-css=ocean --allow-newer all
cp cabal.project $TMP
cp Gtk4.cabal.project cabal.project
cabal new-haddock --haddock-for-hackage --haddock-css=ocean --allow-newer all
mv $TMP cabal.project

cabal upload --publish -d $DOCSDIR/haskell-gi-0.*-docs.tar.gz
cabal upload --publish -d $DOCSDIR/haskell-gi-base-0.*-docs.tar.gz
for doc in $DOCSDIR/gi-*-docs.tar.gz; do
    cabal upload --publish -d $doc
done
