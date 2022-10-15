#!/usr/bin/env bash

cd ../dist-newstyle/sdist

rm -f gi-cairo-render-clock*
rm -f gi-cairo-render-sdl*

for sdist in gi-* haskell-gi-0*.tar.gz haskell-gi-base*; do
    echo "Uploading $sdist"
    result=$(cabal upload --publish $sdist)
    echo $result | grep -q "Package successfully published."
    if [ $? == 0 ]; then
        echo "OK"
        rm $sdist
    else
        echo $result | grep -q "already been uploaded"
        if [ $? == 0 ]; then
            echo "Already uploaded, OK"
            rm $sdist
        else
            echo "Failed to upload $sdist: $result"
            exit 1
        fi
    fi
done
