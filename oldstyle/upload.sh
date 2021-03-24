#!/usr/bin/env bash

for sdist in sdists/*; do
    echo "Uploading $sdist"
    result=$(cabal upload $sdist)
    echo $result | grep -q "Ok$"
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
