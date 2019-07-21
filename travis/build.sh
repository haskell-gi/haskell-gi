#!/bin/bash

set -e

if [ $TRAVIS_OS_NAME = 'osx' ]; then
    ./travis/build.osx.sh
else
    ./travis/build.linux.sh
fi
