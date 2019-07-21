#!/bin/bash

set -e

if [ $TRAVIS_OS_NAME = 'osx' ]; then
    echo "Not needed in OSX!"
else
    ./travis/before_install.linux.sh
fi
