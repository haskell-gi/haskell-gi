# How to build the examples

    # Make sure you have gstreamer-video-1.0 installed
    # on Ubuntu use this command
    apt-get install libgstreamer-plugins-base1.0-dev

    cabal sandbox init

    cabal install happy alex gi-gtk

    cabal install --dependencies-only

    ./runexamples.sh
