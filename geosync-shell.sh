#!/usr/bin/env bash
# Run to enter a shell that contains all geosync dependencies (e.g java)
# Notes
# + share m2 and gitlabs for clojure deps caching.

guix time-machine \
     --channels=channels.scm \
     -- shell \
     --manifest=manifest.scm \
     --container \
     --network \
     --share=$HOME/.m2 \
     --share=$HOME/.gitlibs \
     "$@"
