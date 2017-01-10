#!/bin/sh

# This is problematic on macOS but might work on Linux
# See https://ro-che.info/articles/2015-10-26-static-linking-ghc
stack build --ghc-options='-optl-static -optl-pthread' --force-dirty &&
    ./scripts/copy-cgi.sh
