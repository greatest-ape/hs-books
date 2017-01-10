#!/bin/sh

stack build --ghc-options='-O' && ./scripts/copy-cgi.sh && ./scripts/run.sh
