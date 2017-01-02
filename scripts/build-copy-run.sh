#!/bin/sh

stack build && ./scripts/copy-cgi.sh && ./scripts/run.sh
