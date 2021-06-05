#!/bin/bash

stack build --ghc-options "-O2" && bash scripts/copy-cgi.sh
