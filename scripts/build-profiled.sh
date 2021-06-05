#!/bin/bash

stack build --executable-profiling --ghc-options="-fprof-auto -rtsopts"
