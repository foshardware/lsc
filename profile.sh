#!/bin/sh

# stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"
# stack exec -- lsc +RTS -p

stack build --profile --ghc-options=-fprof-auto-top
stack exec -- lsc -b tests/picorv32.blif -l tests/osu035.lef -x +RTS -p

