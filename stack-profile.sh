#!/bin/sh

# stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- lsc +RTS -p

stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc-test +RTS -p

# stack build --profile --ghc-options=-fprof-auto-top && stack exec -- lsc-test +RTS -p

