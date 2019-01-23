#!/bin/sh

stack build --profile --ghc-options=-fprof-auto && stack test

