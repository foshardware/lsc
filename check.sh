#!/bin/sh


for i in lsc arrowgant
do
  stack test --pedantic --profile --ghc-options=-fprof-auto $i
done

