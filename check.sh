#!/bin/sh


for i in lsc arrowgant
do
  stack test --profile --ghc-options=-fprof-auto $i
done

