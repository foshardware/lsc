#!/bin/sh

stack build && stack exec -- lsc-test || echo "WAIT IT FAILS!"

