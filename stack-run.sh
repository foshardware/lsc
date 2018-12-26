#!/bin/sh

stack exec -- lsc -J -x -l tests/osu035.lef -b ../rocket-chip-verilog/rocket.blif +RTS -N6

# stack build && stack exec -- lsc-test || echo "WAIT IT FAILS!"

