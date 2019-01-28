#!/bin/sh

# stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- lsc +RTS -p

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc-test +RTS -p

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc -J -u ../rocket-chip-verilog/freechips.rocketchip.system.DefaultConfig.v +RTS -p

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc -J -x -l sample/osu035.lef -b ../rocket-chip-verilog/rocket.blif +RTS -p -N6

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc -l sample/osu035.lef -b sample/picorv32.blif -c svg -d -j 2 +RTS -p -N2 > result1.svg

stack build --pedantic --profile --ghc-options=-fprof-auto && stack exec -- lsc -l sample/osu035.lef -b sample/carryripple.blif -c svg -d -j 4 --smt yices +RTS -p > result1.svg

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc -l sample/osu035.lef -b sample/and2.blif -c svg -d -j 2 +RTS -p -N2 > result1.svg

# stack build --profile --ghc-options=-fprof-auto-top && stack exec -- lsc-test +RTS -p

