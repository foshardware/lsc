<!--
Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Libre Silicon Compiler

## Dependencies

### Build dependencies

- [stack](https://www.stackage.org/)


### Runtime dependencies

- [yices-smt2](http://yices.csl.sri.com/)


## Installation

`stack install`

## Usage

### Print usage

`lsc -h`  


### Create a legalized placement with replace

`replace < gcd_nontd_test.tcl`  
`lsc -dy -llibrary/nangate45/NangateOpenCellLibrary.lef gcd_nan45_nontd.def -odef > legalized.def`

### Detailed placement

`replace < gcd_nontd_test.tcl`  
`lsc -dp -llibrary/nangate45/NangateOpenCellLibrary.lef gcd_nan45_nontd.def -odef > placed.def`

### Create graphics from def file

`lsc -l sample/osu035.lef --output=svg sample/map9v3.def > result.svg`  


## Stacktraces

`stack build --profile && stack exec --profile -- lsc +RTS -xc -RTS`  


## Profiling

`stack build --profile && stack exec --profile -- lsc +RTS -p -hc -RTS`  


## Tests

`stack test`  

### Enable concurrency tests

Pass the argument `-j4` for enabling concurrency tests with 4 jobs to run simultaneosly.

`stack test --test-arguments -j4`

## References

### LSC.FM

- Fiduccia, et. al. (1982): A Linear-Time Heuristic for Improving Network Partitions [10.1109/dac.1982.1585498](https://doi.org/10.1109/dac.1982.1585498)
- Alpert, Huang, Kahng (1998): Multilevel Circuit Partitioning, [10.1109/dac.1997.597204](https://doi.org/10.1109/dac.1997.597204)

### LSC.FastDP

- Pan, Viswanathan, Chu (2005): An efficient and effective detailed placement algorithm [10.1109/iccad.2005.1560039](https://doi.org/10.1109/iccad.2005.1560039)

### LSC.Legalize

- Kahng, Markov, Reda (2004): On Legalization of Row-Based Placements [10.1145/988952.989004](https://doi.org/10.1145/988952.989004)

### LSC.SuffixTree

- Becher, Deymonnaz, Heiber (2013): Efficient repeat finding via suffix arrays

### LSC.UnionFind

- Tarjan (1975): Efficiency of a Good But Not Linear Set Union Algorithm [10.1145/321879.321884](https://doi.org/10.1145/321879.321884)

## Licensing:
This program is available as open source under the terms of the GPL-3.0-or-later. However, some elements are being licensed under CC0-1.0. For accurate information, please check individual files.

