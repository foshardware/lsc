<!--
Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Libre Silicon Compiler

## Dependencies

### Build dependencies

- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)


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

### Set number of capabilities

`lsc +RTS -N8 -RTS`  


## Stacktraces

`stack build --profile && stack exec --profile -- lsc +RTS -xc -RTS`  


## Profiling

`stack build --profile && stack exec --profile -- lsc +RTS -p -hc -RTS`  


## Debug build

`stack build --flag lsc:debug && stack exec -- lsc`  


## Tests

`stack test`  

### Enable concurrency tests

Pass the argument `-j4` for enabling concurrency tests with 4 jobs to run simultaneosly.

`stack test --test-arguments -j4`

## Licensing

This program is available as open source under the terms of the GPL-3.0-or-later. However, some elements are being licensed under CC0-1.0. For accurate information, please check individual files.

## References

### LSC.FM

- Fiduccia, et. al. (1982): A Linear-Time Heuristic for Improving Network Partitions [doi:10.1109/dac.1982.1585498](https://doi.org/10.1109/dac.1982.1585498)
- Alpert, Huang, Kahng (1998): Multilevel Circuit Partitioning, [doi:10.1109/dac.1997.597204](https://doi.org/10.1109/dac.1997.597204)

### LSC.FastDP

- Pan, Viswanathan, Chu (2005): An efficient and effective detailed placement algorithm [doi:10.1109/iccad.2005.1560039](https://doi.org/10.1109/iccad.2005.1560039)

### LSC.GlobalRouting

- Cong, Preas (1992): A new algorithm for standard cell global routing [doi:10.1016/0167-9260(92)90010-v](https://doi.org/10.1016/0167-9260%2892%2990010-v)

### LSC.Legalize

- Kahng, Markov, Reda (2004): On Legalization of Row-Based Placements [doi:10.1145/988952.989004](https://doi.org/10.1145/988952.989004)

### LSC.SegmentTree

- Bentley (1977): Solutions to Klee's rectangle problems. Unpublished manuscript

### LSC.SuffixTree

- Becher, Deymonnaz, Heiber (2013): Efficient repeat finding via suffix arrays [arXiv:1304.0528](https://arxiv.org/abs/1304.0528)

### LSC.UnionFind

- Tarjan (1975): Efficiency of a Good But Not Linear Set Union Algorithm [doi:10.1145/321879.321884](https://doi.org/10.1145/321879.321884)

## To do

- Row spacing
  - a posteriori equal to row density

- LSC
  - revisit stage logic

- Cell flipping
  - [doi:10.1016/S0166-218X(98)00114-0](https://doi.org/10.1016/S0166-218X%2898%2900114-0)

- Pin permutations
  - [doi:10.1109/iccd.1992.276294](https://doi.org/10.1109/iccd.1992.276294)

