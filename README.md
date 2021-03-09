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


### Custom setup

For any custom build, e. g. by using cabal-install, you will need at least ghc `8.0.1`.  


### GLPK optimizations

`stack install --flag=lsc:glpk`  

Use the GLPK (GNU Linear Programming Kit) for solving integer programs.

GLPK optimizations are necessary for:

- Cell flipping


## Usage

### Print usage

`lsc -h`  


### Create a detailed placement from replace

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

`stack build --flag=lsc:debug && stack exec -- lsc`  


## Tests

`stack test`  

### Enable concurrency tests

Pass the argument `-j4` for enabling concurrency tests with 4 jobs to run simultaneosly.

`stack test --test-arguments -j4`

## Licensing

This program is available as open source under the terms of the GPL-3.0-or-later. However, some elements are being licensed under CC0-1.0. For accurate information, please check individual files.

## References

### LSC.CellFlipping

- Boros, Hammer, Minoux, Rader (1999): Optimal cell flipping to minimize channel density in VLSI design and pseudo-Boolean optimzation [doi:10.1016/S0166-218X(98)00114-0](https://doi.org/10.1016/S0166-218X%2898%2900114-0)

### LSC.Deque

- Okasaki (1998): Purely Functional Data Structures [doi:10.1017/cbo9780511530104](https://doi.org/10.1017/cbo9780511530104)

### LSC.FM

- Fiduccia, et. al. (1982): A Linear-Time Heuristic for Improving Network Partitions [doi:10.1109/dac.1982.1585498](https://doi.org/10.1109/dac.1982.1585498)
- Alpert, Huang, Kahng (1998): Multilevel Circuit Partitioning, [doi:10.1109/dac.1997.597204](https://doi.org/10.1109/dac.1997.597204)

### LSC.FastDP

- Pan, Viswanathan, Chu (2005): An efficient and effective detailed placement algorithm [doi:10.1109/iccad.2005.1560039](https://doi.org/10.1109/iccad.2005.1560039)

### LSC.GlobalRouting

- Cong, Preas (1992): A new algorithm for standard cell global routing [doi:10.1016/0167-9260(92)90010-v](https://doi.org/10.1016/0167-9260%2892%2990010-v)

### LSC.Legalize

- Kahng, Markov, Reda (2004): On Legalization of Row-Based Placements [doi:10.1145/988952.989004](https://doi.org/10.1145/988952.989004)

### LSC.Polygon

- Gourley, Green (1983): A Polygon-to-Rectangle Conversion Algorithm [doi:10.1109/MCG.1983.262975](https://doi.org/10.1109/MCG.1983.262975)

### LSC.SegmentTree

- Bentley (1977): Solutions to Klee's rectangle problems. Unpublished manuscript

### LSC.SuffixTree

- Becher, Deymonnaz, Heiber (2013): Efficient repeat finding via suffix arrays [arXiv:1304.0528](https://arxiv.org/abs/1304.0528)

### LSC.UnionFind

- Tarjan (1975): Efficiency of a Good But Not Linear Set Union Algorithm [doi:10.1145/321879.321884](https://doi.org/10.1145/321879.321884)


## To do

- LSC
  - revisit stage logic

- Interval stabbing
  - [doi:10.1007/978-3-642-10631-6_18](https://doi.org/10.1007/978-3-642-10631-6_18)

- Pin permutations
  - [doi:10.1109/iccd.1992.276294](https://doi.org/10.1109/iccd.1992.276294)

