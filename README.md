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


### Create graphics from def file

`lsc -l sample/osu035.lef --output=svg sample/map9v3.def > result.svg`  


## Tests

`stack test`  


## References

### LSC.FM

- Fiduccia, et. al. (1982): A Linear-Time Heuristic for Improving Network Partitions [10.1109/dac.1982.1585498](https://doi.org/10.1109/dac.1982.1585498)
- Alpert, Huang, Kahng (1998): Multilevel Circuit Partitioning, [10.1109/dac.1997.597204](https://doi.org/10.1109/dac.1997.597204)

### LSC.Legalize

- Kahng, Markov, Reda (2004): On Legalization of Row-Based Placements [10.1145/988952.989004](https://doi.org/10.1145/988952.989004)

### LSC.SuffixTree

- Becher, Deymonnaz, Heiber (2013): Efficient repeat finding via suffix arrays
