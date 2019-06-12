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


### Place and route a small component

`lsc -l sample/osu035.lef -c -d --output=svg sample/fulladder.blif > result.svg`  


### Create graphics from def file

`lsc -l sample/osu035.lef --output=svg sample/map9v3.def > result.svg`  


## Tests

`stack test`  


## References

### LSC.FM

- Fiduccia, et. al. (1982): A Linear-Time Heuristic for Improving Network Partitions [10.1109/dac.1982.1585498](https://doi.org/10.1109/dac.1982.1585498)
- Alpert, Huang, Kahng (1998): Multilevel Circuit Partitioning, [10.1109/dac.1997.597204](https://doi.org/10.1109/dac.1997.597204)

### LSC.SuffixTree

- Becher, Deymonnaz, Heiber (2013): Efficient repeat finding via suffix arrays
