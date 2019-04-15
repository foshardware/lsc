# Libre Silicon Compiler

## Dependencies

### Build dependencies

- [stack](https://www.stackage.org/)


### Runtime dependencies

- [yices-smt2](http://yices.csl.sri.com/)


## Installation

`stack install`

## Usage

`lsc -h`  
`lsc -b sample/fulladder.blif -l sample/osu035.lef -c > result.svg`  

### Tests

`stack test`  

