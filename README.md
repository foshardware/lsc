# Libre Silicon Compiler

## Dependencies

### Build dependencies

- [stack](https://www.stackage.org/)


### Runtime dependencies

- [yices-smt2](http://yices.csl.sri.com/)
- [freeglut](http://freeglut.sourceforge.net/)


## Installation

`stack install`

## Usage

### Print usage

`lsc -h`  


### Place and route a small component

`lsc -b sample/fulladder.blif -l sample/osu035.lef -c -d > result.svg`  


### Exline a large component

`lsc -b sample/fulladder.blif -l sample/osu035.lef -x -d > result.svg`  


### Tests

`stack test`  

