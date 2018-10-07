# Libre Silicon Compiler

## Installation

### Windows

`cabal install`

### Linux

`stack install`

## Usage

`lsc -h`  
`lsc -b tests/fulladder.blif -l tests/osu035.lef -c > result.svg`  

### Tests

`lsc -t   # requires lsc-test in $PATH`  
`lsc-test`  

