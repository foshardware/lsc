# Libre Silicon Compiler

## Installation

`git submodule update --init --recursive`  
`stack install`

## Usage

`lsc -h`  
`lsc -b tests/fulladder.blif -l tests/osu035.lef -c > result.svg`  

### Tests

`lsc -t   # requires lsc-test in $PATH`  
`lsc-test`  

