{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module Main where

import Language.SMTLib2
import Language.SMTLib2.Pipe

import LSC
import LSC.Types


main = do
    let netlist = Netlist [Gate 4 1, Gate 4 2, Gate 4 3] [Wire 1 2 1, Wire 1 3 2]
    let tech = Technology (5, 5)
    result <- withBackend pipeZ3 $ stage1 netlist `runLSC` tech
    print result

