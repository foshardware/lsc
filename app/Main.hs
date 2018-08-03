{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module Main where

import Data.Text.IO as Text

import Language.SMTLib2
import Language.SMTLib2.Pipe

import BLIF.Parser

import LSC
import LSC.Types


main = do

    -- BLIF
    Prelude.putStrLn . show . parseBLIF =<< Text.readFile "test.blif"

    -- LSC
    let gate1 = Gate (4, 4) 1
        gate2 = Gate (4, 4) 2
        gate3 = Gate (4, 4) 3
        wire1 = Wire gate1 gate2 1
        wire2 = Wire gate1 gate3 2
    let netlist = Netlist [gate1, gate2, gate3] [wire1, wire2]
    let tech = Technology (200, 200) 1
    result <- withBackend pipeZ3 $ stage1 netlist `runLSC` tech
    Prelude.putStrLn result
