{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module Main where

import Data.Text.IO as Text

import Language.SMTLib2
import Language.SMTLib2.Pipe

import BLIF.Parser
import LEF.Parser

import LSC
import LSC.BLIF
import LSC.LEF
import LSC.Types


main = do

    -- Prelude.putStrLn . either show (show . fromBLIF) . parseBLIF =<< Text.readFile "test.blif"
    -- Prelude.putStrLn . either show (show . fromLEF) . parseLEF =<< Text.readFile "test.lef"
    Prelude.putStrLn . show . parseLEF =<< Text.readFile "test.lef"


    -- result <- withBackend pipeZ3 $ stage1 netlist `runLSC` tech

