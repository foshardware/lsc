{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module Main where

import Data.Text.IO as Text

import BLIF.Parser
import LEF.Parser

import Data.SBV

import LSC
import LSC.BLIF
import LSC.LEF
import LSC.SVG
import LSC.Types


main = do

    -- Prelude.putStrLn . either show (show . fromBLIF) . parseBLIF =<< Text.readFile "test.blif"
    -- Prelude.putStrLn . either show (show . freeze . fromLEF) . parseLEF =<< Text.readFile "test.lef"
    bootstrap <- either (error . show) fromLEF  . parseLEF  <$> Text.readFile "test.lef"
    netlist   <- either (error . show) (gnostic bootstrap . fromBLIF) . parseBLIF <$> Text.readFile "test.blif"

    result <- runSMT $ bootstrap `runLSC` stage1 netlist
    plotStdout result

