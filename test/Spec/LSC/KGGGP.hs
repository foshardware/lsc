-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}

module Spec.LSC.KGGGP
    ( gggp
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.FileEmbed
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import Data.Text.Encoding
import Prelude hiding (replicate)

import Test.Tasty
import Test.Tasty.HUnit

import LSC.BLIF
import LSC.Types
import LSC.KGGGP




gggp :: TestTree
gggp = testGroup "KGGGP"
  [ testCase "Insert Gain" kgggpInsertGain
  , testCase "Remove Gain" kgggpRemoveGain
  , testCase "Modify Gain" kgggpModifyGain
  , testCase "Empty Gains" kgggpEmptyGains
  ]



kgggpInsertGain :: IO ()
kgggpInsertGain = do

    x <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 5 3 4 bs
      maximumGain bs
    assertEqual "maximum gain" x (5, 3)



kgggpRemoveGain :: IO ()
kgggpRemoveGain = do

    x <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 2 3 4 bs
      insertGain 5 3 4 bs
      _ <- removeGain 5 3 bs
      maximumGain bs
    assertEqual "maximum gain" x (2, 3)



kgggpModifyGain :: IO ()
kgggpModifyGain = do

    (x, g) <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 3 3 5 bs
      insertGain 2 3 4 bs
      insertGain 1 2 2 bs
      modifyGain 2 3 succ bs
      (,) <$> maximumGain bs <*> removeGain 2 3 bs
    assertEqual "gain" g (Just 5)
    assertEqual "maximum gain" x (2, 3)



kgggpEmptyGains :: IO ()
kgggpEmptyGains = do

    (x, y) <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 3 3 5 bs
      _ <- removeGain 3 3 bs
      x <- emptyGains bs
      insertGain 3 3 5 bs
      y <- emptyGains bs
      pure (x, y)

    assertBool "empty" x
    assertBool "not empty" $ not y



blifHypergraph :: BLIF -> ST s (V, E)
blifHypergraph netlist = inputRoutine
    (top ^. nets . to length)
    (top ^. gates . to length)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList $ top ^. nets
    , c <- w ^. contacts . to HashMap.keys
    ] where top = fromBLIF netlist


queue_1Hypergraph :: ST s (V, E)
queue_1Hypergraph = either (fail . show) blifHypergraph $ parseBLIF queue_1Blif


queue_1Blif :: Text
queue_1Blif = decodeUtf8 $(embedFile "sample/queue_1.blif")


