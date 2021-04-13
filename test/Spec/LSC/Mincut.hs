-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.Mincut where

import Control.Lens
import Data.Default

import Data.Matrix (getMatrixAsVector)
import Data.Vector (filter)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Prelude hiding (filter)

import LSC.Mincut
import LSC.Model
import LSC.Transformer

import Spec.LSC.Model



mincuts :: TestTree
mincuts = testGroup "Mincut"
  [ placeMatrices
  ]


placeMatrices :: TestTree
placeMatrices = testGroup "Place matrix"

  [ testCase "Soundness"
      $ sequence_
      $ replicate 20
      $ do

        c <- generate $ choose (500, 1500)
        Given top <- generate $ resize c arbitrary

        m <- pure $ initialMatrix top
        n <- evalLSC def def $ placeMatrix m

        let i = length $ top ^. gates
            j = length $ filter (>= 0) $ view number <$> getMatrixAsVector m
            k = length $ filter (>= 0) $ view number <$> getMatrixAsVector n

        assertEqual "netgraph generation" c i
        assertEqual "initial gate count"  c j
        assertEqual "placed gate count"   c k

  ]

