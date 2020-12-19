-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.Entropy
    ( entropy
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Entropy



entropy :: TestTree
entropy = testGroup "Entropy"
  [ testCase "Random permutation" testRandomPermutation
  ]



testRandomPermutation :: IO ()
testRandomPermutation = do
  n <- generate $ choose (1000, 10000)
  v <- pure $ V.generate n id
  u <- randomPermutation n =<< create
  a <- V.thaw u
  V.sort a
  w <- V.freeze a

  assertBool "permutation" $ u /= v
  assertBool "sorted"      $ w == v


