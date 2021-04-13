-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TypeApplications #-}

module Spec.LSC.BinarySearch where

import Control.Monad
import Data.Foldable
import Data.List (sort)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.BinarySearch
import LSC.Entropy hiding (Gen)



bsearch :: TestTree
bsearch = testGroup "Binary search"
  [ medians
  , uniques
  ]



medians :: TestTree
medians = testGroup "Median"
  [ testCase "Random input"
      $ do
        med <- generate $ choose (1, 100000)
        assertEqual "even median" (med - 1) . median . sort . toList
          <=< nonDeterministically @IO Nothing $ randomPermutation (2 * med)
        assertEqual "odd median" med . median . sort . toList
          <=< nonDeterministically @IO Nothing $ randomPermutation (2 * med + 1)
  , testCase "Large input" $ assertEqual "is median" 5000000 $ median [0..10000000 :: Int]
  ]



uniques :: TestTree
uniques = testGroup "Unstable unique"
  [ testCase "Random lists"
      $ sequence_
      $ replicate 100
      $ do
        n <- generate $ choose (1, 10000)
        v <- generate $ vector @Int n
        let xs = unstableUnique v
        assertBool "unique and ordered" $ and $ zipWith (<) xs (tail xs)
  ]

