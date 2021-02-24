-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE FlexibleInstances #-}

module Spec.LSC.SegmentTree where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.SegmentTree




segmentTree :: TestTree
segmentTree = testGroup "SegmentTree"
  [ testCase "Construction"
      $ do

        n <- generate $ choose (8, 240)
        xs <- generate $ vector n

        let tree = constructSegmentTree xs :: SegmentTree Int
            cleared = foldr pull tree xs

        assertBool "construction"
          $ all ( > 0) $ flip densityOver tree <$> xs

        assertBool "pull"
          $ all (== 0) $ flip densityOver cleared <$> xs

        assertBool "compact"
          $  map (flip densityOver (foldr compact cleared xs)) xs
          == map (flip densityOver tree) xs
  ]
