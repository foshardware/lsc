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

        assertEqual "compact"
          (flip densityOver (foldr compact cleared xs) <$> xs)
          (flip densityOver tree <$> xs)

  , testCase "Example 1a"
      $ do

        let xs = [(1, 2), (4, 6), (7, 8), (2, 4), (5, 7), (1, 3), (5, 6), (3, 8)]

        let tree = constructSegmentTree xs :: SegmentTree Int

        assertEqual "x1" 2 $ densityOver (1, 1) tree
        assertEqual "x2" 3 $ densityOver (2, 2) tree
        assertEqual "x3" 3 $ densityOver (3, 3) tree
        assertEqual "x4" 3 $ densityOver (4, 4) tree
        assertEqual "x5" 4 $ densityOver (5, 5) tree
        assertEqual "x6" 4 $ densityOver (6, 6) tree
        assertEqual "x7" 3 $ densityOver (7, 7) tree
        assertEqual "x8" 2 $ densityOver (8, 8) tree

        assertEqual "x12" 3 $ densityOver (1, 2) tree
        assertEqual "x34" 3 $ densityOver (3, 4) tree
        assertEqual "x56" 4 $ densityOver (5, 6) tree
        assertEqual "x78" 3 $ densityOver (7, 8) tree

        assertEqual "x14" 3 $ densityOver (1, 4) tree
        assertEqual "x58" 4 $ densityOver (5, 8) tree

        assertEqual "x18" 4 $ densityOver (1, 8) tree

        assertEqual "j1" 3 $ densityOver (1, 2) tree
        assertEqual "j2" 4 $ densityOver (4, 6) tree
        assertEqual "j3" 3 $ densityOver (7, 8) tree
        assertEqual "j4" 3 $ densityOver (2, 4) tree
        assertEqual "j5" 4 $ densityOver (5, 7) tree
        assertEqual "j6" 3 $ densityOver (1, 3) tree
        assertEqual "j7" 4 $ densityOver (5, 6) tree
        assertEqual "j8" 4 $ densityOver (3, 8) tree

  , testCase "Example 1b"
      $ do

        let xs = [(1, 2), (4, 6), (7, 8), (2, 4), (5, 7), (1, 3), (5, 6), (3, 8)]

        let tree = pull (3, 8) $ pull (5, 7) $ constructSegmentTree xs :: SegmentTree Int

        assertEqual "x1" 2 $ densityOver (1, 1) tree
        assertEqual "x2" 3 $ densityOver (2, 2) tree
        assertEqual "x3" 2 $ densityOver (3, 3) tree
        assertEqual "x4" 2 $ densityOver (4, 4) tree
        assertEqual "x5" 2 $ densityOver (5, 5) tree
        assertEqual "x6" 2 $ densityOver (6, 6) tree
        assertEqual "x7" 1 $ densityOver (7, 7) tree
        assertEqual "x8" 1 $ densityOver (8, 8) tree

        assertEqual "x12" 3 $ densityOver (1, 2) tree
        assertEqual "x34" 2 $ densityOver (3, 4) tree
        assertEqual "x56" 2 $ densityOver (5, 6) tree
        assertEqual "x78" 1 $ densityOver (7, 8) tree

        assertEqual "x14" 3 $ densityOver (1, 4) tree
        assertEqual "x58" 2 $ densityOver (5, 8) tree

        assertEqual "x18" 3 $ densityOver (1, 8) tree

        assertEqual "j1" 3 $ densityOver (1, 2) tree
        assertEqual "j2" 2 $ densityOver (4, 6) tree
        assertEqual "j3" 1 $ densityOver (7, 8) tree
        assertEqual "j4" 3 $ densityOver (2, 4) tree
        assertEqual "j5" 2 $ densityOver (5, 7) tree
        assertEqual "j6" 3 $ densityOver (1, 3) tree
        assertEqual "j7" 2 $ densityOver (5, 6) tree
        assertEqual "j8" 2 $ densityOver (3, 8) tree

  ]
