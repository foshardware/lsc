-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.UnionFind
    ( unionFind
    ) where

import Control.Monad.ST

import Test.Tasty
import Test.Tasty.HUnit

import LSC.UnionFind



unionFind :: TestTree
unionFind = testGroup "UnionFind"
  [ testCase "Simple union"
      $ do
        (p1, p2) <- stToIO
          $ do
            x1 <- disjointSet
            x2 <- disjointSet
            x3 <- disjointSet
            union x1 x2
            (,) <$> equivalent x1 x2 <*> equivalent x1 x3
        assertBool "" $ p1 && not p2
  , testCase "Complex union"
      $ do
        (p1, p2, p3, p4) <- stToIO
          $ do
            x1 <- disjointSet
            x2 <- disjointSet
            x3 <- disjointSet
            x4 <- disjointSet
            x5 <- disjointSet
            x6 <- disjointSet
            union x2 x6
            union x2 x5
            union x1 x3
            union x5 x4
            (,,,)
              <$> equivalent x2 x4
              <*> equivalent x1 x3
              <*> equivalent x1 x2
              <*> equivalent x1 x4
        assertBool "" $ p1 && p2 && not p3 && not p4
  ]

