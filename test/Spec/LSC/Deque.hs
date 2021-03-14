-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TypeApplications #-}

module Spec.LSC.Deque
  ( deque 
  ) where

import Data.Maybe
import Data.Foldable

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Deque (enqueue, dequeue)



deque :: TestTree
deque = testGroup "Deque"
  [ testCase "Enqueue"
      $ sequence_
      $ replicate 100
      $ do
        n <- generate $ choose (1000, 10000)
        k <- generate $ choose (2, 20)
        v <- generate $ vector @Int n
        let q = foldl (flip enqueue) mempty v
        assertEqual "first element" (listToMaybe v) (fst $ dequeue q)
        assertEqual "sum" (sum v) (sum q)
        assertEqual "maximum" (maximum v) (maximum q)
        assertEqual "length" (length v) (length q)
        assertEqual "fmap" (fmap (* k) v) (toList $ fmap (* k) q)
  ]

