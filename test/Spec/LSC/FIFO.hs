-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TypeApplications #-}

module Spec.LSC.FIFO
  ( fifo 
  ) where

import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.FIFO (enqueue, dequeue)



fifo :: TestTree
fifo = testGroup "FIFO"
  [ testCase "Enqueue"
      $ sequence_
      $ replicate 100
      $ do
        n <- generate $ choose (1000, 10000)
        v <- generate $ vector @Int n
        let q = foldl (flip enqueue) mempty v
        assertEqual "first element" (listToMaybe v) (fst $ dequeue q)
        assertEqual "sum" (sum v) (sum q)
        assertEqual "maximum" (maximum v) (maximum q)
        assertEqual "length" (length v) (length q)
  ]

