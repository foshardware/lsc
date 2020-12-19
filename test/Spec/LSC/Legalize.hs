-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.Legalize
    ( legalize
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.ST
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Legalize
import LSC.NetGraph
import LSC.Types

import Spec.LSC.Types



legalize :: TestTree
legalize = testGroup "Legalize"

  [ testCase "Row juggling" $ do

        top <- say <$> generate arbitrary

        grouped <- stToIO $ views gates getRows <$> rowJuggling 1 top

        assertBool "exceeds capacity"
          $ all (<= maxRowWidth)
          $ sum . fmap gateWidth <$> grouped

  , testCase "Row legalization" $ do

        top <- say <$> generate arbitrary

        juggled <- stToIO $ rowJuggling 1 top

        grouped <- stToIO $ rowLegalization juggled `mapM` views gates getRows juggled

        assertBool "exceeds capacity"
          $ all (< maxRowWidth)
          $ sum . fmap gateWidth <$> grouped
        assertBool "is unsorted"
          $ all (all (\ (g, h) -> g ^. space . l <= h ^. space . l) . liftA2 V.zip id V.tail)
          $ grouped
        assertBool "has overlaps"
          $ all (not . or . liftA2 (V.zipWith gateOverlap) id V.tail)
          $ grouped
  ]

