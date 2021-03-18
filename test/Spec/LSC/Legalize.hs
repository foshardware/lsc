-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.Legalize
    ( legalize
    , referenceRowLegalization
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.ST
import Data.Function (on)
import Data.Graph (buildG)
import Data.List (groupBy)
import Data.Vector (Vector, (!), unsafeUpd)
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Cartesian
import LSC.Component
import LSC.Legalize
import LSC.Model
import LSC.NetGraph

import Spec.LSC.Model



legalize :: TestTree
legalize = testGroup "Legalize"

  [ testCase "Row juggling"
      $ sequence_
      $ replicate 20
      $ do

        Given top <- generate arbitrary

        jug <- stToIO $ views gates getRows <$> rowJuggling 1 top

        assertEqual "gate count"
           (views gates length top)
           (sum $ length <$> jug)

        assertEqual "gate width"
           (sum $ gateWidth <$> view gates top)
           (sum $ sum . fmap gateWidth <$> jug)

        assertBool "exceeds capacity"
          $ all (<= maxRowWidth)
          $ sum . fmap gateWidth <$> jug

  , testCase "Row legalization"
      $ sequence_
      $ replicate 10
      $ do

        Given top <- generate arbitrary

        jug <- stToIO $ rowJuggling 1 top

        grp <- stToIO $ rowLegalization jug `mapM` views gates getRows jug

        assertEqual "gate count"
           (views gates length top)
           (sum $ length <$> grp)

        assertEqual "gate width"
           (sum $ gateWidth <$> view gates top)
           (sum $ sum . fmap gateWidth <$> grp)

        assertBool "exceeds capacity"
          $ all (<= maxRowWidth)
          $ sum . fmap gateWidth <$> grp

        assertBool "unsorted"
          $ all (all (\ (g, h) -> g ^. space . l <= h ^. space . l) . liftA2 V.zip id V.tail)
          $ grp

        assertBool "overlaps"
          $ all (not . or . liftA2 (V.zipWith gateOverlap) id V.tail)
          $ grp

  , testCase "Reference implementation"
      $ do

        Given top <- generate arbitrary

        jug <- stToIO $ rowJuggling 1 top

        grp <- stToIO $ rowLegalization jug `mapM` views gates getRows jug

        ref <- stToIO $ referenceRowLegalization jug `mapM` views gates getRows jug

        assertBool "equivalent"
          $ fmap (view space <$>) ref == fmap (view space <$>) grp
  ]



referenceRowLegalization :: NetGraph -> Vector Gate -> ST s (Vector Gate)
referenceRowLegalization   _ gs | null gs = pure gs
referenceRowLegalization top gs = do

    let row = top ^. supercell . rows ^? ix (V.head gs ^. space . b)

    let res = maybe 1 (view granularity) row
        off = maybe 0 (subtract res . view l) row `div` res

    let w = (`div` res) . width . view space <$> gs
        x = subtract off . (`div` res) . view (space . l) <$> gs

    let n = maybe 1 (succ . view cardinality) row
        m = length gs

    let count = succ n * succ m

    let target = pred count

    let vertex j k = k + j * succ n
        site i = divMod i $ succ n

    let graph = buildG (0, target) $
          [ (vertex j (pred k), vertex j k)
          | j <- [0 .. m]
          , k <- [1 .. n]
          ] ++
          [ (vertex (pred j) k, vertex j (k + w ! pred j))
          | j <- [1 .. m]
          , k <- [1 .. n - w ! pred j]
          ]
    
    let minPerb (u, v) | fst (site u) == fst (site v) = 0
        minPerb (u, _) | (j, k) <- site u, gs ! j ^. fixed, x ! j /= k = count
        minPerb (u, _) | (j, k) <- site u = abs $ x ! j - k

    shortestPath <- topologicalShortestPath minPerb graph target

    pure $ unsafeUpd gs
      [ (i, gs ! i & space %~ relocateL ((pos + off) * res))
      | (i, pos) <- tail . map head . groupBy (on (==) fst) . map site $ shortestPath
      ]

