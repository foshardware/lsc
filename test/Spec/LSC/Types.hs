-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Spec.LSC.Types
    ( types
    , maxRowWidth
    , Given(..)
    ) where

import Control.Lens hiding (indexed)
import Data.Char
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (fromDistinctAscList)
import Data.List (sort)
import qualified Data.Text as T
import Data.Vector (indexed, replicateM)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Component
import LSC.Entropy hiding (Gen)
import LSC.NetGraph
import LSC.Polygon
import LSC.Types



sizeOfNetGraph :: Int
sizeOfNetGraph = 10000

maxCellHeight :: Int
maxCellHeight = 10000

maxCellWidth :: Int
maxCellWidth = 20000

maxResolution :: Int
maxResolution = 1000


maxRowWidth :: Int
maxRowWidth = maxCellsPerRow * maxCellWidth

maxSiteCount :: Int
maxSiteCount = maxRowWidth `div` maxResolution

maxCellsPerRow :: Int
maxCellsPerRow = ceiling @Double $ sqrt $ fromIntegral sizeOfNetGraph

maxRowCount :: Int
maxRowCount = ceiling @Double $ sqrt $ fromIntegral sizeOfNetGraph


layoutArea :: Component' l Int
layoutArea = rect 0 0 (maxCellsPerRow * maxCellWidth) (maxRowCount * maxCellHeight)



newtype Given a = Given { say :: a }


instance Arbitrary (Given NetGraph) where
    arbitrary = do
        gs <- replicateM sizeOfNetGraph $ resize sizeOfNetGraph arbitrary 
        rs <- replicateM maxRowCount $ resize maxRowWidth arbitrary
        pure
          $ Given
          $ rebuildEdges
          $ assignCellsToRows
          $ def &~ do
            gates .= set number `imap` fmap say gs
            supercell %= set rows (fromDistinctAscList
                [ (i * maxCellHeight, row & number .~ i & b .~ i * maxCellHeight)
                | (i, row) <- toList . indexed $ fmap say rs
                ])



instance Arbitrary (Given Row) where
    arbitrary = do
        n <- getSize
        pure
          $ Given
          $ def &~ do
            cardinality .= n `div` maxResolution
            granularity .= n `div` maxSiteCount



instance Arbitrary (Given Gate) where
    arbitrary = do

        n <- getSize

        x <- choose (0,  width layoutArea - maxCellWidth)
        y <- choose (0, height layoutArea - maxCellHeight)
        w <- (* maxResolution) . succ . (`div` maxResolution) <$> choose (0, pred maxCellWidth)

        k <- choose (2, 6)
        v <- sequence $ replicate k $ do
            p <- T.map toUpper . base16Identifier <$> choose (0, 0xFF)
            e <- base16Identifier <$> choose (0x100, 0x100 + n)
            pure (p, e)

        pure
          $ Given
          $ def &~ do
            space .= rect x y (x + w) (y + maxCellHeight)
            wires .= HashMap.fromList v



instance Arbitrary (Given Pin) where
    arbitrary = do
        x <- choose (0, maxCellWidth)
        w <- (* maxResolution) . succ . (`div` maxResolution) <$> choose (0, pred maxCellWidth)
        v <- T.map toUpper . base16Identifier <$> choose (0, 0xFF)
        pure
          $ Given
          $ def &~ do
            identifier .= v
            geometry .= [simplePolygon $ rect x 0 (x + w) maxCellHeight]




types :: TestTree
types = testGroup "Types"
  [ medians
  , uniques
  ]



medians :: TestTree
medians = testGroup "Median"
  [ testCase "Random input"
      $ do
        rng <- create
        med <- generate $ choose (1, 100000)
        assertEqual "even median" (med - 1) . median . sort . toList =<< randomPermutation (2 * med) rng
        assertEqual "odd median" med . median . sort . toList =<< randomPermutation (2 * med + 1) rng
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

