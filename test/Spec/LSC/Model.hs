-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Spec.LSC.Model
  ( maxRowWidth, sizeOfNetGraph
  , Given(..)
  ) where

import Control.Lens hiding (indexed)
import Data.Char
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (fromDistinctAscList)
import qualified Data.Text as T
import Data.Vector (indexed, replicateM)

import Test.Tasty.QuickCheck

import LSC.Cartesian
import LSC.Component
import LSC.Model
import LSC.NetGraph
import LSC.Polygon



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
        n <- getSize
        gs <- replicateM n $ resize n arbitrary 
        rs <- replicateM maxRowCount $ resize maxRowWidth arbitrary
        pure
          $ Given
          $ rebuildHyperedges
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
            geometry .= rect x y (x + w) (y + maxCellHeight)
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


