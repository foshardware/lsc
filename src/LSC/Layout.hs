-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Custom data structure for fast region searches that is strictly coupled to
-- the netgraph model.
--
--
-- `Layout` supports the following operations:
--
-- - construction from a logic block vector
-- - inserting a logic block
-- - removing a logic block
-- - slicing horizontally and vertically
-- - querying points
-- - searching for logic blocks on a given row
-- - retrieve intervals of unused area
--
-- We are looking for a polymorphic data type that supports efficient implementations
-- of the above-mentioned.
--
module LSC.Layout where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Either
import Data.Function
import Data.IntMap
  ( IntMap
  , lookupLE
  , split, splitLookup
  , insert, alter
  , delete
  , singleton
  , fromAscList, toAscList
  , toDescList
  )
import qualified Data.IntMap as IntMap
import Prelude hiding (lookup)

import LSC.Cartesian
import LSC.Component
import LSC.HigherOrder
import LSC.Model
import LSC.NetGraph



type Layout = IntMap Segment

type Segment = IntMap (Either Area Gate)

type Slot = (Int, Either Area Gate)

type Area = Component' Layer Int


type SegmentIterator = Segment -> Int -> [Slot]


leftNext, rightNext :: SegmentIterator
leftNext  segment k = toDescList $ delete k $ fst $ split k segment
rightNext segment k =  toAscList $ delete k $ snd $ split k segment


spaces :: SegmentIterator -> Int -> Segment -> Area -> [Area]
spaces it n segment = lefts . map snd . take n . it segment . centerX



buildLayout :: Foldable f => f Gate -> Layout
buildLayout
  = fmap intersperseSpace
  . foldl' (\ a g -> alter (pure . insertGate g . fold) (ordinate g) a) mempty
{-# INLINABLE buildLayout #-}



ordinate :: Gate -> Int
ordinate = centerY . view geometry


insertGate :: Gate -> Segment -> Segment
insertGate = liftA2 insert (centerX . view geometry) Right


removeGate :: Gate -> Segment -> Segment
removeGate = delete . centerX . view geometry


intersperseSpace :: Segment -> Segment
intersperseSpace segment = gs <> fromAscList
  [ (centerX area, Left area)
  | (u, v) <- xs `zip` tail xs
  , let area = u ^. geometry & l .~ view (geometry . r) u & r .~ view (geometry . l) v
  ]
  where
    xs = rights $ snd <$> toAscList gs
    gs = IntMap.filter isRight segment



cutLayout :: (Int, Int) -> IntMap a -> IntMap a
cutLayout (lower, upper) zs
  = foldMap (singleton lower) y <> xs <> foldMap (singleton upper) x
  where
    (_, y, ys) = splitLookup lower zs
    (xs, x, _) = splitLookup upper ys



cutSegment :: (Int, Int) -> Segment -> Segment
cutSegment (lower, upper) zs
  | null $ cutLayout (lower, upper) zs -- TODO: not obvious what to return when the
  = fold                               --       optimal region's width has degraded
    [ singleton pos g                  --       or a vertical swap is performed
    | (pos, g) <- toList (lookupLE lower zs)
    , lower <= either (view r) (view (geometry . r)) g + either width gateWidth g `div` 2
    ]
cutSegment (lower, upper) zs
  = cutLayout (lower, upper) zs

