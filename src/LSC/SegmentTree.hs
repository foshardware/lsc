-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.SegmentTree
  ( SegmentTree, constructSegmentTree
  , maxDensity, densityRatio, densityOver
  , unsafeDensityRatio, unsafeDensityOver
  , compact, pull
  , unsafeCompact, unsafePull
  , emptySegment
  , showTree
  ) where

import Data.List (group, sort)
import Data.Ratio


-- | Undefined behaviour for:
--   - Inserting an interval `[x1,x2]` where `x1` or `x2` are unknown to the segment tree
--   - Removing an interval `[x1,x2]` more often than inserting `[x1,x2]`
--
data SegmentTree a
  = Interval (a, a) Tag (SegmentTree a) (SegmentTree a)
  | Leaf a Tag
  deriving Show


type Tag = (Int, Int)


constructSegmentTree :: Ord a => [(a, a)] -> SegmentTree a
constructSegmentTree []
  = error "constructSegmentTree: empty list"
constructSegmentTree xs
  = flip (foldl (flip compact)) xs
  . head
  . until (null . tail) intervals
  . map (flip Leaf (0, 0))
  . map head . group . sort
  . foldMap (\ ~(x, y) -> [x, y])
  $ xs
    where
    intervals (x : y : ys) = Interval (lower x, upper y) (0, 0) x y : intervals ys
    intervals ys = ys


lower, upper :: SegmentTree a -> a
lower     (Leaf x _)     = x
lower (Interval x _ _ _) = fst x
upper     (Leaf x _)     = x
upper (Interval x _ _ _) = snd x


emptySegment :: SegmentTree a -> Bool
emptySegment     (Leaf _ (0, _))     = True
emptySegment (Interval _ (0, _) _ _) = True
emptySegment _ = False


maxDensity :: SegmentTree a -> Int
maxDensity     (Leaf _ (x, _))     = x
maxDensity (Interval _ (x, _) _ _) = x



densityRatio, unsafeDensityRatio :: Ord a => (a, a) -> SegmentTree a -> Rational
densityRatio _ node
  | emptySegment node
  = error "densityRatio: empty segment"
densityRatio (x, y) node
  | x > y
  = unsafeDensityRatio (y, x) node
densityRatio (x, y) node
  = unsafeDensityRatio (x, y) node

unsafeDensityRatio (x, y) node
  = fromIntegral (unsafeDensityOver (x, y) node)
  % fromIntegral (maxDensity node)
{-# INLINABLE densityRatio #-}
{-# INLINABLE unsafeDensityRatio #-}


densityOver, unsafeDensityOver :: Ord a => (a, a) -> SegmentTree a -> Int
densityOver (x, y) node
  | x > y
  = densityOver (y, x) node
densityOver (x, y) node
  = unsafeDensityOver (x, y) node

unsafeDensityOver (x, y) (Leaf a (density, delete))
  | x <= a
  , y >= a
  = density
unsafeDensityOver (x, y) (Interval (a, b) (density, delete) _ _)
  | x <= a
  , y >= b
  = density
unsafeDensityOver (x, y) (Interval _ (_, delete) left right)
  = subtract delete
  $ max l r
    where
    l = if x <= upper left  then unsafeDensityOver (x, y) left  else delete
    r = if y >= lower right then unsafeDensityOver (x, y) right else delete
unsafeDensityOver _ _ = 0
{-# INLINABLE densityOver #-}


compact, unsafeCompact :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
compact (x, y) node
  | x > y
  = unsafeCompact (y, x) node
compact (x, y) node
  = unsafeCompact (x, y) node

unsafeCompact (x, y) (Leaf a (density, delete))
  | x <= a
  , y >= a
  = Leaf a (succ density, delete)
unsafeCompact (x, y) (Interval (a, b) (_, delete) left right)
  = Interval (a, b) (subtract delete $ maxDensity l `max` maxDensity r, delete) l r
    where
    l = if x <= upper left  then unsafeCompact (x, y) left  else left
    r = if y >= lower right then unsafeCompact (x, y) right else right
unsafeCompact _ node = node
{-# INLINABLE compact #-}


pull, unsafePull :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
pull (x, y) node
  | x > y
  = unsafePull (y, x) node
pull (x, y) node
  = unsafePull (x, y) node

unsafePull (x, y) (Leaf a (density, delete))
  | x <= a
  , y >= a
  = Leaf a (pred density, succ delete)
unsafePull (x, y) (Interval (a, b) (density, delete) left right)
  | x <= a
  , y >= b
  = Interval (a, b) (pred density, succ delete) left right
unsafePull (x, y) (Interval (a, b) (_, delete) left right)
  = Interval (a, b) (subtract delete $ maxDensity l `max` maxDensity r, delete) l r
    where
    l = if x <= upper left  then unsafePull (x, y) left  else left
    r = if y >= lower right then unsafePull (x, y) right else right
unsafePull _ node = node
{-# INLINABLE pull #-}


showTree :: Show a => SegmentTree a -> String
showTree node = unlines [""] ++ go "    " node
  where

  go _ (Leaf a (density, delete)) = unlines
      [ show a ++ " " ++ show density ++ "/" ++ show delete
      ]

  go s (Interval (a, b) (density, delete) left right) = unlines
      [ "[" ++ show a ++ "," ++ show b ++ "] " ++ show density ++ "/" ++ show delete
      ] ++ concat 
      [ s ++ go (s ++ "|   ") left
      , s ++ go (s ++ "    ") right
      ]

