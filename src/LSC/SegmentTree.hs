-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.SegmentTree
  ( SegmentTree, constructSegmentTree
  , maxDensity, densityRatio, densityOver
  , unsafeDensityRatio, unsafeDensityOver
  , compact, pull
  , unsafeCompact, unsafePull
  , showTree
  ) where

import Data.List (group, sort)
import Data.Ratio


-- | Undefined behaviour for:
--   - Inserting an interval `[x1,x2]` where `x1` or `x2` are unknown to the segment tree
--   - Removing an interval `[x1,x2]` more often than inserting `[x1,x2]`
--
data SegmentTree a
  = Nil
  | Leaf Int a
  | Interval Int Int (a, a) (SegmentTree a) (SegmentTree a)
  deriving Show


-- | Fold over non-empty endpoints
--
instance Foldable SegmentTree where
    foldMap = foldMapStabs
    null = nullSegment



constructSegmentTree :: Ord a => [(a, a)] -> SegmentTree a
constructSegmentTree []
  = Nil
constructSegmentTree xs
  = flip (foldl (flip compact)) xs
  . head
  . until (null . tail) intervals
  . map (Leaf 0)
  . map head . group . sort
  . foldMap (\ (x, y) -> [x, y])
  $ xs
  where
    intervals (x : y : ys) = Interval 0 0 (lower x, upper y) x y : intervals ys
    intervals ys = ys


lower, upper :: SegmentTree a -> a
lower       Nil            = undefined
lower     (Leaf _   x)     = x
lower (Interval _ _ x _ _) = fst x
upper       Nil            = undefined
upper     (Leaf _   x)     = x
upper (Interval _ _ x _ _) = snd x


nullSegment :: SegmentTree a -> Bool
nullSegment       Nil            = True
nullSegment     (Leaf 0 _)       = True
nullSegment (Interval 0 _ _ _ _) = True
nullSegment _ = False


maxDensity :: SegmentTree a -> Int
maxDensity       Nil            = 0
maxDensity     (Leaf x _)       = x
maxDensity (Interval x _ _ _ _) = x


asc :: Ord a => (a, a) -> (a, a)
asc (x, y) = (min x y, max x y)
{-# INLINE asc #-}


densityRatio, unsafeDensityRatio :: Ord a => (a, a) -> SegmentTree a -> Rational
densityRatio _ node
  | nullSegment node
  = error "densityRatio: empty segment"
densityRatio int node
  = unsafeDensityRatio (asc int) node

unsafeDensityRatio (x, y) node
  = fromIntegral (unsafeDensityOver (x, y) node)
  % fromIntegral (maxDensity node)
{-# INLINABLE densityRatio #-}
{-# INLINABLE unsafeDensityRatio #-}


densityOver, unsafeDensityOver :: Ord a => (a, a) -> SegmentTree a -> Int
densityOver int node
  = unsafeDensityOver (asc int) node

unsafeDensityOver (x, y) (Leaf density a)
  | x <= a
  , y >= a
  = density
unsafeDensityOver (x, y) (Interval density _ (a, b) _ _)
  | x <= a
  , y >= b
  = density
unsafeDensityOver (x, y) (Interval _ delete _ left right)
  = subtract delete
  $ max l r
  where
    l = if x <= upper left  then unsafeDensityOver (x, y) left  else delete
    r = if y >= lower right then unsafeDensityOver (x, y) right else delete
unsafeDensityOver _ _ = 0
{-# INLINABLE densityOver #-}


compact, unsafeCompact :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
compact int node
  = unsafeCompact (asc int) node

unsafeCompact (x, y) (Leaf density a)
  | x <= a
  , y >= a
  = Leaf (succ density) a
unsafeCompact (x, y) (Interval _ delete (a, b) left right)
  = Interval (subtract delete $ maxDensity l `max` maxDensity r) delete (a, b) l r
  where
    l = if x <= upper left  then unsafeCompact (x, y) left  else left
    r = if y >= lower right then unsafeCompact (x, y) right else right
unsafeCompact _ node = node
{-# INLINABLE compact #-}


pull, unsafePull :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
pull int node
  = unsafePull (asc int) node

unsafePull (x, y) (Leaf density a)
  | x <= a
  , y >= a
  = Leaf (pred density) a
unsafePull (x, y) (Interval density delete (a, b) left right)
  | x <= a
  , y >= b
  = Interval (pred density) (succ delete) (a, b) left right
unsafePull (x, y) (Interval _ delete (a, b) left right)
  = Interval (subtract delete $ maxDensity l `max` maxDensity r) delete (a, b) l r
  where
    l = if x <= upper left  then unsafePull (x, y) left  else left
    r = if y >= lower right then unsafePull (x, y) right else right
unsafePull _ node = node
{-# INLINABLE pull #-}



foldMapStabs :: Monoid m => (a -> m) -> SegmentTree a -> m
foldMapStabs f = go 0

  where

    go k node | maxDensity node <= k = mempty

    go k (Interval _ d _ l r) = go (k + d) l <> go (k + d) r

    go _ (Leaf _ a) = f a

    go _ Nil = mempty



showTree :: Show a => SegmentTree a -> String
showTree node = unlines [""] ++ go "    " node

  where

    go _ Nil = "[empty segment tree]"

    go _ (Leaf density a) = unlines
      [ show a ++ ": " ++ show density
      ]

    go s (Interval density delete (a, b) left right) = unlines
      [ "[" ++ show a ++ "," ++ show b ++ "] " ++ show density ++ "/" ++ show delete
      ] ++ concat 
      [ s ++ go (s ++ "|   ") left
      , s ++ go (s ++ "    ") right
      ]

