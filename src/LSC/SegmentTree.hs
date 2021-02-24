-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.SegmentTree
  ( SegmentTree
  , constructSegmentTree
  , density, densityOver
  , compact, pull
  ) where

import Data.List (group, sort)


data SegmentTree a
  = Interval (a, a) Tag (SegmentTree a) (SegmentTree a)
  | Leaf a Tag
  deriving Show


type Tag = (Int, Int)


lower, upper :: SegmentTree a -> a
lower (Leaf a _) = a
lower (Interval (a, _) _ _ _) = a
upper (Leaf a _) = a
upper (Interval (_, a) _ _ _) = a


density :: SegmentTree a -> Int
density (Leaf _ (x, _)) = x
density (Interval _ (x, _) _ _) = x



densityOver :: Ord a => (a, a) -> SegmentTree a -> Int
densityOver (x, y) node
  | x > y
  = densityOver (y, x) node
densityOver _ (Leaf _ (den, _))
  = den
densityOver (x, y) (Interval (a, b) (den, _) _ _)
  | x <= a
  , y >= b
  = den
densityOver (x, y) (Interval _ (_, del) left right)
  = max (l - del) (r - del)
    where
    l = if x <= upper left  then densityOver (x, y) left  else del
    r = if y >= lower right then densityOver (x, y) right else del



constructSegmentTree :: Ord a => [(a, a)] -> SegmentTree a
constructSegmentTree [] = error "constructSegmentTree: empty list" 
constructSegmentTree xs
  = flip (foldl (flip compact)) xs
  . head
  . until converged intervals
  . map (flip Leaf (0, 0))
  . map head . group . sort
  . foldMap (\ ~(x, y) -> [x, y])
  $ xs

    where

    converged [_] = True
    converged   _ = False

    intervals (x : y : ys) = Interval (lower x, upper y) (0, 0) x y : intervals ys
    intervals ys = ys



compact :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
compact (x, y) t
  | x > y
  = compact (y, x) t
compact _ (Leaf a (den, del))
  = Leaf a (succ den, del)
compact (x, y) (Interval (a, b) (_, del) left right)
  = Interval (a, b) (density l `max` density r - del, del) l r
    where
    l = if x <= upper left  then compact (x, y) left  else left
    r = if y >= lower right then compact (x, y) right else right



pull :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
pull (x, y) node
  | x > y
  = pull (y, x) node
pull _ (Leaf a (den, del))
  = Leaf a (pred den, succ del)
pull (x, y) (Interval (a, b) (den, del) left right)
  | x <= a
  , y >= b
  = Interval (a, b) (pred den, succ del) left right
pull (x, y) (Interval (a, b) (_, del) left right)
  = Interval (a, b) (density l `max` density r - del, del) l r
    where
    l = if x <= upper left  then pull (x, y) left  else left
    r = if y >= lower right then pull (x, y) right else right

