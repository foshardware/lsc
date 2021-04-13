-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}

module LSC.Quadtree where

import Data.Bifunctor.TH
import Data.Matrix (Matrix, nrows, ncols, getElem)
import Data.List (partition)

import LSC.Cartesian



threshold :: Word
threshold = 2


data Quadtree a f x y
  = Leaf [(f x y, a)]
  | Quad (Line x y) (Quadtree a f x y) (Quadtree a f x y) (Quadtree a f x y) (Quadtree a f x y)
  deriving (Functor, Foldable, Traversable, Show)

$(deriveBifunctor     ''Quadtree)
$(deriveBifoldable    ''Quadtree)
$(deriveBitraversable ''Quadtree)

type Quadtree' a f x = Quadtree a f x x

instance Cartesian f x y => Cartesian (Quadtree a f) x y


constructQuadtree :: Cartesian f x y => Line x y -> [(f x y, a)] -> Quadtree a f x y
constructQuadtree = go
  where

    go _ [ ] = Leaf [ ]
    go _ [x] = Leaf [x]

    go f xs -- degraded to a point
      |  width f <= fromIntegral threshold
      , height f <= fromIntegral threshold
      = Leaf xs

    go f xs
      = Quad f (go f1 q1) (go f2 q2) (go f3 q3) (go f4 q4)
      where
        (ls, rs) = partition ((<= centerX f) . centerX . fst) xs
        (q3, q2) = partition ((<= centerY f) . centerY . fst) ls
        (q4, q1) = partition ((<= centerY f) . centerY . fst) rs
        f1 = Line (centerX f, centerY f) (maxX f, maxY f)
        f2 = Line (minX f, centerY f) (centerX f, maxY f)
        f3 = Line (minX f, minY f) (centerX f, centerY f)
        f4 = Line (centerX f, minY f) (maxX f, centerY f)
{-# INLINABLE constructQuadtree #-}


datapoints :: Monoid m => (a -> m) -> Quadtree a f x y -> [m]
datapoints f = go
  where
    go (Leaf xs)
      = [foldMap (f . snd) xs]
    go (Quad _ q1 q2 q3 q4)
      = (m1 <> m2 <> m3 <> m4)
      : (p1 ++ p2 ++ p3 ++ p4)
      where
        p1@(m1 : _) = go q1
        p2@(m2 : _) = go q2
        p3@(m3 : _) = go q3
        p4@(m4 : _) = go q4
{-# INLINABLE datapoints #-}


fromMatrix :: Matrix a -> Quadtree a (,) Int Int
fromMatrix m = go (Line (1, 1) (ncols m, nrows m))
  where
    go (Line (x1, y1) (x2, y2))
      | x2 - x1 <= 0 || y2 - y1 <= 0
      = Leaf []
    go (Line (x1, y1) (x2, y2))
      | 1 <- x2 - x1
      , 1 <- y2 - y1
      = Quad (Line (x1, y1) (x2, y2))
        (Leaf [((x2, y2), getElem y2 x2 m)])
        (Leaf [((x1, y2), getElem y2 x1 m)])
        (Leaf [((x1, y1), getElem y1 x1 m)])
        (Leaf [((x2, y1), getElem y1 x2 m)])
    go (Line (x1, y1) (x2, y2))
      | 1 <- x2 - x1
      = Quad (Line (x1, y1) (x2, y2))
        (Leaf [])
        (Leaf [((x1, y2), getElem y2 x1 m)])
        (Leaf [((x1, y1), getElem y1 x1 m)])
        (Leaf [])
    go (Line (x1, y1) (x2, y2))
      | 1 <- y2 - y1
      = Quad (Line (x1, y1) (x2, y2))
        (Leaf [])
        (Leaf [])
        (Leaf [((x1, y1), getElem y1 x1 m)])
        (Leaf [((x2, y1), getElem y1 x2 m)])
    go f
      = Quad f (go f1) (go f2) (go f3) (go f4)
      where
        f1 = Line (centerX f, centerY f) (maxX f, maxY f)
        f2 = Line (minX f, centerY f) (centerX f, maxY f)
        f3 = Line (minX f, minY f) (centerX f, centerY f)
        f4 = Line (centerX f, minY f) (maxX f, centerY f)

