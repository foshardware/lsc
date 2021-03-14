-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE BangPatterns #-}

module LSC.UnionFind
    ( DisjointSet
    , disjointSet, pair
    , union, representation, equivalent
    ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef



newtype DisjointSet s = DisjointSet (STRef s (Either Rank (DisjointSet s)))
  deriving Eq

type Rank = Word


disjointSet :: ST s (DisjointSet s)
disjointSet = DisjointSet <$> newSTRef (Left 0)


pair :: ST s (DisjointSet s, DisjointSet s)
pair = do
    x <- DisjointSet <$> newSTRef (Left 1)
    y <- DisjointSet <$> newSTRef (Right x)
    pure (x, y)



findRoot :: DisjointSet s -> ST s (Rank, DisjointSet s)
findRoot = go
  where
    go i@(DisjointSet s)
      = do
        v <- readSTRef s
        case v of
          Left w -> pure (w, i)
          -- ^ arrived at root
          Right j -> do
              (w, k) <- go j
              when (j /= k) $ writeSTRef s (Right k)
              -- ^ path compression
              pure (w, k)



union :: DisjointSet s -> DisjointSet s -> ST s ()
union x y
  = do
    (w1, i1@(DisjointSet s1)) <- findRoot x
    (w2, i2@(DisjointSet s2)) <- findRoot y
    let !w = succ w1
    unless (i1 == i2)
      $ case w1 `compare` w2 of -- merge the smaller tree, left-biased
          EQ -> writeSTRef s2 (Right i1) >> writeSTRef s1 (Left w)
          GT -> writeSTRef s2 (Right i1) -- ^ union by rank
          LT -> writeSTRef s1 (Right i2)



representation :: DisjointSet s -> ST s (DisjointSet s)
representation x = snd <$> findRoot x


equivalent :: DisjointSet s -> DisjointSet s -> ST s Bool
equivalent x y = (==) <$> representation x <*> representation y

