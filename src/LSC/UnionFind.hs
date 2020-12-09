{-# LANGUAGE BangPatterns #-}

module LSC.UnionFind
    ( DisjointSet(..)
    , newDisjointSet, newDisjointSetSized
    , union, representation, equivalent
    , singleton
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Hashable
import Data.HashTable.ST.Basic
import Data.Maybe
import Data.Word
import Prelude hiding (lookup)



newtype DisjointSet s a = DisjointSet (HashTable s a (Rank, a))

type Rank = Word


newDisjointSet :: ST s (DisjointSet s a)
newDisjointSet = DisjointSet <$> new


newDisjointSetSized :: Int -> ST s (DisjointSet s a)
newDisjointSetSized n = DisjointSet <$> newSized n



findRoot :: (Eq a, Hashable a) => DisjointSet s a -> a -> ST s (Rank, a)
findRoot ds@(DisjointSet v) i = do
    u <- lookup v i
    case u of
        Nothing -> pure (minBound, i)
        -- ^ create new point
        Just (w, j) | i == j -> pure (w, j)
        -- ^ arrived at root
        Just (_, j) -> do
            (w, k) <- findRoot ds j
            when (j /= k) $ insert v i (w, k)
            -- ^ path compression
            pure (w, k)
{-# SPECIALIZE findRoot :: DisjointSet s Int -> Int -> ST s (Rank, Int) #-}


union :: (Eq a, Hashable a) => DisjointSet s a -> a -> a -> ST s ()
union ds@(DisjointSet v) x y = do
    (w1, i1) <- findRoot ds x
    (w2, i2) <- findRoot ds y
    let !w0 = succ w1
    unless (i1 == i2)
      $ case w1 `compare` w2 of -- merge the smaller tree, left-biased
          EQ -> insert v i2 (w0, i1) >> insert v i1 (w0, i1)
          GT -> insert v i2 (w1, i1) -- ^ union by rank
          LT -> insert v i1 (w2, i2)
{-# SPECIALIZE union :: DisjointSet s Int -> Int -> Int -> ST s () #-}
{-# INLINABLE union #-}


representation :: (Eq a, Hashable a) => DisjointSet s a -> a -> ST s a
representation ds x = snd <$> findRoot ds x
{-# SPECIALIZE representation :: DisjointSet s Int -> Int -> ST s Int #-}
{-# INLINABLE representation #-}


equivalent :: (Eq a, Hashable a) => DisjointSet s a -> a -> a -> ST s Bool
equivalent ds x y = (==) <$> representation ds x <*> representation ds y
{-# SPECIALIZE equivalent :: DisjointSet s Int -> Int -> Int -> ST s Bool #-}
{-# INLINABLE equivalent #-}


singleton :: (Eq a, Hashable a) => DisjointSet s a -> a -> ST s Bool
singleton (DisjointSet v) x = isNothing <$> lookup v x
{-# SPECIALIZE singleton :: DisjointSet s Int -> Int -> ST s Bool #-}
{-# INLINABLE singleton #-}

