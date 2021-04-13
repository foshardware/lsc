-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module LSC.BinarySearch where

import Data.Function (on)
import Data.List (sort, group, groupBy)


newtype Distinct a b = Distinct { unDistinct :: (a, b) }
  deriving (Functor, Foldable, Show)

distinct :: a -> b -> Distinct a b
distinct = curry Distinct

getDistinct :: Distinct a b -> b
getDistinct = snd . unDistinct


instance Eq a => Eq (Distinct a b) where
    (==) = (==) `on` fst . unDistinct

instance Ord a => Ord (Distinct a b) where
    compare = compare `on` fst . unDistinct



binarySearch :: Ord a => [a] -> [a]
binarySearch = map head . group . sort
{-# INLINABLE binarySearch #-}


unstableUnique :: Ord a => [a] -> [a]
unstableUnique = binarySearch
{-# INLINABLE unstableUnique #-}


groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)
{-# INLINABLE groupOn #-}



distinctPairs :: [a] -> [(a, a)]
distinctPairs = go
  where go (x : xs) = zip (repeat x) xs ++ go xs
        go [] = []



median :: Integral a => [a] -> a
median
  = uncurry div
  . foldl (\ (a, len) x -> (a + x, len + 1)) (0, 0)
  . medianElements
{-# INLINABLE median #-}


medianElements :: [a] -> [a]
medianElements zs = go zs zs
  where go (x : _)        [_] = [x]
        go (x : y : _) [_, _] = [x, y]
        go (_ : xs) (_ : _ : ys) = go xs ys
        go _ _ = error "medianElements: empty list"

