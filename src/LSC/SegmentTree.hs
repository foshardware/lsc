-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DeriveFunctor #-}

module LSC.SegmentTree
  ( SegmentTree, fromList
  , constructSegmentTree, endpoints
  , maxDensity, densityRatio, densityOn, densityOver
  , unsafeDensityRatio, unsafeDensityOver
  , push, pull
  , unsafePush, unsafePull
  , showTree

  , Stabs, skeleton
  , append, unsafeAppend, adjust, unsafeAdjust
  , stab, stabOver, unsafeStabOver

  ) where

import Data.Bifoldable
import Data.Foldable
import Data.Ratio

import LSC.BinarySearch


-- | Custom tree data type for creating stabbing queries over monoids, e. g. always
--   appending `Sum 1` counts the number of intervals intersecting the stabbing abscissa.
--
--   Note that the underlying monoid type may be changed by using `fmap`.
--
data Stabs a m
  = Tip
  | Stab  m a
  | Range m a a (Stabs a m) (Stabs a m)
  deriving (Functor, Show)



skeleton :: (Ord a, Monoid m) => [a] -> Stabs a m
skeleton []
  = Tip
skeleton xs
  = head
  . until (null . tail) ranges
  . map (Stab mempty)
  $ xs
  where
    ranges (x : y : ys) = Range mempty (fst (bounds x)) (snd (bounds y)) x y : ranges ys
    ranges ys = ys
{-# INLINABLE skeleton #-}


bounds :: Stabs a m -> (a, a)
bounds    Tip            = undefined
bounds  (Stab _ x)       = (x, x)
bounds (Range _ x y _ _) = (x, y)



append, unsafeAppend :: (Ord a, Semigroup m) => (a, a) -> m -> Stabs a m -> Stabs a m
append = unsafeAppend . asc

unsafeAppend range m = unsafeAdjust range (<> m)
{-# INLINABLE append #-}
{-# INLINABLE unsafeAppend #-}


adjust, unsafeAdjust :: Ord a => (a, a) -> (m -> m) -> Stabs a m -> Stabs a m
adjust = unsafeAdjust . asc

unsafeAdjust = go
  where
    go (x, y) f (Stab m a)
        | x <= a, a <= y
        = Stab (f m) a
    go (x, y) _ (Range m a b left right)
        | y < a || b < x
        = Range m a b left right
    go (x, y) f (Range m a b left right)
        = Range
          (if x <= a && b <= y then f m else m) a b
          (go (x, y) f left)
          (go (x, y) f right)
    go _ _ node = node
{-# INLINABLE adjust #-}
{-# INLINABLE unsafeAdjust #-}



stab :: (Ord a, Monoid m) => a -> Stabs a m -> m
stab x = unsafeStabOver (x, x)
{-# INLINABLE stab #-}


stabOver, unsafeStabOver :: (Ord a, Monoid m) => (a, a) -> Stabs a m -> m
stabOver = unsafeStabOver . asc

unsafeStabOver = go
  where
    go (x, y) (Stab m a)
        | x <= a, a <= y
        = m
    go (x, y) (Range m a b _ _)
        | x <= a, b <= y
        = m
    go (x, y) (Range _ _ _ left right)
        = l <> r
        where
          l = if x <= snd (bounds left)  then go (x, y) left  else mempty
          r = if y >= fst (bounds right) then go (x, y) right else mempty
    go _ _ = mempty
{-# INLINABLE stabOver #-}
{-# INLINABLE unsafeStabOver #-}




-- | An implementation closer to the literature
--
data SegmentTree a
  = Nil
  | Leaf     {-# UNPACK #-} !Int a
  | Interval {-# UNPACK #-} !Int {-# UNPACK #-} !Int (a, a) (SegmentTree a) (SegmentTree a)
  deriving Show


-- | Fold over non-empty endpoints
--
instance Foldable SegmentTree where
    foldMap = foldMapStabs
    foldr = foldrStabs
    null = nullSegment



fromList :: Ord a => [(a, a)] -> SegmentTree a
fromList = constructSegmentTree <$> endpoints <*> id
{-# INLINABLE fromList #-}


-- | This is unsafe: `constructSegmentTree` assumes its first argument to be distinct
--   and in ascending order
--
constructSegmentTree :: Ord a => [a] -> [(a, a)] -> SegmentTree a
constructSegmentTree [] _
  = Nil
constructSegmentTree xs is
  = flip (foldl' (flip push)) is
  . head
  . until (null . tail) intervals
  . map (Leaf 0)
  $ xs
  where
    intervals (x : y : ys) = Interval 0 0 (lower x, upper y) x y : intervals ys
    intervals ys = ys
{-# INLINABLE constructSegmentTree #-}


endpoints :: Ord a => [(a, a)] -> [a]
endpoints = binarySearch . foldMap biList
{-# INLINABLE endpoints #-}


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
asc (x, y) | x > y = (y, x)
asc (x, y) = (x, y)
{-# INLINABLE asc #-}


densityRatio, unsafeDensityRatio :: Ord a => (a, a) -> SegmentTree a -> Rational
densityRatio _ node
  | nullSegment node
  = error "densityRatio: empty segment"
densityRatio interval node
  = unsafeDensityRatio (asc interval) node

unsafeDensityRatio interval node
  = fromIntegral (unsafeDensityOver interval node)
  % fromIntegral (maxDensity node)
{-# INLINABLE densityRatio #-}
{-# INLINABLE unsafeDensityRatio #-}


densityOn :: Ord a => a -> SegmentTree a -> Int
densityOn x = unsafeDensityOver (x, x)
{-# INLINABLE densityOn #-}


densityOver, unsafeDensityOver :: Ord a => (a, a) -> SegmentTree a -> Int
densityOver = unsafeDensityOver . asc

unsafeDensityOver = go
  where
    go (x, y) (Leaf density a)
        | x <= a, a <= y
        = density
    go (x, y) (Interval density _ (a, b) _ _)
        | x <= a, b <= y
        = density
    go (x, y) (Interval _ delete _ left right)
        = subtract delete $ max l r
        where
          l = if x <= upper left  then go (x, y) left  else delete
          r = if y >= lower right then go (x, y) right else delete
    go _ _ = 0
{-# INLINABLE densityOver #-}
{-# INLINABLE unsafeDensityOver #-}


-- | Insert an edge, mnemonic: to push a needle into the haystack
--
push, unsafePush :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
push = unsafePush . asc

unsafePush = go
  where
    go (x, y) (Leaf density a)
        | x <= a, a <= y
        = Leaf (succ density) a
    go (x, y) (Interval _ delete (a, b) left right)
        = Interval (subtract delete $ maxDensity l `max` maxDensity r) delete (a, b) l r
        where
          l = if x <= upper left  then go (x, y) left  else left
          r = if y >= lower right then go (x, y) right else right
    go _ node = node
{-# INLINABLE push #-}
{-# INLINABLE unsafePush #-}


-- | Delete an edge, mnemonic: to pull a needle from the haystack
--
pull, unsafePull :: Ord a => (a, a) -> SegmentTree a -> SegmentTree a
pull = unsafePull . asc

unsafePull = go
  where
    go (x, y) (Leaf density a)
        | x <= a, a <= y
        = Leaf (pred density) a
    go (x, y) (Interval density delete (a, b) left right)
        | x <= a, b <= y
        = Interval (pred density) (succ delete) (a, b) left right
    go (x, y) (Interval _ delete (a, b) left right)
        = Interval (subtract delete $ maxDensity l `max` maxDensity r) delete (a, b) l r
        where
          l = if x <= upper left  then go (x, y) left  else left
          r = if y >= lower right then go (x, y) right else right
    go _ node = node
{-# INLINABLE pull #-}
{-# INLINABLE unsafePull #-}



foldMapStabs :: Monoid m => (a -> m) -> SegmentTree a -> m
foldMapStabs = go 0
  where
    go k _ node | maxDensity node <= k = mempty
    go k f (Interval _ d _ l r) = go (k + d) f l <> go (k + d) f r
    go _ f     (Leaf _ a)       = f a
    go _ _       Nil            = mempty
{-# INLINABLE foldMapStabs #-}


foldrStabs :: (a -> b -> b) -> b -> SegmentTree a -> b
foldrStabs = go 0
  where
    go k _ z node | maxDensity node <= k = z
    go k f z (Interval _ d _ l r) = go (k + d) f (go (k + d) f z r) l
    go _ f z     (Leaf _ a)       = f a z
    go _ _ z       Nil            = z



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

