-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module LSC.Component where

#if MIN_VERSION_base(4,10,0)
#else
import Data.Semigroup
#endif

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Hashable
import Data.IntSet (IntSet, union, intersection)

import Data.Aeson (encode, FromJSON, ToJSON)

import GHC.Generics



data Layer
  = AnyLayer
  | Metal1
  | Metal2
  | Metal3
  | Metal4
  | Metal5
  | Metal6
  | Metal7
  | Metal8
  | Metal9
  | Metal10
  deriving (Eq, Ord, Enum, Generic, FromJSON, ToJSON, Hashable, Show)


data Orientation = N | S | W | E | FN | FS | FW | FE
  deriving (Eq, Ord, Generic, NFData, FromJSON, ToJSON, Hashable, Show)


instance Semigroup Orientation where

  N <> a = a
  a <> N = a

  S <> S = N
  S <> E = W
  S <> W = E

  W <> E = N
  W <> W = S

  E <> E = S

  FN <> FN = N
  FN <> FS = S
  FN <> FW = W
  FN <> FE = E

  FN <> S = FS
  FN <> W = FW
  FN <> E = FE


  FS <> FS = N
  FS <> FW = E
  FS <> FE = W

  FS <> S = FN
  FS <> W = FE
  FS <> E = FW


  FW <> FE = N
  FW <> FW = S

  FW <> S = FE
  FW <> W = FS
  FW <> E = FN


  FE <> FE = S

  FE <> S = FW
  FE <> W = FS
  FE <> E = FN


  a <> b = b <> a


instance Monoid Orientation where
  mempty = N
#if MIN_VERSION_base(4,11,0)
#else
  mappend = (<>)
#endif



type Ring l a = Component l (Component l a)


data Component l a = Component
  { _l :: !a
  , _b :: !a
  , _r :: !a
  , _t :: !a
  , _z :: !IntSet
  , _orientation :: Orientation
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData, FromJSON, ToJSON, Show)

instance ToJSON a => Hashable (Component l a) where
    hashWithSalt s = hashWithSalt s . encode


rect :: a -> a -> a -> a -> Component l a
rect x1 y1 x2 y2 = Component x1 y1 x2 y2 mempty mempty
{-# INLINE rect #-}


instance Ord a => Semigroup (Component l a) where
    p <> q = getBoundingBox (BoundingBox p <> BoundingBox q)

instance (Ord a, Bounded a) => Monoid (Component l a) where
    mempty = getBoundingBox mempty
#if MIN_VERSION_base(4,11,0)
#else
    mappend = (<>)
#endif


newtype BoundingBox l a = BoundingBox { getBoundingBox :: Component l a }

instance Ord a => Semigroup (BoundingBox l a) where
    BoundingBox (Component l1 b1 r1 t1 ls1 o) <> BoundingBox (Component l2 b2 r2 t2 ls2 _)
        = BoundingBox (Component (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2) (union ls1 ls2) o)

instance (Ord a, Bounded a) => Monoid (BoundingBox l a) where
    mempty = BoundingBox (rect maxBound maxBound minBound minBound)
#if MIN_VERSION_base(4,11,0)
#else
    mappend = (<>)
#endif


newtype Overlap l a = Overlap { getOverlap :: Component l a }

instance Ord a => Semigroup (Overlap l a) where
    Overlap (Component l1 b1 r1 t1 ls1 o) <> Overlap (Component l2 b2 r2 t2 ls2 _)
        = Overlap (Component (max l1 l2) (max b1 b2) (min r1 r2) (min t1 t2) (intersection ls1 ls2) o)

instance (Ord a, Bounded a) => Monoid (Overlap l a) where
    mempty = Overlap (rect minBound minBound maxBound maxBound)
#if MIN_VERSION_base(4,11,0)
#else
    mappend = (<>)
#endif


data Line a = Line (a, a) (a, a)
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData, FromJSON, ToJSON, Hashable, Show)


line :: Iso' ((a, a), (a, a)) (Line a)
line = iso
  (\ ((x1, y1), (x2, y2)) -> (Line (x1, y1) (x2, y2)))
  (\ (Line (x1, y1) (x2, y2)) -> ((x1, y1), (x2, y2)))



makeFieldsNoPrefix ''Component

makeFieldsNoPrefix ''Line



implode :: Integral a => Component l a -> Component l a
implode c = rect (centerX c) (centerY c) (centerX c) (centerY c)
{-# INLINABLE implode #-}


center :: Integral a => Component l a -> (a, a)
center = liftA2 (,) centerX centerY
{-# INLINABLE center #-}


centerX :: Integral a => Component l a -> a
centerX p = div (p ^. r + p ^. l) 2
{-# INLINABLE centerX #-}


centerY :: Integral a => Component l a -> a
centerY p = div (p ^. t + p ^. b) 2
{-# INLINABLE centerY #-}


moveX :: Num a => a -> Component l a -> Component l a
moveX x p = p &~ do
    l += x
    r += x
{-# INLINABLE moveX #-}


moveY :: Num a => a -> Component l a -> Component l a
moveY y p = p &~ do
    b += y
    t += y
{-# INLINABLE moveY #-}


relocateL :: Num a => a -> Component l a -> Component l a
relocateL x p = p &~ do
    l .= x
    r += x - p ^. l
{-# INLINABLE relocateL #-}


relocateR :: Num a => a -> Component l a -> Component l a
relocateR x p = p &~ do
    l += x - p ^. r
    r .= x
{-# INLINABLE relocateR #-}


relocateB :: Num a => a -> Component l a -> Component l a
relocateB y p = p &~ do
    b .= y
    t += y - p ^. b
{-# INLINABLE relocateB #-}


relocateX :: Integral a => a -> Component l a -> Component l a
relocateX x p = p &~ do
    l += x - centerX p
    r += x - centerX p
{-# INLINABLE relocateX #-}


relocateY :: Integral a => a -> Component l a -> Component l a
relocateY y p = p &~ do
    b += y - centerY p
    t += y - centerY p
{-# INLINABLE relocateY #-}


width, height :: Num a => Component l a -> a
width  p = p ^. r - p ^. l
height p = p ^. t - p ^. b
{-# INLINABLE width  #-}
{-# INLINABLE height #-}


areaOverlap :: Ord a => Component l a -> Component l a -> Bool
areaOverlap p q = overlapX p q && overlapY p q
{-# INLINABLE areaOverlap #-}


overlapX :: Ord a => Component l a -> Component l a -> Bool
overlapX p q = p ^. l <= q ^. l && p ^. r > q ^. l || p ^. l > q ^. l && p ^. l < q ^. r
{-# INLINABLE overlapX #-}


overlapY :: Ord a => Component l a -> Component l a -> Bool
overlapY p q = p ^. b <= q ^. b && p ^. t > q ^. b || p ^. b > q ^. b && p ^. b < q ^. t
{-# INLINABLE overlapY #-}


castLayer :: Component k a -> Component l a
castLayer (Component x1 y1 x2 y2 ls o) = Component x1 y1 x2 y2 ls o


inner, outer :: Ring l a -> Component l a
inner p = rect (p ^. l . r) (p ^. b . t) (p ^. r . l) (p ^. t . b)
outer p = rect (p ^. l . l) (p ^. b . b) (p ^. r . r) (p ^. t . t)


hypothenuse :: Component l a -> Line a
hypothenuse c = Line (c ^. l, c ^. b) (c ^. r, c ^. t)


component :: Line a -> Component l a
component (Line (x1, y1) (x2, y2)) = Component x1 y1 x2 y2 mempty mempty

