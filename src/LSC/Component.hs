-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.Component where

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.IntSet (IntSet, union, intersection)

import Data.Aeson (FromJSON, ToJSON)

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
  deriving (Eq, Ord, Enum, Generic, FromJSON, ToJSON, Show)


data Orientation = N | FN | S | FS | W | FW | E | FE
  deriving (Eq, Ord, Enum, Generic, NFData, FromJSON, ToJSON, Show)


instance Semigroup Orientation where

  N <> o = o
  o <> N = o

  S  <>  S = N
  W  <>  E = N
  E  <>  W = N
  FN <> FN = N
  FS <> FS = N
  FW <> FE = N
  FE <> FW = N

  W  <>  W = S
  E  <>  E = S
  FN <> FS = S
  FS <> FN = S
  FW <> FW = S
  FE <> FE = S

  S  <>  E = W
  E  <>  S = W
  FN <> FW = W
  FS <> FE = W
  FW <> FN = W
  FE <> FS = W

  S  <>  W = E
  W  <>  S = E
  FN <> FE = E
  FS <> FW = E
  FW <> FS = E
  FE <> FN = E

  S  <> FS = FN
  W  <> FE = FN
  E  <> FW = FN
  FS <>  S = FN
  FW <>  E = FN
  FE <>  W = FN

  S  <> FN = FS
  W  <> FW = FS
  E  <> FE = FS
  FN <>  S = FS
  FW <>  W = FS
  FE <>  E = FS

  S  <> FE = FW
  W  <> FN = FW
  E  <> FS = FW
  FN <>  W = FW
  FS <>  E = FW
  FE <>  S = FW

  S  <> FW = FE
  W  <> FS = FE
  E  <> FN = FE
  FN <>  E = FE
  FS <>  W = FE
  FW <>  S = FE


instance Monoid Orientation where
  mempty = N
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif



type Ring l x y = Component l (Component l x y) (Component l x y)

type Ring' l a = Ring l a a


data Component l x y = Component
  { _l :: !x
  , _b :: !y
  , _r :: !x
  , _t :: !y
  , _z :: !IntSet
  , _orientation :: !Orientation
  } deriving (Eq, Ord, Functor, Generic, NFData, FromJSON, ToJSON, Show)


type Component' l a = Component l a a


rect :: x -> y -> x -> y -> Component l x y
rect x1 y1 x2 y2 = Component x1 y1 x2 y2 mempty mempty
{-# INLINE rect #-}


instance (Ord x, Ord y) => Semigroup (Component l x y) where
    p <> q = getBoundingBox (BoundingBox p <> BoundingBox q)

instance (Ord x, Ord y, Bounded x, Bounded y) => Monoid (Component l x y) where
    mempty = getBoundingBox mempty
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif


instance Bifunctor (Component l) where
    bimap f g (Component x1 y1 x2 y2 zs o) = Component (f x1) (g y1) (f x2) (g y2) zs o



newtype BoundingBox l x y = BoundingBox { getBoundingBox :: Component l x y }

type BoundingBox' l a = BoundingBox l a a


instance (Ord x, Ord y) => Semigroup (BoundingBox l x y) where
    BoundingBox (Component l1 b1 r1 t1 ls1 o) <> BoundingBox (Component l2 b2 r2 t2 ls2 _)
      = BoundingBox (Component (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2) (union ls1 ls2) o)

instance (Ord x, Ord y, Bounded x, Bounded y) => Monoid (BoundingBox l x y) where
    mempty = BoundingBox (rect maxBound maxBound minBound minBound)
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif


instance Bifunctor (BoundingBox l) where
    bimap f g (BoundingBox h) = BoundingBox (bimap f g h)



newtype Overlap l x y = Overlap { getOverlap :: Component l x y }

type Overlap' l a = Overlap l a a


instance (Ord x, Ord y) => Semigroup (Overlap l x y) where
    Overlap (Component l1 b1 r1 t1 ls1 o) <> Overlap (Component l2 b2 r2 t2 ls2 _)
      = Overlap (Component (max l1 l2) (max b1 b2) (min r1 r2) (min t1 t2) (intersection ls1 ls2) o)

instance (Ord x, Ord y, Bounded x, Bounded y) => Monoid (Overlap l x y) where
    mempty = Overlap (rect minBound minBound maxBound maxBound)
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif


instance Bifunctor (Overlap l) where
    bimap f g (Overlap h) = Overlap (bimap f g h)



data Line x y = Line (x, y) (x, y)
  deriving (Eq, Ord, Functor, Generic, NFData, FromJSON, ToJSON, Show)


type Line' a = Line a a


instance Bifunctor Line where
    bimap f g (Line j k) = Line (bimap f g j) (bimap f g k)


line :: Iso' ((x, y), (x, y)) (Line x y)
line = iso
  (\ ((x1, y1), (x2, y2)) -> (Line (x1, y1) (x2, y2)))
  (\ (Line (x1, y1) (x2, y2)) -> ((x1, y1), (x2, y2)))



makeFieldsNoPrefix ''Component

makeFieldsNoPrefix ''Line



implode :: (Integral x, Integral y) => Component l x y -> Component l x y
implode c = rect (centerX c) (centerY c) (centerX c) (centerY c)
{-# INLINABLE implode #-}


center :: (Integral x, Integral y) => Component l x y -> (x, y)
center = liftA2 (,) centerX centerY
{-# INLINABLE center #-}


centerX :: Integral x => Component l x y -> x
centerX p = div (p ^. r + p ^. l) 2
{-# INLINABLE centerX #-}


centerY :: Integral y => Component l x y -> y
centerY p = div (p ^. t + p ^. b) 2
{-# INLINABLE centerY #-}


moveX :: Num x => x -> Component l x y -> Component l x y
moveX x p = p &~ do
    l += x
    r += x
{-# INLINABLE moveX #-}


moveY :: Num y => y -> Component l x y -> Component l x y
moveY y p = p &~ do
    b += y
    t += y
{-# INLINABLE moveY #-}


relocateL :: Num x => x -> Component l x y -> Component l x y
relocateL x p = p &~ do
    l .= x
    r += x - p ^. l
{-# INLINABLE relocateL #-}


relocateR :: Num x => x -> Component l x y -> Component l x y
relocateR x p = p &~ do
    l += x - p ^. r
    r .= x
{-# INLINABLE relocateR #-}


relocateB :: Num y => y -> Component l x y -> Component l x y
relocateB y p = p &~ do
    b .= y
    t += y - p ^. b
{-# INLINABLE relocateB #-}


relocateX :: Integral x => x -> Component l x y -> Component l x y
relocateX x p = p &~ do
    l += x - centerX p
    r += x - centerX p
{-# INLINABLE relocateX #-}


relocateY :: Integral y => y -> Component l x y -> Component l x y
relocateY y p = p &~ do
    b += y - centerY p
    t += y - centerY p
{-# INLINABLE relocateY #-}


width :: Num x => Component l x y -> x
width p = p ^. r - p ^. l
{-# INLINABLE width  #-}

height :: Num y => Component l x y -> y
height p = p ^. t - p ^. b
{-# INLINABLE height #-}


areaOverlap :: (Ord x, Ord y) => Component l x y -> Component l x y -> Bool
areaOverlap p q = overlapX p q && overlapY p q
{-# INLINABLE areaOverlap #-}


overlapX :: Ord x => Component l x y -> Component l x y -> Bool
overlapX p q = p ^. l <= q ^. l && p ^. r > q ^. l || p ^. l > q ^. l && p ^. l < q ^. r
{-# INLINABLE overlapX #-}


overlapY :: Ord y => Component l x y -> Component l x y -> Bool
overlapY p q = p ^. b <= q ^. b && p ^. t > q ^. b || p ^. b > q ^. b && p ^. b < q ^. t
{-# INLINABLE overlapY #-}


castLayer :: Component k x y -> Component l x y
castLayer (Component x1 y1 x2 y2 ls o) = Component x1 y1 x2 y2 ls o


inner, outer :: Ring l x y -> Component l x y
inner p = rect (p ^. l . r) (p ^. b . t) (p ^. r . l) (p ^. t . b)
outer p = rect (p ^. l . l) (p ^. b . b) (p ^. r . r) (p ^. t . t)


hypothenuse :: Component l x y -> Line x y
hypothenuse c = Line (c ^. l, c ^. b) (c ^. r, c ^. t)


component :: Line x y -> Component l x y
component (Line (x1, y1) (x2, y2)) = Component x1 y1 x2 y2 mempty mempty

