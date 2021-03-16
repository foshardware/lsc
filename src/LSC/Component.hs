-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module LSC.Component where

import Control.DeepSeq
import Control.Lens
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.IntSet (IntSet, union, intersection)
import Data.Semigroup

import Data.Aeson (FromJSON, ToJSON)

import GHC.Generics

import LSC.Cartesian



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
  } deriving
  ( Eq, Ord
  , Functor, Foldable
  , Generic
  , NFData
  , FromJSON, ToJSON
  , Show
  )

$(deriveBifunctor  ''Component)
$(deriveBifoldable ''Component)

type Component' l a = Component l a a

rect :: x -> y -> x -> y -> Component l x y
rect x1 y1 x2 y2 = Component x1 y1 x2 y2 mempty mempty
{-# INLINE rect #-}


instance (Integral x, Integral y) => Cartesian (Component l) x y where

    minX = _l
    maxX = _r

    minY = _b
    maxY = _t

    centerX c = div (_l c + _r c) 2
    centerY c = div (_b c + _t c) 2



newtype BoundingBox l x y = BoundingBox { getBoundingBox :: Component l x y }
  deriving (Eq, Functor, Foldable)

$(deriveBifunctor  ''BoundingBox)
$(deriveBifoldable ''BoundingBox)

type BoundingBox' l a = BoundingBox l a a

instance (Integral x, Integral y) => Cartesian (BoundingBox l) x y where

    width  = width  . getBoundingBox
    height = height . getBoundingBox

    abscissae d = let Component x1 _ x2 _ _ _ = getBoundingBox d in [x1, x2]
    ordinates d = let Component _ y1 _ y2 _ _ = getBoundingBox d in [y1, y2]


instance (Ord x, Ord y) => Semigroup (BoundingBox l x y) where
    BoundingBox (Component l1 b1 r1 t1 ls1 o) <> BoundingBox (Component l2 b2 r2 t2 ls2 _)
      = BoundingBox (Component (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2) (union ls1 ls2) o)
    stimes = stimesIdempotent

instance (Ord x, Ord y, Bounded x, Bounded y) => Monoid (BoundingBox l x y) where
    mempty = BoundingBox (rect maxBound maxBound minBound minBound)
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif



newtype Overlap l x y = Overlap { getOverlap :: Component l x y }
  deriving (Eq, Functor, Foldable)

$(deriveBifunctor  ''Overlap)
$(deriveBifoldable ''Overlap)

type Overlap' l a = Overlap l a a

instance (Integral x, Integral y) => Cartesian (Overlap l) x y where

    width  = width  . getOverlap
    height = height . getOverlap


instance (Ord x, Ord y) => Semigroup (Overlap l x y) where
    Overlap (Component l1 b1 r1 t1 ls1 o) <> Overlap (Component l2 b2 r2 t2 ls2 _)
      = Overlap (Component (max l1 l2) (max b1 b2) (min r1 r2) (min t1 t2) (intersection ls1 ls2) o)
    stimes = stimesIdempotent

instance (Ord x, Ord y, Bounded x, Bounded y) => Monoid (Overlap l x y) where
    mempty = Overlap (rect minBound minBound maxBound maxBound)
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif



data Line x y = Line (x, y) (x, y) deriving
  ( Eq, Ord
  , Functor, Foldable
  , Generic
  , NFData
  , FromJSON, ToJSON
  , Show
  )

$(deriveBifunctor  ''Line)
$(deriveBifoldable ''Line)

type Line' a = Line a a


instance (Integral x, Integral y) => Cartesian Line x y where

    minX (Line (x, _) _) = x
    maxX (Line _ (x, _)) = x

    minY (Line (_, y) _) = y
    maxY (Line _ (_, y)) = y



line :: Iso' ((x, y), (x, y)) (Line x y)
line = iso
  (\ ((x1, y1), (x2, y2)) -> (Line (x1, y1) (x2, y2)))
  (\ (Line (x1, y1) (x2, y2)) -> ((x1, y1), (x2, y2)))



makeFieldsNoPrefix ''Component

makeFieldsNoPrefix ''Line



implode :: (Integral x, Integral y) => Component l x y -> Component l x y
implode = rect <$> centerX <*> centerY <*> centerX <*> centerY
{-# INLINABLE implode #-}


inline :: Cartesian f x y => Component l x y -> f x y -> f x y
inline a = case a ^. orientation of
    FN -> moveX (a ^. l + a ^. r) . first negate . bimap (+ a ^. l) (+ a ^. b)
    _  -> bimap (+ a ^. l) (+ a ^. b)
{-# INLINABLE inline #-}


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


