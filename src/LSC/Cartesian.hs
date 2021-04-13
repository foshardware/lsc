-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module LSC.Cartesian where

import Control.DeepSeq
import Control.Lens

import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Maybe
import Data.Semigroup

import Data.Aeson (FromJSON, ToJSON)

import GHC.Generics


class (Bifunctor f, Bifoldable f, Integral x, Integral y) => Cartesian f x y where

    moveX :: x -> f x y -> f x y
    moveX = first . (+)

    moveY :: y -> f x y -> f x y
    moveY = second . (+)


    width :: f x y -> x
    width = subtract <$> minX <*> maxX

    height :: f x y -> y
    height = subtract <$> minY <*> maxY


    center :: f x y -> (x, y)
    center = (,) <$> centerX <*> centerY

    centerX :: f x y -> x
    centerX = (`div` 2) . ((+) <$> minX <*> maxX)

    centerY :: f x y -> y
    centerY = (`div` 2) . ((+) <$> minY <*> maxY)


    relocateX :: x -> f x y -> f x y
    relocateX x = first (+ x) . (first . subtract <$> centerX <*> id)

    relocateY :: y -> f x y -> f x y
    relocateY y = second (+ y) . (second . subtract <$> centerY <*> id)


    minX, maxX :: f x y -> x
    minX
      = getMin
      . fromMaybe (error "minX: empty structure")
      . bifoldMap (Just . Min) (const Nothing)
    maxX
      = getMax
      . fromMaybe (error "maxX: empty structure")
      . bifoldMap (Just . Max) (const Nothing)

    minY, maxY :: f x y -> y
    minY
      = getMin
      . fromMaybe (error "minY: empty structure")
      . bifoldMap (const Nothing) (Just . Min)
    maxY
      = getMax
      . fromMaybe (error "maxY: empty structure")
      . bifoldMap (const Nothing) (Just . Max)


    relocateL, relocateR :: x -> f x y -> f x y
    relocateL x = first (+ x) . (first . subtract <$> minX <*> id)
    relocateR x = first (+ x) . (first . subtract <$> maxX <*> id)

    relocateB, relocateT :: y -> f x y -> f x y
    relocateB y = second (+ y) . (second . subtract <$> minY <*> id)
    relocateT y = second (+ y) . (second . subtract <$> maxY <*> id)


    abscissae :: f x y -> [x]
    abscissae = bifoldr (:) (const id) []

    ordinates :: f x y -> [y]
    ordinates = bifoldr (const id) (:) []



instance (Integral x, Integral y) => Cartesian (,) x y


data Line x y = Line (x, y) (x, y) deriving
  ( Eq, Ord
  , Functor
  , Foldable
  , Traversable
  , Generic
  , NFData
  , FromJSON, ToJSON
  , Show
  )

$(deriveBifunctor     ''Line)
$(deriveBifoldable    ''Line)
$(deriveBitraversable ''Line)

type Line' a = Line a a

instance (Integral x, Integral y) => Cartesian Line x y where

    minX (Line (x, _) _) = x
    maxX (Line _ (x, _)) = x

    minY (Line (_, y) _) = y
    maxY (Line _ (_, y)) = y



line :: Iso ((a, b), (a, b)) ((x, y), (x, y)) (Line a b) (Line x y)
line = iso
  (\ ((x1, y1), (x2, y2)) ->  Line (x1, y1) (x2, y2) )
  (\ (Line (x1, y1) (x2, y2)) -> ((x1, y1), (x2, y2)))


