-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LSC.Cartesian where

import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Maybe
import Data.Semigroup


class (Bifunctor f, Bifoldable f, Integral x, Integral y) => Cartesian f x y where

    moveX :: x -> f x y -> f x y
    moveX = first . (+)

    moveY :: y -> f x y -> f x y
    moveY = second . (+)


    width :: f x y -> x
    width = subtract <$> minimumX <*> maximumX

    height :: f x y -> y
    height = subtract <$> minimumY <*> maximumY


    center :: f x y -> (x, y)
    center = (,) <$> centerX <*> centerY

    centerX :: f x y -> x
    centerX = (`div` 2) . liftA2 (+) minimumX maximumX

    centerY :: f x y -> y
    centerY = (`div` 2) . liftA2 (+) minimumY maximumY


    relocateX :: x -> f x y -> f x y
    relocateX x = first (+ x) . liftA2 (first . subtract) centerX id

    relocateY :: y -> f x y -> f x y
    relocateY y = second (+ y) . liftA2 (second . subtract) centerY id


    minimumX, maximumX :: f x y -> x
    minimumX
      = getMin
      . fromMaybe (error "minimumX: empty structure")
      . bifoldMap (Just . Min) (const Nothing)
    maximumX
      = getMax
      . fromMaybe (error "maximumX: empty structure")
      . bifoldMap (Just . Max) (const Nothing)

    minimumY, maximumY :: f x y -> y
    minimumY
      = getMin
      . fromMaybe (error "minimumY: empty structure")
      . bifoldMap (const Nothing) (Just . Min)
    maximumY
      = getMax
      . fromMaybe (error "maximumY: empty structure")
      . bifoldMap (const Nothing) (Just . Max)


    relocateL, relocateR :: x -> f x y -> f x y
    relocateL x = first (+ x) . liftA2 (first . subtract) minimumX id
    relocateR x = first (+ x) . liftA2 (first . subtract) maximumX id

    relocateB, relocateT :: y -> f x y -> f x y
    relocateB y = second (+ y) . liftA2 (second . subtract) minimumY id
    relocateT y = second (+ y) . liftA2 (second . subtract) maximumY id


    abscissae :: f x y -> [x]
    abscissae = bifoldr (:) (flip const) []

    ordinates :: f x y -> [y]
    ordinates = bifoldr (flip const) (:) []


instance (Integral x, Integral y) => Cartesian (,) x y


