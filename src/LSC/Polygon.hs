-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module LSC.Polygon
  ( Polygon, constructPolygon
  , Polygon', path, z
  , polygon, emphasiseHeight
  , containingBox, simplePolygon
  , columns
  ) where

import Control.Applicative
import Control.Conditional
import Control.DeepSeq
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Bifunctor
import Data.Foldable (maximumBy)
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Y
import Data.IntSet (IntSet)
import qualified Data.IntSet as X
import Data.Maybe
import Data.Vector hiding (null, minimum, maximum, maximumBy)

import GHC.Generics

import LSC.Component



data Polygon l x y = Polygon
  { _z    :: !IntSet
  , _path :: Vector (x, y)
  } deriving (Generic, Functor, NFData, FromJSON, ToJSON, Show)


type Polygon' l a = Polygon l a a


instance Bifunctor (Polygon l) where
    bimap f g (Polygon zs h) = Polygon zs (bimap f g <$> h)



constructPolygon :: (Eq x, Eq y) => [(x, y)] -> Polygon l x y
constructPolygon = Polygon mempty . fromList . sanitise
  where
    sanitise ((x1, y1) : (x2, y2) : (x3, y3) : xs)
      | x1 == x2 && x2 == x3 || y1 == y2 && y2 == y3 
      = (x1, y1) : sanitise ((x3, y3) : xs)
    sanitise (x : xs) = x : sanitise xs
    sanitise xs = xs



makeFieldsNoPrefix ''Polygon




polygon :: (Integral x, Integral y) => Polygon l x y -> [Component l x y]
polygon p =
  [ rect x1 y1 x2 y2 & z .~ p ^. z & bimap fromIntegral fromIntegral
  | ((y1, x1), (y2, x2)) <- rectangleConversion
  $ constructPTR . toList -- run the algorithm mirrored around `y = x`
  $ bimap fromIntegral fromIntegral p ^. path
  ]



emphasiseHeight :: (Integral x, Integral y) => Polygon l x y -> Component l x y
emphasiseHeight = maximumBy (compare `on` liftA2 (,) height width) . polygon



columns :: (Integral x, Ord y) => x -> x -> Polygon l x y -> [Component l x y]
columns offset step = cut . containingBox
  where
    off = offset + step `div` 2
    cut c | width c <= 0 = []
    cut c = over r (min k) c : cut (over l (max k) c)
      where k = succ (((c ^. l) + off) `div` step) * step - off



containingBox :: (Ord x, Ord y) => Polygon l x y -> Component l x y
containingBox p = rect
  (minimum $ p ^. path <&> fst)
  (minimum $ p ^. path <&> snd)
  (maximum $ p ^. path <&> fst)
  (maximum $ p ^. path <&> snd)
  & z .~ p ^. z



simplePolygon :: Component l x y -> Polygon l x y
simplePolygon p
  = Polygon (p ^. z)
  $ fromListN 4
    [ (p ^. l, p ^. b)
    , (p ^. l, p ^. t)
    , (p ^. r, p ^. t)
    , (p ^. r, p ^. b)
    ]



type PTR = IntMap IntSet


constructPTR :: [(Int, Int)] -> PTR
constructPTR
  = Y.fromListWith (<>)
  . fmap (second X.singleton)


maintain :: (Int, Int) -> PTR -> PTR
maintain (x, y) ptr
  | Just True <- X.member x <$> Y.lookup y ptr
  = remove (x, y) ptr
maintain (x, y) ptr
  = insert (x, y) ptr


insert :: (Int, Int) -> PTR -> PTR
insert (x, y) = Y.alter (pure . maybe (X.singleton x) (X.insert x)) y


remove :: (Int, Int) -> PTR -> PTR
remove (x, y) = Y.alter (select X.null (const Nothing) pure . X.delete x =<<) y



rectangleConversion :: PTR -> [((Int, Int), (Int, Int))]
rectangleConversion ptr
  | null ptr
  = []
rectangleConversion ptr
  = ((xk, yk), (xl, ym)) : rectangleConversion (next ptr)

    where

    next = maintain (xl, ym) . maintain (xk, ym) . remove (xl, yl) . remove (xk, yk)

    Just (xk, yk) = getPk ptr
    Just (xl, yl) = getPl (xk, yk) ptr
    Just (_ , ym) = getPm (xk, yk) xl ptr


getPk :: PTR -> Maybe (Int, Int)
getPk
  = fmap (\ (y, xs) -> (X.findMin xs, y))
  . Y.lookupMin


getPl :: (Int, Int) -> PTR -> Maybe (Int, Int)
getPl (x, y)
  = fmap (, y)
  . X.lookupGT x <=< Y.lookup y


getPm :: (Int, Int) -> Int -> PTR -> Maybe (Int, Int)
getPm (xk, yk) xl
  = listToMaybe
  . ifoldMap (\ y -> fmap (, y) . X.elems . snd . X.split (pred xk) . fst . X.split xl)
  . snd . Y.split yk -- ^ this is suboptimal

