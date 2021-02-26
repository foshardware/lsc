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
  , path, z
  , polygon, emphasiseHeight
  , shiftX, shiftY
  , simplePolygon
  ) where

import Control.Applicative
import Control.Arrow
import Control.Conditional
import Control.DeepSeq
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Foldable (maximumBy)
import Data.Function
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Maybe
import Data.Vector hiding (null, maximumBy)

import GHC.Generics

import LSC.Component



data Polygon l a = Polygon
  { _z    :: !IntSet
  , _path :: Vector (a, a)
  } deriving (Generic, Functor, NFData, FromJSON, ToJSON, Show)


instance ToJSON a => Hashable (Polygon l a) where
    hashWithSalt s = hashWithSalt s . encode



constructPolygon :: Eq a => [(a, a)] -> Polygon l a
constructPolygon = Polygon mempty . fromList . sanitise
    where
    sanitise ((x1, y1) : (x2, y2) : (x3, y3) : xs)
      | x1 == x2 && x2 == x3 || y1 == y2 && y2 == y3 
      = (x1, y1) : sanitise ((x3, y3) : xs)
    sanitise (x : xs) = x : sanitise xs
    sanitise xs = xs



makeFieldsNoPrefix ''Polygon



polygon :: Integral a => Polygon l a -> [Component l a]
polygon p =
  [ rect x1 y1 x2 y2 & z .~ p ^. z <&> fromIntegral
  | ((y1, x1), (y2, x2)) <- rectangleConversion
  $ constructPTR . toList
  $ fmap swap -- convert the polygon mirrored around `f(x) = x`
  $ fmap fromIntegral p ^. path
  ]



emphasiseHeight :: Integral a => Polygon l a -> Component l a
emphasiseHeight = maximumBy (compare `on` liftA2 (,) height width) . polygon



simplePolygon :: Component l a -> Polygon l a
simplePolygon p
  = Polygon (p ^. z)
  $ fromListN 4
    [ (p ^. l, p ^. b)
    , (p ^. l, p ^. t)
    , (p ^. r, p ^. t)
    , (p ^. r, p ^. b)
    ]



shiftX :: Num a => a -> Polygon l a -> Polygon l a
shiftX = over path . fmap . first . (+)

shiftY :: Num a => a -> Polygon l a -> Polygon l a
shiftY = over path . fmap . second . (+)



swap :: (a, b) -> (b, a)
swap ~(x, y) = (y, x)



type PTR = IntMap IntSet


constructPTR :: [(Int, Int)] -> PTR
constructPTR
  = Map.fromListWith (<>)
  . fmap (second Set.singleton)
  . fmap swap


maintain :: (Int, Int) -> PTR -> PTR
maintain (x, y) ptr
  | Just True <- Set.member x <$> Map.lookup y ptr
  = remove (x, y) ptr
maintain (x, y) ptr
  = insert (x, y) ptr


insert :: (Int, Int) -> PTR -> PTR
insert (x, y) = Map.alter (pure . maybe (Set.singleton x) (Set.insert x)) y


remove :: (Int, Int) -> PTR -> PTR
remove (x, y) = Map.alter (select Set.null (const Nothing) pure . Set.delete x =<<) y



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
  = fmap (swap . second Set.findMin)
  . Map.lookupMin


getPl :: (Int, Int) -> PTR -> Maybe (Int, Int)
getPl (x, y)
  = fmap (, y)
  . Set.lookupGT x <=< Map.lookup y


getPm :: (Int, Int) -> Int -> PTR -> Maybe (Int, Int)
getPm (xk, yk) xl
  = listToMaybe
  . ifoldMap (\ y -> fmap (, y) . Set.elems . snd . Set.split (pred xk) . fst . Set.split xl)
  . snd . Map.split yk

