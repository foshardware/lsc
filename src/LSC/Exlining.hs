{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.Exlining where

import Data.Function
import Data.Vector hiding (replicate)
import Prelude hiding ((++), splitAt, length, concat, drop, take)

import LSC.Types
import LSC.Duval


exline (Netlist _ _ _ nodes _)
  = length <$> lyn
  where lyn = recurseLyndon 8 (GateChar <$> nodes)


recurseLyndon :: Ord a => Int -> Vector a -> Vector (Vector a)
recurseLyndon k nodes
  | i <- maxLyndon nodes
  , (xs, ys) <- splitAt i nodes
  , i > k
  = recurseLyndon k xs ++ recurseLyndon k ys
recurseLyndon _ nodes = singleton nodes

maxLyndon :: Ord a => Vector a -> Int
maxLyndon nodes
  = fst
  $ maximumBy (compare `on` weight)
  $ fromList
  [ (i, duval xs ++ duval ys)
  | i <- [ 0 .. length nodes - 1 ]
  , let (xs, ys) = splitAt i nodes
  ] where weight (_, xs) = length xs

