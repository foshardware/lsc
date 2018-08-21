{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.Exlining where

import Data.Function
import Data.Hashable
import qualified Data.Set as Set
import Data.Vector hiding (replicate)
import Prelude hiding ((++), splitAt, length, concat, drop, take, null, zip, unzip)

import LSC.Types
import LSC.Duval
import LSC.SuffixTree


exline (Netlist _ _ _ nodes _)
  -- = (lz, length $ lz, length $ unique, length $ lyn, length <$> lyn)
  = (longestSubString tree)
  where
    tree@(SuffixTree _ _ lcp) = constructSuffixTree (hash . gateIdent) nodes
    lyn = recurseLyndon 16 (GateChar <$> nodes)
    unique = fromList $ Set.toList $ Set.fromList $ toList lyn



recurseLyndon :: Ord a => Int -> Vector a -> Vector (Vector a)
recurseLyndon k nodes
  | i <- maxLyndon nodes
  , (xs, ys) <- splitAt i nodes
  , i > 0
  , length nodes > k
  = recurseLyndon k xs ++ recurseLyndon k ys
recurseLyndon _ nodes = singleton nodes

maxLyndon :: Ord a => Vector a -> Int
maxLyndon nodes
  = fst
  $ maximumBy (compare `on` weight)
  $ fromList
  [ (i, duval ys ++ duval xs)
  | i <- [ 0 .. length nodes - 1 ]
  , let (xs, ys) = splitAt i nodes
  ] where weight (_, xs) = length xs

