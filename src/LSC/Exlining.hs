{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.Exlining where

import Control.Monad
import Data.Generator
import Data.Hashable
import Data.Semigroup.Reducer
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Vector hiding (replicate)
import Prelude hiding ((++), splitAt, length, concat, drop, take)

import LSC.SuffixTree
import LSC.Types
import LSC.LZ78
import LSC.Duval


exline (Netlist name pins models nodes edges) = length <$> recurseLyndon 4 (GateChar <$> nodes)


recurseLyndon :: Ord a => Int -> Vector a -> Vector (Vector a)
recurseLyndon n nodes
  | i <- maxLyndon nodes
  , (xs, ys) <- splitAt i nodes
  , i > n
  = recurseLyndon n xs ++ recurseLyndon n ys
recurseLyndon _ nodes = singleton nodes

maxLyndon :: Ord a => Vector a -> Int
maxLyndon nodes
  = fst
  $ maximumBy ( \ a b -> weight a `compare` weight b)
  $ fromList
  [ (i, duval xs ++ duval ys)
  | i <- [ 1 .. length nodes - 1 ]
  , let (xs, ys) = splitAt i nodes
  ] where weight (_, xs) = length xs


hierarchical :: Netlist -> Netlist
hierarchical (Netlist name pins models nodes edges) = Netlist name

  pins

  models

  nodesPass

  edges

  where

    suffixTree = constructSuffixTree (hash . gateIdent) nodes

    (ix, len, prefix) = longestSubstring suffixTree

    (xs, ys) = splitAt ix nodes
    ( _, zs) = splitAt len ys
    nodesPass = xs ++ singleton component ++ zs

    component = Gate mempty mempty 0

