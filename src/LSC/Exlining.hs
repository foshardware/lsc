{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.Exlining where

import Control.Monad
import Data.Generator
import Data.Hashable
import Data.Semigroup.Reducer
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Vector hiding (replicate)
import Prelude hiding ((++), splitAt, length, concat, drop, take)

import LSC.SuffixTree
import LSC.Types
import LSC.LZ78
import LSC.Duval


exline (Netlist name pins models nodes edges) = length <$> recurseLyndon 8 nodes


recurseLyndon :: Int -> Vector Gate -> Vector (Vector Gate)
recurseLyndon n nodes
  | i <- maxLyndon nodes
  , (xs, ys) <- splitAt i nodes
  , i > n
  = recurseLyndon n xs ++ duval ys
recurseLyndon _ nodes = singleton nodes

maxLyndon :: Vector Gate -> Int
maxLyndon nodes
  = fst
  $ maximumBy ( \ a b -> snd a `compare` snd b)
  $ fromList
  [ (i, weight $ duval ordering)
  | i <- [0..]
  | ordering <- tails $ GateChar <$> nodes
  ] where weight = length 

tails :: Vector a -> [Vector a]
tails v =
  [ drop i v
  | i <- [0 .. length v - 1]
  ]

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

