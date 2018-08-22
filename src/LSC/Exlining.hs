{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.Exlining where

import Control.Monad
import Data.Foldable
import Data.Function
import qualified Data.List as List
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector hiding (replicate, foldl', null, toList, length)

import LSC.Types
import LSC.SuffixTree


exline k (Netlist name pins subModels nodes edges)
  | not $ null maxr
  = Netlist name pins subModels

  (foldl' undefined nodes pos)

  edges

  where

    ((len, pos, _) : _) = maxr
    maxr = maximalRepeatsDisjoint (hash . gateIdent) nodes k

    outerScope =
      [ g | (p, q) <- List.zip (fmap (+ len) pos `mappend` pure 0) (length nodes : pos)
      , g <- toList $ slice p (q - p) nodes
      ]
    innerScope = [ g | p <- pos, g <- toList $ slice p len nodes ]

    outerScopeWires = scopeWires outerScope
    innerScopeWires = scopeWires innerScope

exline _ netlist = netlist


scopeWires :: [Gate] -> Set Wire
scopeWires nodes = Set.fromList [ snd wire | node <- nodes, wire <- gateWires node ]


