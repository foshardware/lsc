{-# LANGUAGE TypeFamilies #-}

module LSC.Exlining where

import Data.Generator
import Data.Hashable
import Data.Semigroup.Reducer
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Vector
import Prelude hiding ((++), splitAt)

import LSC.SuffixTree
import LSC.Types
import LSC.LZ78


data Exlining a
  = ToLZ (LZ78 a)


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

