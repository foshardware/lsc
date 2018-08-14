
module LSC.Exlining where

import Data.Hashable
import Data.Vector
import Prelude hiding ((++), splitAt)

import LSC.SuffixTree
import LSC.Types


hierarchical :: Netlist -> Netlist
hierarchical (Netlist models nodes edges) = Netlist

  models

  nodesPass

  edges

  where

    suffixTree = constructSuffixTree (hash . gateIdent) nodes

    (ix, len, prefix) = longestSubstring suffixTree

    (xs, ys) = splitAt ix nodes
    ( _, zs) = splitAt len ys
    nodesPass = xs ++ zs
