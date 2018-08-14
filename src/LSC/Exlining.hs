
module LSC.Exlining where

import Data.Hashable

import LSC.SuffixTree
import LSC.Types


hierarchical :: Netlist -> Netlist
hierarchical (Netlist nodes edges) = Netlist

  nodesPass

  edges

  where

    suffixTree = constructSuffixTree (hash . gateIdent) nodes

    (ix, len, prefix) = longestSubstring suffixTree

    (xs, ys) = splitAt ix nodes
    ( _, zs) = splitAt len ys
    nodesPass = xs ++ zs
