
module LSC.Exlining where

import Data.Hashable
import Data.Vector
import Prelude hiding ((++), splitAt)

import LSC.SuffixTree
import LSC.Types


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
    nodesPass = xs ++ zs
