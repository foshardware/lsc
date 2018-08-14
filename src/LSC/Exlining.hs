
module LSC.Exlining where

import Data.Hashable
import Data.Vector

import LSC.SuffixTree
import LSC.Types


hierarchical :: Netlist -> Vector Gate
hierarchical (Netlist nodes _) = longestSubstring $ constructSuffixTree (hash . gateIdent) nodes

