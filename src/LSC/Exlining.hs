
module LSC.Exlining where

import Data.Hashable

import LSC.SuffixTree
import LSC.Types


hierarchical :: Netlist -> [Gate]
hierarchical (Netlist nodes _) = longestSubstring $ constructSuffixTree goedel nodes
  where goedel gate = hash $ gateIdent gate

