
module LSC.Exlining where

import Data.Text (unpack)
import Data.Hashable

import LSC.SuffixTree
import LSC.Types


hierarchical :: Netlist -> SuffixTree Gate
hierarchical (Netlist nodes _) = constructSuffixTree goedel nodes
  where goedel gate = hash $ gateIdent gate

