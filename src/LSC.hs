
module LSC where

import LSC.Arboresence
import LSC.Exlining
import LSC.Types
import LSC.NetGraph
import LSC.Placement


stage1 :: Int -> NetGraph -> LSC Stage1
stage1 _ netlist
  = placement
  $ netlist

