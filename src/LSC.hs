
module LSC where

import LSC.Arboresence
import LSC.Exlining
import LSC.Types
import LSC.NetGraph
import LSC.Placement
import LSC.Routing


stage1 :: Int -> NetGraph -> LSC NetGraph
stage1 _ netlist
  =   pnr
      netlist

