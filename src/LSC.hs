
module LSC where

import LSC.Arboresence
import LSC.Exlining
import LSC.Types
import LSC.NetGraph


stage1 :: Int -> NetGraph -> LSC Stage1
stage1 j netlist
  = fmap head
  $ concLSC
  $ take j
  $ fmap pnr
  $ getLeaves
  $ exline_ (repeat 20) netlist

