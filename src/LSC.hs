
module LSC where

import LSC.Arboresence
import LSC.Types


stage1 :: Int -> NetGraph -> LSC NetGraph
stage1 n netlist
  = fmap last
  $ concLSC
  $ replicate (max 1 n)
  $ pnr netlist

