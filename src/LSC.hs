
module LSC where

import LSC.Placement
import LSC.Routing
import LSC.Types


stage1 :: Int -> NetGraph -> LSC NetGraph
stage1 n netlist
  = fmap last
  . concLSC
  . replicate n
  . routeSat
  -- =<< placeEasy
  =<< pure
      netlist
