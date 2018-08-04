
module LSC.BLIF where

import BLIF.Syntax
import LSC.Types


fromBLIF :: BLIF -> Netlist
fromBLIF (BLIF models) = Netlist
  [ g | Model _ _ _ _ commands <- models
  , (i, command) <- zip [1..] commands
  , g <- gates i command
  ]
  []

gates :: Int -> Command -> [Gate]
gates i (LibraryGate _ assignments)
  = [ Gate
        (fmap snd assignments)
        i ]
gates i _ = []
