
module LSC.BLIF where

import BLIF.Syntax
import LSC.Types


fromBLIF :: BLIF -> Netlist
fromBLIF (BLIF models) = Netlist
  [ result
  | Model _ _ _ _ commands <- models
  , (i, command) <- zip [1..] commands
  , result <- gate i command
  ]
  []

gate :: Int -> Command -> [Gate]
gate i (LibraryGate_Command (LibraryGate _ assignments))
  = [ Gate
        (fmap snd assignments)
        i ]
gate i (LogicGate_Command (LogicGate wires _))
  = [ Gate
        wires
        i ]
gate i _ = []
