
module LSC.BLIF where

import BLIF.Syntax
import LSC.Types


fromBLIF :: BLIF -> Netlist
fromBLIF (BLIF models) = Netlist [] []
