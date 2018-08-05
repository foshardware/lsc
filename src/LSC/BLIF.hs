
module LSC.BLIF where

import Control.Monad.Reader

import BLIF.Syntax
import LSC.Types


fromBLIF :: BLIF -> Gnostic Netlist
fromBLIF (BLIF models) = do
  technology <- ask
  pure $ Netlist

    [ g | Model _ _ _ _ commands <- models
    , (i, command) <- zip [1..] commands
    , g <- gates i command
    ]

    []


gates :: Int -> Command -> [Gate]
gates i (LibraryGate ident assignments)
  = [ Gate
        ident
        (fmap snd assignments)
        i ]
gates i (Subcircuit ident assignments)
  = [ Gate
        ident
        (fmap snd assignments)
        i ]
gates i _ = []
