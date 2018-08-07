
module LSC.BLIF where

import Control.Monad.Reader

import Data.Either
import qualified Data.Map as Map
import Data.Maybe

import BLIF.Syntax
import LSC.Types


fromBLIF :: BLIF -> Gnostic Netlist
fromBLIF (BLIF models) = do

  technology <- ask

  let nodes =
        [ g | Model _ _ _ _ commands <- models
        , (i, command) <- zip [1..] commands
        , g <- gates i command
        ]

  let pins = 
        [ if pinDir pin == In
            then Right (gate, wire, pin)
            else Left (gate, wire, pin)
        | gate@(Gate ident assignments _) <- nodes
        , (contact, wire) <- assignments
        , com <- maybeToList $ Map.lookup ident $ components technology
        , pin <- maybeToList $ Map.lookup contact $ componentPins com
        ]

  let edges =
        [ Wire (sourceGate, pinOut) (targetGate, pinIn) 0
        | (sourceGate, input, pinOut) <- lefts pins
        , (targetGate, output, pinIn) <- rights pins
        , input == output
        ]


  pure $ Netlist nodes edges
    


gates :: Int -> Command -> [Gate]
gates i (LibraryGate ident assignments)
  = [ Gate
        ident
        assignments
        i ]
gates i (Subcircuit ident assignments)
  = [ Gate
        ident
        assignments
        i ]
gates _ _ = []
