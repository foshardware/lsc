
module LSC.BLIF where

import Control.Monad.Reader

import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as Vector

import BLIF.Syntax
import LSC.Types hiding (ask)


fromBLIF :: BLIF -> Gnostic Netlist
fromBLIF (BLIF models) = do

  technology <- ask

  let nodes =
        [ g | Model _ _ _ _ commands <- models
        , (i, command) <- zip [1..] commands
        , g <- gates i command
        ]

  let nets = Map.fromListWith (++)
        [ (net, [(gate, pin)])
        | gate@(Gate ident assignments _) <- nodes
        , (contact, net) <- assignments
        , com <- maybeToList $ Map.lookup ident $ components technology
        , pin <- maybeToList $ Map.lookup contact $ componentPins com
        ]

  let edges =
        [ Net pins i
        | (i, pins) <- [1..] `zip` Map.elems nets
        ]

  pure $ Netlist
    mempty
    (Vector.fromList nodes)
    (Vector.fromList edges)
    

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
