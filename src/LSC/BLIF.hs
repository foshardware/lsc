
module LSC.BLIF where

import Control.Monad.Reader

import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import BLIF.Syntax
import LSC.Types hiding (ask)


fromBLIF :: BLIF -> Gnostic Netlist
fromBLIF (BLIF     []) = pure mempty
fromBLIF (BLIF models) = do

  let (model, submodels) = splitAt 1 models

  technology <- ask

  let nodes =
        [ gate
        | Model _ _ _ _ commands <- model
        , (i, command) <- zip [1..] commands
        , gate <- gates i command
        ]

  let nets = Map.fromListWith mappend
        [ (net, [Contact gate contact pin])
        | gate@(Gate ident assignments _ _) <- nodes
        , (contact, net) <- Map.assocs assignments
        , com <- maybe [] pure $ Map.lookup ident $ components technology
        , pin <- maybe [] pure $ Map.lookup contact $ componentPins com
        ]

  let edges =
        [ Net pins i
        | (i, pins) <- [1..] `zip` Map.elems nets
        ]

  subGraphs <- sequence
        [ (,) name <$> fromBLIF (BLIF [submodel])
        | submodel@(Model name _ _ _ _) <- submodels
        ]

  pure $ Netlist
    (head [name | Model name _ _ _ _ <- model])
    (head [(i, o, c) | Model _ i o c _ <- model])
    (Map.fromList subGraphs)
    (Vector.fromList nodes)
    (Vector.fromList edges)


gates :: Int -> Command -> [Gate]
gates i (LibraryGate ident assignments)
  = [ Gate
        ident
        (Map.fromList assignments)
        mempty
        i ]
gates i (Subcircuit ident assignments)
  = [ Gate
        ident
        (Map.fromList assignments)
        mempty
        i ]
gates _ _ = []



toBLIF :: Netlist -> BLIF
toBLIF netlist = BLIF $ toModel netlist : [ toModel list | list <- toList $ subModels netlist ]


toModel :: Netlist -> Model
toModel (Netlist name (inputList, outputList, clockList) _ nodes _) = Model name

  inputList outputList clockList

  [ toSubcircuit node | node <- toList nodes ]


toSubcircuit :: Gate -> Command
toSubcircuit (Gate ident wires _ _) = Subcircuit ident (Map.assocs wires)


