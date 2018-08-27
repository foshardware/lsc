
module LSC.BLIF where

import Control.Monad.Reader

import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import BLIF.Syntax
import LSC.Types hiding (ask)


fromBLIF :: BLIF -> Gnostic NetGraph
fromBLIF (BLIF     []) = pure mempty
fromBLIF (BLIF models) = do

  let (model, submodels) = splitAt 1 models

  technology <- ask

  let gates =
        [ gate
        | Model _ _ _ _ commands <- model
        , (i, command) <- zip [1..] commands
        , gate <- toGates i command
        ]

  let nets =
        [ (net, Net net (Map.singleton g [(contact, pin)]) 0)
        | g@(Gate ident assignments _ _) <- gates
        , (contact, net) <- Map.assocs assignments
        , com <- maybe [] pure $ Map.lookup ident $ components technology
        , pin <- maybe [] pure $ Map.lookup contact $ componentPins com
        ]

  let nodes = Vector.fromList gates
  let edges = Map.fromListWith mappend nets

  subGraphs <- sequence
        [ (,) name <$> fromBLIF (BLIF [submodel])
        | submodel@(Model name _ _ _ _) <- submodels
        ]

  pure $ NetGraph
    (head [name | Model name _ _ _ _ <- model])
    (head [(i, o, c) | Model _ i o c _ <- model])
    (Map.fromList subGraphs)
    nodes
    edges


toGates :: Int -> Command -> [Gate]
toGates i (LibraryGate ident assignments)
  = [ Gate
        ident
        (Map.fromList assignments)
        mempty
        i ]
toGates i (Subcircuit ident assignments)
  = [ Gate
        ident
        (Map.fromList assignments)
        mempty
        i ]
toGates _ _ = []



toBLIF :: NetGraph -> BLIF
toBLIF netlist = BLIF $ toModel netlist : [ toModel list | list <- toList $ subModels netlist ]


toModel :: NetGraph -> Model
toModel (NetGraph name (inputList, outputList, clockList) _ nodes _) = Model name

  inputList outputList clockList

  [ toSubcircuit node | node <- toList nodes ]


toSubcircuit :: Gate -> Command
toSubcircuit (Gate ident wires _ _) = Subcircuit ident (Map.assocs wires)


