{-# LANGUAGE ParallelListComp #-}

module LSC.BLIF where

import Control.Monad.Reader

import Data.Default
import Data.Foldable
import Data.Map
  ( assocs, lookup
  , union, unions
  , singleton
  , fromList, fromListWith, fromAscList
  )
import Data.Maybe
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

import BLIF.Syntax
import LSC.Types hiding (ask)


fromBLIF :: BLIF -> Gnostic NetGraph
fromBLIF (BLIF []) = pure def
fromBLIF (BLIF (Model name inputs outputs clocks commands : submodels)) = do

  technology <- ask

  let gates = join $ toGates 0 <$> commands

  let nodes = Vector.fromList
        [ gate { gateIndex = i }
        | (i, gate) <- zip [0.. ] gates
        ]

  let nets = fromListWith mappend
        [ (net, Net net mempty (singleton gate [pin]))
        | gate@(Gate ident _ assignments _) <- toList nodes
        , (contact, net) <- assocs assignments
        , com <- maybeToList $ lookup ident $ components technology
        , pin <- maybeToList $ lookup contact $ componentPins com
        ]

  let edges = nets

  subGraphs <- fromList <$> sequence
        [ (,) name <$> fromBLIF (BLIF [submodel])
        | submodel@(Model name _ _ _ _) <- submodels
        ]

  pure $ NetGraph
    name
    (AbstractGate mempty $ [ Pin i In def | i <- inputs ++ clocks] ++ [ Pin i Out def | i <- outputs])
    subGraphs
    nodes
    edges


toGates :: Int -> Command -> [Gate]
toGates i (LibraryGate ident assignments)
  = [ Gate
        ident
        mempty
        (fromList assignments)
        i ]
toGates i (Subcircuit ident assignments)
  = [ Gate
        ident
        mempty
        (fromList assignments)
        i ]
toGates _ _ = []



toSubcircuit :: Gate -> Command
toSubcircuit (Gate ident _ wires _) = Subcircuit ident (assocs wires)


