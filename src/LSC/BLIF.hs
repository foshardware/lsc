{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module LSC.BLIF
  ( module Language.BLIF.Syntax
  , module Language.BLIF.Parser
  , module Language.BLIF.Builder
  , fromBLIF, toBLIF
  , toSubcircuit
  ) where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Map (assocs, elems, fromList)
import Data.Maybe
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

import Language.BLIF.Parser (parseBLIF)
import Language.BLIF.Syntax
import Language.BLIF.Builder

import LSC.NetGraph
import LSC.Types



toBLIF :: NetGraph -> BLIF
toBLIF = BLIF . fmap toModel . flatten subcells 


toModel :: NetGraph -> Model
toModel top = Model

  (top ^. identifier)

  [ p ^. identifier | p <- top ^. supercell . pins . to elems, p ^. dir == Just In  ]
  [ p ^. identifier | p <- top ^. supercell . pins . to elems, p ^. dir == Just Out ]
  []

  [ Subcircuit (g ^. identifier) (g ^. wires . to assocs) | g <- toList $ top ^. gates ]


fromBLIF :: BLIF -> NetGraph
fromBLIF (BLIF []) = def
fromBLIF (BLIF (top : down)) = fromModel top
  & subcells .~ fromList [ (x ^. identifier, x) | x <- fmap fromModel down ]
  & treeStructure


fromModel :: Model -> NetGraph
fromModel (Model name inputs outputs clocks commands)
  = NetGraph
    name
    superCell
    mempty
    nodes
    edges

  where  

    nodes = Vector.fromList
        [ c & number .~ i
        | i <- [0.. ]
        | c <- catMaybes $ fromNetlist <$> commands
        ]

    edges = rebuildEdges nodes

    superCell = def
      & pins <>~ fromList [(i, Pin i (Just  In) def) | i <- inputs ++ clocks] 
      & pins <>~ fromList [(i, Pin i (Just Out) def) | i <- outputs]


fromNetlist :: Command -> Maybe Gate
fromNetlist (LibraryGate ident assignments) = def
      & identifier .~ ident
      & wires .~ fromList assignments
      & Just
fromNetlist (Subcircuit ident assignments) = def
      & identifier .~ ident
      & wires .~ fromList assignments
      & Just
fromNetlist _ = Nothing


toSubcircuit :: Gate -> Command
toSubcircuit gate = Subcircuit (gate ^. identifier) (gate ^. wires & assocs)

