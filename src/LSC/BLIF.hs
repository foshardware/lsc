{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module LSC.BLIF
  ( module Language.BLIF.Syntax
  , module Language.BLIF.Parser
  , module Language.BLIF.Builder
  , fromBLIF, toBLIF
  , toSubcircuit
  ) where

import Control.Lens hiding (imap)
import Data.Default
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe hiding (mapMaybe)
import Data.Vector (mapMaybe, imap)
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

  [ p ^. identifier | p <- top ^. supercell . pins & HashMap.elems, p ^. dir == Just In  ]
  [ p ^. identifier | p <- top ^. supercell . pins & HashMap.elems, p ^. dir == Just Out ]
  []

  $ fmap (\ g -> Subcircuit (g ^. identifier) (g ^. wires & HashMap.toList)) (top ^. gates)


fromBLIF :: BLIF -> NetGraph
fromBLIF (BLIF []) = def
fromBLIF (BLIF (top : down)) = fromModel top
  & subcells .~ HashMap.fromList [ (x ^. identifier, x) | x <- fmap fromModel down ]
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

    nodes = set number `imap` mapMaybe fromNetlist commands

    edges = generateEdges nodes

    superCell = def
      & pins <>~ HashMap.fromList [(i, Pin i (Just  In) def) | i <- inputs ++ clocks] 
      & pins <>~ HashMap.fromList [(i, Pin i (Just Out) def) | i <- outputs]


fromNetlist :: Command -> Maybe Gate
fromNetlist (LibraryGate ident assignments) = def
      & identifier .~ ident
      & wires .~ HashMap.fromList assignments
      & Just
fromNetlist (Subcircuit ident assignments) = def
      & identifier .~ ident
      & wires .~ HashMap.fromList assignments
      & Just
fromNetlist _ = Nothing


toSubcircuit :: Gate -> Command
toSubcircuit gate = Subcircuit (gate ^. identifier) (gate ^. wires & HashMap.toList)

