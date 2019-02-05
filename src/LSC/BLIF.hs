{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module LSC.BLIF
  ( module Language.BLIF.Syntax
  , module Language.BLIF.Parser
  , fromBLIF
  , toSubcircuit
  ) where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Map
  ( assocs
  , singleton
  , fromList, fromListWith
  )
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

import Language.BLIF.Parser (parseBLIF)
import Language.BLIF.Syntax

import LSC.Types


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
        [ fromNetlist c & number .~ i
        | i <- [0.. ]
        | c <- commands
        ]

    edges = fromListWith mappend
        [ (net, Net net mempty (singleton (gate ^. number) [pin]))
        | gate <- toList nodes
        , (contact, net) <- gate ^. wires & assocs
        , let pin = def & identifier .~ contact
        ]

    superCell = def
      & pins <>~ fromList [(i, Pin i  In def) | i <- inputs ++ clocks] 
      & pins <>~ fromList [(i, Pin i Out def) | i <- outputs]


fromNetlist :: Command -> Gate
fromNetlist (LibraryGate ident assignments) = def
      & identifier .~ ident
      & wires .~ fromList assignments
fromNetlist (Subcircuit ident assignments) = def
      & identifier .~ ident
      & wires .~ fromList assignments
fromNetlist _ = def


toSubcircuit :: Gate -> Command
toSubcircuit gate = Subcircuit (gate ^. identifier) (gate ^. wires & assocs)

