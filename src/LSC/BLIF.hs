{-# LANGUAGE ParallelListComp #-}

module LSC.BLIF
  ( module Language.BLIF.Syntax
  , module Language.BLIF.Parser
  , fromBLIF
  , toSubcircuit
  ) where

import Control.Monad.Reader

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Map
  ( assocs, lookup
  , singleton
  , fromList, fromListWith
  )
import Data.Maybe
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

import Language.BLIF.Parser (parseBLIF)
import Language.BLIF.Syntax

import LSC.Types


fromBLIF :: BLIF -> NetGraph
fromBLIF (BLIF []) = def
fromBLIF (BLIF (Model name inputs outputs clocks commands : submodels)) = do

  let nodes = Vector.fromList
        [ gate & number .~ i
        | i    <- [0.. ]
        | gate <- join $ toGates <$> commands
        ]

  let edges = fromListWith mappend
        [ (net, Net net mempty (singleton (gate ^. number) mempty))
        | gate <- toList nodes
        , (contact, net) <- assocs $ gate ^. wires
        ]

  subGraphs <- fromList <$> sequence
        [ (,) i <$> fromBLIF (BLIF [submodel])
        | submodel@(Model i _ _ _ _) <- submodels
        ]

  let ps =
        [ (i, Pin i In def)
        | i <- inputs ++ clocks
        ] ++
        [ (i, Pin i Out def)
        | i <- outputs
        ]

  let superCell = def & pins .~ fromList ps

  pure $ NetGraph
    name
    superCell
    subGraphs
    nodes
    edges


toGates :: Command -> [Gate]
toGates (LibraryGate ident assignments)
  = pure $ def
    & identifier .~ ident
    & wires .~ fromList assignments
toGates (Subcircuit ident assignments)
  = pure $ def
    & identifier .~ ident
    & wires .~ fromList assignments
toGates _ = mempty



toSubcircuit :: Gate -> Command
toSubcircuit gate = Subcircuit (gate ^. identifier) (assocs $ gate ^. wires)


