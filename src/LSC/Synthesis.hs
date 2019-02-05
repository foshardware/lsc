
{-# LANGUAGE FlexibleContexts #-}

module LSC.Synthesis where

import Control.Lens
import Data.Foldable
import Data.Map (lookup, assocs, singleton, fromListWith)
import Data.Maybe
import Prelude hiding (lookup)

import LSC.Types


contactGeometry :: NetGraph -> LSC NetGraph
contactGeometry netlist = do

  tech <- technology

  netlist
    & gates %~ fmap (vddGnd tech)
    & nets  .~ createNets tech
    & pure

  where

    vddGnd tech g
      | Just sc <- view identifier g `lookup` view stdCells tech
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc  
    vddGnd _ g
      | Just sc <- view identifier g `lookup` view subcells netlist <&> view supercell
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc
    vddGnd _ g = g

    createNets tech = fromListWith mappend
      [ (net, Net net mempty (singleton (gate ^. number) [pin]))
      | gate <- toList $ netlist ^. gates
      , (contact, net) <- assocs $ gate ^. wires
      , com <- maybeToList $ lookup (gate ^. identifier) (tech ^. stdCells)
      , pin <- maybeToList $ lookup contact (com ^. pins)
      ]



synthesizeLogic :: RTL -> LSC RTL
synthesizeLogic = undefined

synthesizeGeometry :: RTL -> LSC NetGraph
synthesizeGeometry = undefined
