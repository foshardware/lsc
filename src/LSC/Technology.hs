-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE FlexibleContexts #-}

-- | Apply technology parameters
--
module LSC.Technology where

import Control.Lens

import Data.HashMap.Lazy

import LSC.Component
import LSC.Model
import LSC.Polygon
import LSC.Transformer



gateGeometry :: NetGraph -> LSC Identity NetGraph
gateGeometry top = do
    tech <- technology
    assume ("invalid scale factor: " ++ views scaleFactor show tech)
      $ tech ^. scaleFactor > 0
    pure $ rebuildGates (tech ^. stdCells) top


rebuildGates :: HashMap Identifier Cell -> NetGraph -> NetGraph
rebuildGates cells top
  = top &~ do
    gates %= fmap expand

  where

    fh = maximum $ cells <&> snd . view dims
    fw = maximum $ top ^. supercell . rows <&> view granularity

    expand g | g ^. feedthrough = g & geometry %~ \ x -> x & r .~ x^.l + fw & t .~ x^.b + fh
    expand g = g & geometry %~ maybe id drag (cells ^? views identifier ix g . dims)

    drag (w, h) p = p & r .~ view l p + w & t .~ view b p + h



pinGeometry :: NetGraph -> LSC Identity NetGraph
pinGeometry top = do
    tech <- technology
    assume ("invalid scale factor: " ++ views scaleFactor show tech)
      $ tech ^. scaleFactor > 0
    pure $ rebuildPins (tech ^. stdCells) top


rebuildPins :: HashMap Identifier Cell -> NetGraph -> NetGraph
rebuildPins cells top
  = top &~ do
    nets %= fmap (over contacts . imap $ fmap . maybe id align . views gates (^?) top . ix)

  where

    align g p
      | g ^. feedthrough
      = p & geometry .~ pure (g ^. geometry . to simplePolygon)
    align g p
      = maybe p (absolute g) $ cells ^? views identifier ix g . pins . views identifier ix p

    absolute g
      = over geometry . fmap
      $ inline (g ^. geometry)

