-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Run estimates on models
--
module LSC.Estimate where

import Control.Lens
import Data.Text (unpack)
import Data.Matrix (Matrix, nrows, ncols)
import qualified Data.Vector as V
import Text.Printf

import LSC.Cartesian
import LSC.Component
import LSC.HigherOrder
import LSC.Model
import LSC.NetGraph
import LSC.Rent
import LSC.Transformer



estimations :: NetGraph -> LSC ()
estimations top = do
 
  let gs = top ^. gates
      ns = top ^. nets
 
  let box = coarseBoundingBox $ view space <$> gs

  let area = head (top ^. supercell . geometry)
      pivot = div (area ^. r + area ^. l) 2
  let (xs, ys) = V.partition ((<= pivot) . centerX) (view space <$> V.filter (views fixed not) gs)

  let segs = sum $ length . view netSegments <$> ns

  let k = views identifier unpack top

  info $
    [ k ++ " layout area: " ++ show (width box, height box)
    , k ++ " sum of hpwl: " ++ show (sum $ hpwl <$> ns)
    , k ++ " gate count: "  ++ show (length gs)
    ]
    ++ [ k ++ " net segments: " ++ show segs | segs > 0 ]
    ++ [ k ++ " row balance: " ++ show (sum $ width <$> xs) ++ " | " ++ show (sum $ width <$> ys) ]
    ++ [ k ++ " rent exponent: " ++ printf "%.6f" (rentExponent top) ]



estimationsMatrix :: Matrix Gate -> LSC ()
estimationsMatrix m = do

  info
    [ show $ view number <$> m
    , unwords [show $ nrows m, "x", show $ ncols m]
    , unwords ["gate count:", show $ foldl' (\ a g -> if g ^. number < 0 then a else succ a :: Int) 0 m]
    , unwords [" net count:", show $ length $ generateHyperedges m]
    , unwords ["sum of hpwl:", show $ sumOfHpwlMatrix m]
    ]

