-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Run estimates on models
--
module LSC.Estimate where

import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.Ratio
import Data.Text (unpack)
import qualified Data.Vector as V
import Text.Printf

import LSC.Cartesian
import LSC.Component
import LSC.Model
import LSC.NetGraph
import LSC.Rent
import LSC.Transformer



estimations :: NetGraph -> LSC IO ()
estimations top = do
 
  let gs = top ^. gates
      ns = top ^. nets
 
  let box = coarseBoundingBox $ view geometry <$> gs

  let area = head (top ^. supercell . geometry)
      pivot = div (area ^. r + area ^. l) 2

  let balance = sum (width <$> xs) % sum (width <$> ys)
      (xs, ys) = V.partition ((<= pivot) . centerX) (view geometry <$> V.filter (views fixed not) gs)

  let segs = sum $ length . view netSegments <$> ns

  let title = "Model " ++ views identifier unpack top

  let so = round $ 100 * (1 - p') / (1 - fromMaybe 0 px) :: Int
      p' = spatialRentExponent top
      px = top ^. supercell . pstar


  warning
    [ "p* is exactly 1!" | px == Just 1 ]


  info
    $ title :
    [ printf "Layout area: (%d,%d)" (width box) (height box)
    , printf "Sum of hpwl: %d" $ sum $ hpwl <$> ns
    , printf "Gate count: %d" $ length gs
    ]
    ++ [ printf "Net segments: %d" segs | segs > 0 ]
    ++ [ printf "Row balance: %d %% %d" (numerator balance) (denominator balance) ]
    ++ [ printf "p* = %.2f | p' = %.6f | %d%% spatial optimality" p p' so | p <- toList px ]

