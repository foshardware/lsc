-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Operations pertaining to Rent's Rule
--
module LSC.Rent where

import Control.Lens
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (size, singleton, delete, union, isSubsetOf)
import qualified Data.IntSet as Set
import Data.Vector ((!), accum)
import qualified Data.Vector as Vector

import qualified Control.Foldl as F
import Control.Foldl.Statistics

import LSC.Component
import LSC.Model
import LSC.NetGraph
import LSC.Quadtree



rentExponent :: NetGraph -> Double
rentExponent top
  = lrrSlope
  . F.fold fastLinearReg
  . fmap (bimap logPlot logPlot)
  . filter (not . Set.null . snd)
  . fmap complete
  . datapoints id
  . constructQuadtree (top ^. supercell . geometry . to coarseBoundingBox . to hypothenuse)
  . toList
  . imap (\ i xs -> (view gates top ! i ^. space, (singleton i, delete i xs)))
  . accum union (mempty <$ top ^. gates)
  $ [ (k, singleton i)
    | (i, n) <- [0 ..] `zip` views nets toList top
    , k <- views contacts HashMap.keys n
    , k >= 0
    ]

  where

    logPlot = log . fromIntegral . (* 2) . size

    complete (gs, ns)
      | size gs < 2
      = (gs, ns)
    complete (gs, ns)
      = (gs, Set.filter (\ i -> not $ view (ix i) edges `isSubsetOf` gs) ns)

    edges = views contacts (Set.fromList . HashMap.keys) <$> views nets (Vector.fromList . toList) top

