-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Operations pertaining to Rent's Rule
--
module LSC.Rent where

import Control.Lens
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet, size, singleton, isSubsetOf)
import qualified Data.IntSet as Set
import Data.Vector (Vector, (!), accum)
import qualified Data.Vector as Vector

import qualified Control.Foldl as L
import Control.Foldl.Statistics (lrrSlope, fastLinearReg)

import LSC.Component
import LSC.Model
import LSC.NetGraph
import LSC.Quadtree



type Edges = IntSet

type Block = (IntSet, IntSet)


rentExponent :: [Block] -> Double
rentExponent
  = lrrSlope
  . L.fold fastLinearReg
  . map (bimap (log . fromIntegral . size) (log . fromIntegral . size))
  . filter (not . Set.null . snd)



spatialRentExponent :: NetGraph -> Double
spatialRentExponent whole
  = rentExponent
  . tail
  . map (mergeBlock edges)
  . datapoints id
  . constructQuadtree (top ^. supercell . geometry . to coarseBoundingBox . to hypothenuse)
  . toList
  . imap (\ i xs -> (view gates top ! i ^. geometry, xs))
  $ vertices
  where
    (vertices, edges) = blockGraph top
    top = rebuildHyperedges $ over gates logicBlocks whole




blockGraph :: NetGraph -> (Vector Block, Vector Edges)
blockGraph top = (vertices, edges)

  where

    edges = views contacts (Set.fromList . HashMap.keys) <$> views nets (Vector.fromList . toList) top

    vertices
      = imap (\ i xs -> (singleton i, xs))
      . accum (<>) (mempty <$ top ^. gates)
      $ [ (k, singleton i)
        | (i, n) <- [0 ..] `zip` views nets toList top
        , k <- views contacts HashMap.keys n
        , k >= 0
        ]



proportion :: Foldable f => (Vector Block, Vector Edges) -> f Gate -> Block
proportion (vertices, edges) = mergeBlock edges . foldMap (getBlock vertices)
{-# INLINABLE proportion #-}


getBlock :: Vector Block -> Gate -> Block
getBlock = flip $ view . ix . view number


mergeBlock :: Vector Edges -> Block -> Block
mergeBlock _ (gs, ns)
  | size gs <= 1
  = (gs, ns)
mergeBlock e (gs, ns)
  = (gs, Set.filter (\ i -> not $ view (ix i) e `isSubsetOf` gs) ns)


