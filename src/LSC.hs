-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module LSC
  ( stage3, stage4
  , detailedPlacement, fastDP
  , computePstar
  , estimate
  , module LSC.Arrow
  ) where

import Control.Arrow
import Control.Category
import Control.Lens
import Data.Foldable
import Data.Functor.Contravariant
import Data.Maybe
import Data.Vector (Vector)
import Prelude hiding ((.), id)

import LSC.Arrow
import LSC.CellFlipping
import LSC.Estimate
import LSC.FastDP
import LSC.GlobalRouting
import LSC.Legalization
import LSC.Model
import LSC.Mincut
import LSC.NetGraph
import LSC.Rent
import LSC.Technology
import LSC.Transformer




stage3 :: Transpiler NetGraph
stage3 = id

    >>> induce gateGeometry
    >>> arr rebuildHyperedges

    >>> assignPstar
    >>> estimate

    >>> detailedPlacement
    >>> strategy1 significantHpwl detailedPlacement

    >>> finalEstimate



stage4 :: Transpiler NetGraph
stage4 = id

    >>> local determineRowSpacing
    >>> arr rebuildHyperedges

    >>> assignPstar
    >>> estimate

    >>> induce pinGeometry

    >>> local cellFlipping
    >>> estimate

    >>> induce pinGeometry

    >>> local determineNetSegments
    >>> finalEstimate

    -- >>> local determineRowSpacing
    -- >>> arr rebuildHyperedges >>> finalEstimate



-- globalPlacement :: Transpiler NetGraph
-- globalPlacement = local placeQuad



detailedPlacement :: Transpiler NetGraph
detailedPlacement = id

    >>> induce gateGeometry

    >>> arr assignCellsToRows
    >>> local juggleCells
    >>> local legalizeRows
    >>> arr rebuildHyperedges >>> estimate

    >>> fastDP >>> estimate

    >>> arr removeFeedthroughs
    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate

    >>> local legalizeRows
    >>> arr rebuildHyperedges >>> estimate

    >>> induce pinGeometry

    >>> local determineFeedthroughs
    >>> induce gateGeometry >>> estimate

    >>> local legalizeRows
    >>> arr rebuildHyperedges >>> estimate

    >>> strategy2 significantHpwl (local singleSegmentClustering >>> arr rebuildHyperedges)
    >>> estimate

    >>> local legalizeRows

    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate

    >>> arr removeFeedthroughs
    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate

    >>> induce pinGeometry

    >>> local determineFeedthroughs
    >>> induce gateGeometry >>> estimate

    >>> local legalizeRows
    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate




fastDP :: Transpiler NetGraph
fastDP = id
    -- >>> local singleSegmentClustering >>> arr rebuildHyperedges >>> estimate
    >>> strategy2 significantHpwl
        (id
        >>> estimate
        >>> local globalSwap
        >>> local legalizeRows >>> arr rebuildHyperedges
        >>> local verticalSwap
        >>> local legalizeRows >>> arr rebuildHyperedges
        >>> local localReordering
        >>> local legalizeRows >>> arr rebuildHyperedges
        >>> id)
    -- >>> strategy1 significantHpwl
    --     (local singleSegmentClustering >>> arr rebuildHyperedges >>> estimate)



assignPstar :: Transpiler NetGraph
assignPstar = proc top -> do
  enabled <- get environment -< view pstar
  if enabled && isNothing (top ^. supercell . pstar)
  then do
    inform -< "Compute optimal rent exponent"
    logicOnly <- arr $ rebuildHyperedges . over gates logicBlocks -< top
    p <- strategy3 4 defaultComparison computePstar -< logicOnly
    q <- arr $ coarsen 20 -< p
    arr $ uncurry $ over supercell . set pstar -< (Just q, top)
  else returnA -< top
  where coarsen r = (/ r) . fromIntegral @Int . round . (* r)


computePstar :: Compiler NetGraph Double
computePstar = proc top -> do
    blocks <- filter ((2 <=) . length) ^<< bisections <<^ view gates -< top
    singletons <- arr $ map pure . toList . view gates -< top
    f <- arr $ proportion . blockGraph -< top
    arr rentExponent -< f <$> blocks <> singletons


bisections :: Compiler (Vector Gate) [Vector Gate]
bisections = proc v -> do
  if length v <= 2
  then returnA -< []
  else do
    (p, q) <- snd ^<< strategy3 4 (fst >$< defaultComparison) (local fmBisection) -< v
    (r, s) <- bisections *** bisections -< (p, q)
    returnA -< p : q : r ++ s



estimate :: Transpiler NetGraph
estimate = proc top -> do
    remote estimations -< top
    returnA -< top


finalEstimate :: Transpiler NetGraph
finalEstimate = proc top -> do
    inform -< "Final estimate"
    estimate -< top

