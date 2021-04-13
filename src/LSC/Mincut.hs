-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module LSC.Mincut where

import Control.Lens
import Data.Default
import Data.Foldable hiding (concat)
import Data.Function
import Data.Matrix hiding (toList, (!))
import Data.Maybe
import Data.IntSet (size, elems)
import Data.Vector (Vector, fromListN, backpermute)
import Prelude hiding (concat, lookup, read, unzip)

import LSC.FM as FM
import LSC.Model
import LSC.NetGraph
import LSC.Transformer





fmBisection :: Vector Gate -> LSC IO (Int, (Vector Gate, Vector Gate))
fmBisection v
  = do

    top <- determinate
      $ hypergraph
      $ rebuildHyperedges
      $ def &~ do
        gates .= set number `imap` v

    Bisect p q <- entropicIndeterminate
      $ evalFM
      $ fmMultiLevel top coarseningThreshold matchingRatio

    let v1 = v `backpermute` fromListN (size p) (elems p)
        v2 = v `backpermute` fromListN (size q) (elems q)

    pure (cutSize top (Bisect p q), (v1, v2))



quadrisection :: Matrix a -> (Matrix a, Matrix a, Matrix a, Matrix a)
quadrisection m
  | nrows m <= 1 || ncols m <= 1
  = error
  $ "quadrisection: cannot quadrisect matrix with dimensions: " ++ show (nrows m, ncols m)
quadrisection m
  = splitBlocks (nrows m `div` 2) (ncols m `div` 2) m




fmQuadrisection :: Matrix Gate -> LSC IO (Int, (Matrix Gate, Matrix Gate, Matrix Gate, Matrix Gate))
fmQuadrisection m
  | nrows m * ncols m <= 4
  = pure (0, quadrisection m)
fmQuadrisection m
  = do

    let v = flattenGateMatrix m
        e = generateHyperedges $ set number `imap` v

    let h = nrows m `div` 2
        w = ncols m `div` 2

    assume "fmQuadrisection: sector is not quadratic"
      $ w == h


    top <- determinate
      $ hypergraph
      $ def &~ do
        nets .= e
        gates .= set number `imap` v

    bisection2134 <- entropicIndeterminate
      $ evalFM
      $ rebalance =<< fmMultiLevel top coarseningThreshold matchingRatio

    Bisect q21 q34 <- determinate $ refit top (2*w*h) bisection2134

    -- let v21 = fromListN (size q21) $ (v!) <$> elems q21
    --     v34 = fromListN (size q34) $ (v!) <$> elems q34

    let v21 = v `backpermute` fromListN (size q21) (elems q21)
        v34 = v `backpermute` fromListN (size q34) (elems q34)

    let e21 = generateHyperedges $ set number `imap` v21
        e34 = generateHyperedges $ set number `imap` v34


    h21 <- determinate
      $ hypergraph
      $ def &~ do
        nets .= e21
        gates .= set number `imap` v21

    bisection21 <- entropicIndeterminate
      $ evalFM
      $ rebalance =<< fmMultiLevel h21 coarseningThreshold matchingRatio

    Bisect q2 q1 <- determinate $ refit h21 (w*h) bisection21


    h34 <- determinate
      $ hypergraph
      $ def &~ do
        nets .= e34
        gates .= set number `imap` v34

    bisection34 <- entropicIndeterminate
      $ evalFM
      $ rebalance =<< fmMultiLevel h34 coarseningThreshold matchingRatio

    Bisect q3 q4 <- determinate $ refit h34 (w*h) bisection34


    let c1 = cutSize top $ Bisect q21 q34
        c2 = cutSize h21 $ Bisect q2 q1
        c3 = cutSize h34 $ Bisect q3 q4

    -- let v1 = fromListN (size q1) $ (v21!) <$> elems q1
    --     v2 = fromListN (size q2) $ (v21!) <$> elems q2
    --     v3 = fromListN (size q3) $ (v34!) <$> elems q3
    --     v4 = fromListN (size q4) $ (v34!) <$> elems q4

    let v1 = v21 `backpermute` fromListN (size q1) (elems q1)
        v2 = v21 `backpermute` fromListN (size q2) (elems q2)
        v3 = v34 `backpermute` fromListN (size q3) (elems q3)
        v4 = v34 `backpermute` fromListN (size q4) (elems q4)

    let m1 = fromList h w $ toList v1 ++ repeat def 
        m2 = fromList h w $ toList v2 ++ repeat def 
        m3 = fromList h w $ toList v3 ++ repeat def 
        m4 = fromList h w $ toList v4 ++ repeat def 


    assume "fmQuadrisection: quadrant overflow"
      $ all (<= w * h) [length v1, length v2, length v3, length v4]

    pure (c1 + c2 + c3, (m1, m2, m3, m4))




initialMatrix :: NetGraph -> Matrix Gate
initialMatrix top
  = matrix w h $ \ case
      (x, _) | pred x < wd -> def
      (_, y) | pred y < hd -> def
      (x, _) | w - x < wd -> def
      (_, y) | h - y < hd -> def
      (x, y) -> fromMaybe def $ v ^? ix (f x y)

  where

    v = top ^. gates

    w = until (\ x -> length v <= x * x) (2*) 1
    h = w

    d = ceiling $ sqrt (fromIntegral $ length v :: Float)

    wd = div (w - d) 2
    hd = div (h - d) 2

    f x y = (pred x - wd) * w + (pred y - hd) - max 0 (pred x - wd) * 2 * wd



placeMatrix :: Matrix Gate -> LSC IO (Matrix Gate)
placeMatrix m
  | nrows m <= 1 || ncols m <= 1
  = pure m
placeMatrix m
  = do
    (m1, m2, m3, m4) <- snd <$> fmQuadrisection m
    q1 <- placeMatrix m1
    q2 <- placeMatrix m2
    q3 <- placeMatrix m3
    q4 <- placeMatrix m4
    pure $ joinBlocks (q1, q2, q3, q4)


