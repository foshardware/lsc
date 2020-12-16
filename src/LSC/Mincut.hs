-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module LSC.Mincut where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.ST
import Control.Monad.State (execStateT, get, put)
import Data.Default
import Data.Foldable hiding (concat)
import Data.Function
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (permutations)
import Data.Maybe
import Data.IntSet (size, elems)
import Data.Semigroup
import Data.Matrix hiding (toList, (!))
import Data.Vector (Vector, fromListN, (!))
import Data.Vector (unsafeFreeze)
import Data.Vector.Mutable (new, write)
import qualified Data.Vector as V
import Prelude hiding (concat, lookup, read, unzip)

import LSC.Entropy
import LSC.Improve
import LSC.FM as FM
import LSC.NetGraph
import LSC.Types hiding (thaw)





slidingWindow :: Int -> Matrix Gate -> LSC (Matrix Gate)
slidingWindow k m = flip execStateT m $ sequence
  [ do
    s <- get
    let middle = submatrix i (i+k-1) 1 (ncols m) s
        upper = ala Endo foldMap
              [ (submatrix 1 (pred i) 1 (ncols m) s <->) | i > 1 ]
        lower = ala Endo foldMap
              [ (<-> submatrix (i+k) (nrows m) 1 (ncols m) s) | i+k <= nrows m ]
    result <- lift $ upper . lower <$> placeWindow middle
    put result
  | i <- [1 .. nrows m - k]
  ]





placeWindow :: Matrix Gate -> LSC (Matrix Gate)
placeWindow m | ncols m <= 2 = pure m
placeWindow _ = undefined




placeQuad :: NetGraph -> LSC NetGraph
placeQuad top = do

    let std = (20000, 20000)

    m <- placeMatrix =<< initialMatrix top
    estimationsMatrix m

    cells <- view stdCells <$> technology

    let geo g = g & space .~ foldMap id (maybe (padCell g) (gate g) $ cells ^? ix (g ^. identifier) . dims)

        gate g (w, h) = region ^? ix (g ^. number)
            <&> \ (x, y) -> Component x y (x + w) (y + h) [Metal2, Metal3] N

        padCell g = region ^? ix (g ^. number)
            <&> \ (x, y) -> Component x y (x + fst std) (y + snd std) [Metal1] N

        region = runST $ do
            u <- new $ succ $ maximum $ view number <$> m
            sequence_
              [ write u g (fromIntegral j * (w + div w 2), fromIntegral i * (h + div h 2))
              | i <- [1 .. nrows m]
              , j <- [1 .. ncols m]
              , let g = getElem i j m ^. number
              , let (w, h) = std
              , g >= 0
              ]
            unsafeFreeze u

    pure $ top &~ do
        gates .= fmap geo (flattenGateMatrix m)




initialMatrix :: NetGraph -> LSC (Matrix Gate)
initialMatrix top = do

    let v = top ^. gates

    let (w, h) = head
               $ dropWhile (\ (x, y) -> x * y < length v)
               $ iterate (bimap (2*) (2*)) (1, 1)

    let d = ceiling $ sqrt (fromIntegral $ length v :: Float)

    let wd = div (w - d) 2
        hd = div (h - d) 2

    let f x y = (pred x - wd) * w + (pred y - hd) - (max 0 $ pred x - wd) * 2 * wd

    let result = matrix w h $ \ pos -> case pos of
                 (x, _) | pred x < wd -> def
                 (_, y) | pred y < hd -> def
                 (x, _) | w - x < wd -> def
                 (_, y) | h - y < hd -> def
                 (x, y) -> maybe def id $ v ^? ix (f x y)

    pure result




rotateBins :: Matrix Gate -> Matrix Gate
rotateBins m | nrows m * ncols m <= 4 = m
rotateBins m = ala Endo foldMap
    [ setBin i j
    $ minimumBy (compare `on` subHpwl m)
    $ matrixPermutations
    $ submatrix i (succ i) j (succ j) m
    | i <- [1 .. nrows m - 2]
    , j <- [1 .. ncols m - 2]
    , odd i, odd j
    ] m



subHpwl :: Matrix Gate -> Matrix Gate -> Int
subHpwl m n = sum $ hpwlMatrix (coordsVector m) <$> generateEdges n



setBin :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
setBin i j n = ala Endo foldMap
    [ setElem (getElem x y n) (i + x - 1, j + y - 1)
    | x <- [1 .. nrows n]
    , y <- [1 .. ncols n]
    ]



rim :: Int -> Int -> [(Int, Int)]
rim k w = base <> (swap <$> base)
  where
    swap ~(i, j) = (j, i)
    base = [(i, j) | i <- [k, w - k + 1], j <- [k .. w - k + 1]]



swapElem :: (Int, Int) -> (Int, Int) -> Matrix a -> Matrix a
swapElem (fi, fj) (ti, tj) m
    = setElem (getElem fi fj m) (ti, tj)
    $ setElem (getElem ti tj m) (fi, fj)
    $ m


permutations4 :: (a, a, a, a) -> [(a, a, a, a)]
permutations4 (e1, e2, e3, e4) =
    [ (f1, f2, f3, f4)
    | [f1, f2, f3, f4] <- permutations [e1, e2, e3, e4]
    ]



matrixPermutations :: Matrix a -> [Matrix a]
matrixPermutations m
  = fromList (nrows m) (ncols m) <$> (permutations $ toList $ getMatrixAsVector m)



placeMatrix :: Matrix Gate -> LSC (Matrix Gate)
placeMatrix m
    | nrows m * ncols m <= 4
    = pure $ minimumBy (compare `on` sumOfHpwlMatrix) (matrixPermutations m)
placeMatrix m = do

    let v = flattenGateMatrix m
        e = generateEdges $ set number `imap` v

    let h = nrows m `div` 2
        w = ncols m `div` 2

    it <- view iterations <$> environment

    (q1, q2, q3, q4) <- liftIO $ nonDeterministic $ runFMWithGen $ do

        by <- st $ hypergraph (set number `imap` v) e
        Bisect q21 q34 <- improve it ((-) `on` cutSize by) (bisect by mempty) $ \ _ ->
            refit by (2*w*h) mempty <$> fmMultiLevel by mempty coarseningThreshold matchingRatio


        let v21 = fromListN (size q21) $ (v!) <$> elems q21
            v34 = fromListN (size q34) $ (v!) <$> elems q34
        let e21 = generateEdges $ set number `imap` v21
            e34 = generateEdges $ set number `imap` v34


        h21 <- st $ hypergraph (set number `imap` v21) e21
        Bisect q2 q1 <- improve it ((-) `on` cutSize h21) (bisect h21 mempty) $ \ _ ->
            refit h21 (w*h) mempty <$> fmMultiLevel h21 mempty coarseningThreshold matchingRatio

        h34 <- st $ hypergraph (set number `imap` v34) e34
        Bisect q3 q4 <- improve it ((-) `on` cutSize h34) (bisect h34 mempty) $ \ _ ->
            refit h34 (w*h) mempty <$> fmMultiLevel h34 mempty coarseningThreshold matchingRatio


        pure
          ( fromListN (size q1) ((v21!) <$> elems q1)
          , fromListN (size q2) ((v21!) <$> elems q2)
          , fromListN (size q3) ((v34!) <$> elems q3)
          , fromListN (size q4) ((v34!) <$> elems q4)
          )


    m1 <- placeMatrix
        $ matrix h w $ \ (x, y) -> maybe def id $ q1 ^? ix (pred x * w + pred y)
    m2 <- placeMatrix
        $ matrix h w $ \ (x, y) -> maybe def id $ q2 ^? ix (pred x * w + pred y)
    m3 <- placeMatrix
        $ matrix h w $ \ (x, y) -> maybe def id $ q3 ^? ix (pred x * w + pred y)
    m4 <- placeMatrix
        $ matrix h w $ \ (x, y) -> maybe def id $ q4 ^? ix (pred x * w + pred y)

    pure
      $ minimumBy (compare `on` sumOfHpwlMatrix)
      $ fmap joinBlocks
      $ permutations4 (m1, m2, m3, m4)

    where bisect = bipartitionEven




hypergraph :: Vector Gate -> HashMap Identifier Net -> ST s (V, E)
hypergraph v e = inputRoutine (length e) (length v)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList e
    , w ^. identifier /= "clk"
    , c <- w ^. contacts & HashMap.keys
    ]




inline :: Int -> NetGraph -> NetGraph
inline n top
  | top ^. gates . to length < n
  , subs <- HashMap.filter predicate $ top ^. subcells
  , not $ null subs
  , sub <- maximumBy (compare `on` length . view pins . view supercell) subs
  = top &~ do
      gates  %= V.filter (\ x -> x ^. identifier /= sub ^. identifier)
      gates <>= sub ^. gates
    where predicate g = length (view gates g) + length (view gates top) <= succ n
inline _ top = top



