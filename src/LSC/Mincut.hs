{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.List (permutations, intersect)
import Data.Maybe
import Data.IntSet (singleton, size, elems)
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Semigroup
import Data.Matrix hiding (toList, (!))
import Data.Vector (Vector, fromListN, (!))
import qualified Data.Vector as V
import Prelude hiding (concat, lookup, take, read, unzip)

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

    estimationsMatrix =<< initialMatrix top
    m <- placeMatrix [N, W, S, E] =<< initialMatrix top

    estimationsMatrix m

    cells <- view stdCells <$> technology

    let geo g = g &~ do
            geometry .= toList (maybe (padCell g) (gate g) $ cells ^? ix (g ^. identifier) . dims)

        gate g (w, h) = region ^? ix (g ^. number)
            <&> \ (x, y) -> Layered x y (x + w) (y + h) [Metal2, Metal3] N

        padCell g = region ^? ix (g ^. number)
            <&> \ (x, y) -> Layered x y (x + fst std) (y + snd std) [Metal1] N

        region = IntMap.fromList
            [ (g ^. number, (fromIntegral j * (w + div w 2), fromIntegral i * (h + div h 2)))
            | i <- [1 .. nrows m]
            , j <- [1 .. ncols m]
            , let g = getElem i j m
            , let (w, h) = std
            , g ^. number >= 0
            ]

    pure $ top &~ do
        gates .= fmap geo (flattenGateMatrix m)




initialMatrix :: NetGraph -> LSC (Matrix Gate)
initialMatrix top = do

    let k = maximum $ top ^. gates <&> view number

    let padCells
          = imap (set number . (k +))
          $ fmap (set virtual True)
          $ fromListN (top ^. supercell . pins . to length)
          $ toList
          $ top ^. supercell . pins <&> \ p -> def &~ do
              identifier .= p ^. identifier
              wires .= Map.singleton (p ^. identifier) (p ^. identifier)

    let vector = top ^. gates <> padCells

    let (w, h) : _ = dropWhile (\ (x, y) -> x * y < length vector)
                   $ iterate (bimap (2*) (2*)) (1, 1)
        result = matrix w h $ \ (x, y) -> maybe def id $ vector ^? ix (pred x * w + pred y)

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
subHpwl m n = sum $ hpwlMatrix m <$> rebuildEdges (getMatrixAsVector n)



setBin :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
setBin i j n = ala Endo foldMap
    [ setElem (getElem x y n) (i + x - 1, j + y - 1)
    | x <- [1 .. nrows n]
    , y <- [1 .. ncols n]
    ]




matrixPermutations :: Matrix a -> [Matrix a]
matrixPermutations m
  = fromList (nrows m) (ncols m) <$> (permutations $ toList $ getMatrixAsVector m)



placeMatrix :: [Orientation] -> Matrix Gate -> LSC (Matrix Gate)
placeMatrix _ m | nrows m * ncols m <= 4 = pure m
placeMatrix o m = do

    let v = flattenGateMatrix m
        e = rebuildEdges $ set number `imap` v

    let h = nrows m `div` 2
        w = ncols m `div` 2

    it <- view iterations <$> environment

    (q1, q2, q3, q4) <- liftIO $ nonDeterministic $ do


        let ly = lockCardinalDirection (intersect o [N, S])
               $ V.filter (view virtual) (set number `imap` v)

        by <- st $ hypergraph (set number `imap` v) e

        Bisect q12 q34 <- improve it (flip compare `on` cutSize by) (bisect by ly) $ \ _ ->
            refit by (2*w*h) <$> fmMultiLevel by ly coarseningThreshold matchingRatio


        let v12 = fromListN (size q12) $ (v!) <$> elems q12
            v34 = fromListN (size q34) $ (v!) <$> elems q34
        let e12 = rebuildEdges $ set number `imap` v12
            e34 = rebuildEdges $ set number `imap` v34


        let l12 = lockCardinalDirection (intersect o [W, E])
                $ V.filter (view virtual) (set number `imap` v12)

        h12 <- st $ hypergraph (set number `imap` v12) e12

        Bisect q2 q1 <- improve it (flip compare `on` cutSize h12) (bisect h12 l12) $ \ _ ->
            refit h12 (w*h) <$> fmMultiLevel h12 l12 coarseningThreshold matchingRatio


        let l34 = lockCardinalDirection (intersect o [W, E])
                $ V.filter (view virtual) (set number `imap` v34)

        h34 <- st $ hypergraph (set number `imap` v34) e34

        Bisect q3 q4 <- improve it (flip compare `on` cutSize h34) (bisect h34 l34) $ \ _ ->
            refit h34 (w*h) <$> fmMultiLevel h34 l34 coarseningThreshold matchingRatio


        pure
          ( fromListN (size q1) ((v12!) <$> elems q1)
          , fromListN (size q2) ((v12!) <$> elems q2)
          , fromListN (size q3) ((v34!) <$> elems q3)
          , fromListN (size q4) ((v34!) <$> elems q4)
          )

    m1 <- placeMatrix
        (intersect o [N, E])
        $ matrix h w $ \ (x, y) -> maybe def id $ q1 ^? ix (pred x * w + pred y)
    m2 <- placeMatrix
        (intersect o [N, W])
        $ matrix h w $ \ (x, y) -> maybe def id $ q2 ^? ix (pred x * w + pred y)
    m3 <- placeMatrix
        (intersect o [S, W])
        $ matrix h w $ \ (x, y) -> maybe def id $ q3 ^? ix (pred x * w + pred y)
    m4 <- placeMatrix
        (intersect o [S, E])
        $ matrix h w $ \ (x, y) -> maybe def id $ q4 ^? ix (pred x * w + pred y)

    pure $ joinBlocks (m2, m1, m3, m4)

    where bisect = bipartitionEven




lockCardinalDirection :: [Orientation] -> Vector Gate -> Lock
-- lockCardinalDirection [N] gs = Lock (foldMap singleton $ view number <$> gs) mempty
-- lockCardinalDirection [W] gs = Lock (foldMap singleton $ view number <$> gs) mempty
-- lockCardinalDirection [S] gs = Lock mempty (foldMap singleton $ view number <$> gs)
-- lockCardinalDirection [E] gs = Lock mempty (foldMap singleton $ view number <$> gs)
lockCardinalDirection   _  _ = mempty



hypergraph :: Vector Gate -> Map Identifier Net -> ST s (V, E)
hypergraph v e = inputRoutine (length e) (length v)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList e
    , w ^. identifier /= "clk"
    , (c, _) <- w ^. contacts . to assocs
    ]




inline :: Int -> NetGraph -> NetGraph
inline n top
  | top ^. gates . to length < n
  , subs <- Map.filter predicate $ top ^. subcells
  , not $ null subs
  , sub <- maximumBy (compare `on` length . view pins . view supercell) subs
  = top &~ do
      gates  %= V.filter (\ x -> x ^. identifier /= sub ^. identifier)
      gates <>= sub ^. gates
    where predicate g = length (view gates g) + length (view gates top) <= succ n
inline _ top = top



