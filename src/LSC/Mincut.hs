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
import Data.Maybe
import Data.IntSet (size, elems, fromAscList)
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Semigroup
import Data.Matrix hiding (toList, (!))
import Data.Vector (Vector, filter, fromListN, (!))
import Prelude hiding (filter, concat, lookup, take, read, unzip)

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

    m <- placeMatrix =<< initialMatrix top

    estimationsMatrix m

    cells <- view stdCells <$> technology

    let geo g = g & geometry .~ toList (pos g =<< cells ^? ix (g ^. identifier) . dims)
        pos g (w, h) = region ^? ix (g ^. number) <&> \(x,y) -> Layered x y (x+w) (y+h) [Metal2, Metal3] N
        region = IntMap.fromList
          [ (g ^. number, (fromIntegral j * (w + div w 2), fromIntegral i * (h + div h 2)))
          | i <- [1 .. nrows m]
          , j <- [1 .. ncols m]
          , let g = getElem i j m
          , let (w, h) = std
          , g ^. number >= 0
          ]

    pure $ top & gates %~ fmap geo




initialMatrix :: NetGraph -> LSC (Matrix Gate)
initialMatrix top = do

    let (w, h) = head $ dropWhile (\ (x, y) -> x * y < top ^. gates . to length) boards
        result = matrix w h $ \ (x, y) -> maybe def id $ top ^. gates ^? ix (pred x * w + pred y)

    pure result

    where boards = iterate (bimap (2*) (2*)) (1, 1)




placeMatrix :: Matrix Gate -> LSC (Matrix Gate)
placeMatrix m | nrows m * ncols m <= 4 = pure m
placeMatrix m = do

    let v = flattenGateMatrix m
        e = rebuildEdges $ set number `imap` v

    let h = nrows m `div` 2
        w = ncols m `div` 2

    it <- view iterations <$> environment

    (q1, q2, q3, q4) <- liftIO $ nonDeterministic $ do

        hy <- st $ hypergraph (set number `imap` v) e
        Bisect q12 q34 <- improve it (flip compare `on` cutSize hy) (bisect hy) $ \ _ -> do
            refit hy (2*w*h) <$> fmMultiLevel hy coarseningThreshold matchingRatio

        let v12 = fromListN (size q12) $ (v!) <$> elems q12
            v34 = fromListN (size q34) $ (v!) <$> elems q34
        let e12 = rebuildEdges $ set number `imap` v12
            e34 = rebuildEdges $ set number `imap` v34

        h12 <- st $ hypergraph (set number `imap` v12) e12
        Bisect q1 q2 <- improve it (flip compare `on` cutSize h12) (bisect h12) $ \ _ -> do
            refit hy (w*h) <$> fmMultiLevel h12 coarseningThreshold matchingRatio

        h34 <- st $ hypergraph (set number `imap` v34) e34
        Bisect q3 q4 <- improve it (flip compare `on` cutSize h34) (bisect h34) $ \ _ -> do
            refit hy (w*h) <$> fmMultiLevel h34 coarseningThreshold matchingRatio


        pure
          ( fromListN (size q1) ((v12!) <$> elems q1)
          , fromListN (size q2) ((v12!) <$> elems q2)
          , fromListN (size q3) ((v34!) <$> elems q3)
          , fromListN (size q4) ((v34!) <$> elems q4)
          )

    m1 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q1 ^? ix (pred x * w + pred y)
    m2 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q2 ^? ix (pred x * w + pred y)
    m3 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q3 ^? ix (pred x * w + pred y)
    m4 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q4 ^? ix (pred x * w + pred y)

    pure $ joinBlocks (m2, m1, m3, m4)

    where
        bisect by = Bisect
            (fromAscList [x | x <- [0 .. length (fst by) - 1], even x])
            (fromAscList [x | x <- [0 .. length (fst by) - 1], odd x])




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
      gates  %= filter (\ x -> x ^. identifier /= sub ^. identifier)
      gates <>= sub ^. gates
    where predicate g = length (view gates g) + length (view gates top) <= succ n
inline _ top = top



