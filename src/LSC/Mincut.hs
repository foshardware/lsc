{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.Mincut where

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Default
import Data.Foldable hiding (concat)
import Data.Function
import Data.IntSet (IntSet, size, elems, singleton, member, fromDistinctAscList)
import qualified Data.IntSet as S
import Data.Map (Map, fromList, restrictKeys, withoutKeys, mapWithKey, lookup, assocs, keysSet) 
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Matrix (Matrix, (<|>), (<->), nrows, ncols, fromLists, matrix, submatrix, getRow)
import Data.Vector (Vector, take, drop, filter, fromListN, (!), generate, concat)
import Prelude hiding (filter, concat, lookup, take, drop)

import LSC.FM as FM
import LSC.NetGraph
import LSC.Types







initialMatrix :: NetGraph -> LSC (Matrix Gate)
initialMatrix top = do

    let (w, h) = head $ dropWhile (\ (x, y) -> x * y < top ^. gates . to length) boards
        result = matrix w h $ \ (x, y) -> maybe def id $ top ^. gates ^? ix (pred x * w + pred y)

    pure result



boards :: [(Int, Int)]
boards = iterate (bimap (2*) (2*)) (1, 1)



placeMatrix :: Matrix Gate -> LSC (Matrix Gate)
placeMatrix m | nrows m * ncols m <= 4 = pure m
placeMatrix m = do

    let v = filter (\ g -> g ^. number >= 0) $ concat [ getRow i m | i <- [1 .. nrows m] ]
        e = rebuildEdges $ set number `imap` v

    (q1, q2, q3, q4) <- liftIO $ nonDeterministic $ do

        h <- st $ hypergraph (set number `imap` v) e
        Bisect q12 q34 <- fmMultiLevel h coarseningThreshold matchingRatio

        let v12 = fromListN (size q12) $ (v!) <$> elems q12
            v34 = fromListN (size q34) $ (v!) <$> elems q34
        let e12 = rebuildEdges $ set number `imap` v12
            e34 = rebuildEdges $ set number `imap` v34

        h12 <- st $ hypergraph (set number `imap` v12) e12
        Bisect q1 q2 <- fmMultiLevel h12 coarseningThreshold matchingRatio

        h34 <- st $ hypergraph (set number `imap` v34) e34
        Bisect q3 q4 <- fmMultiLevel h34 coarseningThreshold matchingRatio

        pure
          ( fromListN (size q1) ((v12!) <$> elems q1)
          , fromListN (size q2) ((v12!) <$> elems q2)
          , fromListN (size q3) ((v34!) <$> elems q3)
          , fromListN (size q4) ((v34!) <$> elems q4)
          )

    let h = nrows m `div` 2
        w = ncols m `div` 2

    m1 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q1 ^? ix (pred x * w + pred y)
    m2 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q2 ^? ix (pred x * w + pred y)
    m3 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q3 ^? ix (pred x * w + pred y)
    m4 <- placeMatrix $ matrix h w $ \ (x, y) -> maybe def id $ q4 ^? ix (pred x * w + pred y)


    pure $ (m2 <|> m1) <-> (m3 <|> m4)




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



