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
import Data.STRef
import Data.Default
import Data.Foldable hiding (concat)
import Data.Function
import Data.Maybe
import Data.IntSet (size, elems, singleton, intersection)
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Matrix hiding (toList, (!))
import Data.Vector (Vector, filter, fromListN, (!), concat, thaw, unsafeFreeze)
import Data.Vector.Mutable ( read, write)
import Prelude hiding (filter, concat, lookup, take, read)

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
placeWindow m = do

    let w = ncols m `div` 2
        h = nrows m

    let v = flattenGateMatrix m
        e = rebuildEdges $ set number `imap` v


    Bisect p q <- liftIO $ nonDeterministic $ do

        hy <- st $ hypergraph (set number `imap` v) e
        fmMultiLevel hy coarseningThreshold matchingRatio

    let u = I.fromList [ (c ^. number, c) | c <- toList v ]

    let result = ala Endo foldMap [ reorder i v u $ Bisect p q | i <- [1 .. h] ] m

    m1 <- placeWindow $ submatrix 1 h 1 w result
    m2 <- placeWindow $ submatrix 1 h (succ w) (ncols m) result

    pure $ m1 <|> m2




reorder :: Int -> Vector Gate -> IntMap Gate -> Bipartitioning -> Matrix Gate -> Matrix Gate
reorder i v u (Bisect p q) m = runST $ do

    let this = foldMap singleton $ filter (>= 0) $ view number <$> getRow i m
        that = foldMap (singleton . view number) . fmap (v!) . elems
        cells = fromListN (ncols m) $ catMaybes $ (u ^?) . ix
           <$> elems (intersection this $ that p)
            <> elems (intersection this $ that q)

    ctr <- newSTRef 0
    mrow <- thaw $ getRow i m
    for_ [0 .. ncols m - 1] $ \ j -> do
        k <- readSTRef ctr
        g <- read mrow j
        unless (g ^. number < 0) $ do
            write mrow j $ cells ! k
            modifySTRef' ctr succ

    row <- unsafeFreeze mrow
    pure $ mapRow (\ j _ -> row ! pred j) i m






placeQuad :: NetGraph -> LSC NetGraph
placeQuad top = do

    m <- placeMatrix =<< initialMatrix top

    pure top



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

        -- TODO: rebalance sections based on available space
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

    pure $ joinBlocks (m2, m1, m3, m4)




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



