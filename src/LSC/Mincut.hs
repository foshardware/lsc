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

    debug [show $ view number <$> result]

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

    debug $ show . fmap (view number) <$> [m1,m2,m3,m4]

    let result = (m2 <|> m1) <-> (m3 <|> m4)

    debug [show $ view number <$> result]

    pure result




hypergraph :: Vector Gate -> Map Identifier Net -> ST s (V, E)
hypergraph v e = inputRoutine (length e) (length v)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList e
    , w ^. identifier /= "clk"
    , (c, _) <- w ^. contacts . to assocs
    ]



columns :: NetGraph -> LSC NetGraph
columns top = do
  let k = ceiling (sqrt $ fromIntegral $ top ^. gates . to length :: Float)
  next <- kWayPartitioning k top
  let cs = [ (s ^. identifier, s) | s <- leaves next ]
      ss = Map.fromList cs
      gs = fromListN (length ss)
        [ def &~ do
            identifier .= i
            wires .= mapWithKey const (c ^. supercell . pins)
        | (i, c) <- assocs ss
        ]
  pure $ next &~ do
      gates .= gs
      subcells .= ss




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




recursiveBisection :: Int -> NetGraph -> LSC NetGraph
recursiveBisection 0 top
    = pure top
recursiveBisection i top = do
    next <- bisection top
    if next ^. subcells . to length <= 1
        then pure top
        else flip (set subcells) next <$> recursiveBisection (pred i) `mapM` view subcells next


bisection :: NetGraph -> LSC NetGraph
bisection top = do

  debug
    [ unpack (top ^. identifier) ++ ": exlining starts"
    , netGraphStats top
    ]

  it <- view cutRatio <$> environment
  let estimate = sqrt $ fromIntegral $ length $ view gates top :: Float
  let predicate (h, p) = fromIntegral (cutSize h p) <= fromIntegral it * estimate

  (_, Bisect p q) <- liftIO $ iterateUntil predicate $ nonDeterministic $ do
      h <- st $ inputRoutine
          (top ^. nets . to length)
          (top ^. gates . to length)
          [ (n, c)
          | (n, w) <- zip [0..] $ toList $ top ^. nets
          , w ^. identifier /= "clk"
          , (c, _) <- w ^. contacts . to assocs
          ]
      (h, ) <$> fmMultiLevel h coarseningThreshold matchingRatio

  -- get a gate
  let g i = view gates top ! i

  -- gate vector for each partition
  let g1 = set number `imap` fromListN (size p) (g <$> elems p)
      g2 = set number `imap` fromListN (size q) (g <$> elems q)

  -- edge identifiers for each partition
  let e1 = Set.fromList $ snd <$> foldMap (view $ wires . to assocs) g1
      e2 = Set.fromList $ snd <$> foldMap (view $ wires . to assocs) g2

      eb = Set.intersection e1 e2
      cut = Set.difference eb $ top ^. supercell . pins . to keysSet

  -- signals originating in first partition
  cells <- view stdCells <$> technology
  let s1 = Set.fromList
        [ name
        | n <- toList g1
        , c <- toList $ view identifier n `lookup` cells
        , (i, name) <- n ^. wires . to assocs
        , sp <- toList $ lookup i $ c ^. pins
        , sp ^. dir == Just Out
        ]

  -- cut edges between partitions
  let fc = fromList [(e, def & identifier .~ e & dir .~ Just Out) | e <- toList cut]
      f1 = mconcat [restrictKeys fc s1, invert <$> withoutKeys fc s1]
      f2 = mconcat [withoutKeys fc s1, invert <$> restrictKeys fc s1]

  -- super cell pins for each partition
  let p1 = restrictKeys (top ^. supercell . pins) e1
      p2 = restrictKeys (top ^. supercell . pins) e2

  -- super cells for each partition
  let c1 = top &~ do
        identifier .= view identifier top <> "_"
        supercell %= (pins .~ p1 <> f1)
        gates .= g1
        nets .= rebuildEdges g1
  let c2 = top &~ do
        identifier .= view identifier top <> "-"
        supercell %= (pins .~ p2 <> f2)
        gates .= g2
        nets .= rebuildEdges g2

  -- new cell for each partition
  let n1 = def &~ do
        identifier .= view identifier c1
        wires .= mapWithKey const p1
  let n2 = def &~ do
        identifier .= view identifier c2
        wires .= mapWithKey const p2

  let pt = [(n1, c1) | size p > 0] ++ [(n2, c2) | size q > 0]

  let result = top &~ do
        gates .= (fst <$> fromListN 2 pt)
        nets .= rebuildEdges (fst <$> fromListN 2 pt)
        subcells .= fromList (fmap (\ (_, c) -> (view identifier c, c)) pt)

  debug
    [ unpack (view identifier top) ++ ": exlining finished"
    , netGraphStats result
    , "max cut size: "++ show (floor $ fromIntegral it * estimate :: Int)
    , "cut size: "++ show (length cut)
    , ""
    ]

  pure result


