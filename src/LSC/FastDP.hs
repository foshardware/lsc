-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.FastDP
    ( singleSegmentClustering, globalSwap, verticalSwap, localReordering
    , localReorderSegment, swapRoutine, clusterSegment
    , Layout, buildLayout
    , penaltyForSpace, penaltyForCell
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import Data.Either
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap
    ( IntMap
    , lookupLE, lookupGE, lookupLT, lookupGT
    , splitLookup
    , insert, alter
    , adjust, delete
    , singleton
    , fromAscList, toAscList
    )
import qualified Data.IntMap as IntMap
import Data.List (sort, permutations)
import Data.Maybe
import Data.Ratio
import Data.STRef
import Data.Vector (Vector, (!), unsafeFreeze, unsafeThaw, fromListN, update)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (new, read, write, copy)
import qualified Data.Vector.Unboxed.Mutable as T
import Data.Vector.Mutable (slice)
import Prelude hiding (read, lookup)

import LSC.NetGraph
import LSC.Types



-- | wt1 and wt2 penalize overlaps resulting from swapping cells.
-- wt1: Penalty on shifting the closest two cells to resolve overlaps
-- wt2: Penalty on shifting cells other than the closest two cells
--
wt1, wt2 :: Rational
wt1 = 1
wt2 = 5


localReordering :: NetGraph -> LSC NetGraph
localReordering top = do 

    assert "localReordering: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    segments <- liftIO . stToIO
        $ sequence
        $ localReorderSegment 3 top <$> views gates getSegments top

    pure $ top &~ do
        gates %= flip update (liftA2 (,) (view number) id <$> Vector.concat segments)


localReorderSegment :: Int -> NetGraph -> Vector Gate -> ST s (Vector Gate)
localReorderSegment m top segment = do

    let n = m `min` length segment

    assert "localReorderSegment: segment is unsorted!"
        $ all (\ (g, h) -> g ^. space . l <= h ^. space . l) $ Vector.zip segment (Vector.tail segment)

    v <- Vector.thaw segment

    for_ [ 0 .. length segment - n ] $ \ pos -> do

        u <- Vector.freeze $ slice pos n v

        let leftBoundary  = Vector.head u ^. space . l
            rightBoundary = Vector.last u ^. space . r
            windowWidth = rightBoundary - leftBoundary

        let reorder :: Int -> Gate -> Gate
            reorder i | i == pred n = space %~ relocateR rightBoundary
            reorder 0 = space %~ relocateL leftBoundary
            reorder i = space %~ relocateX (leftBoundary + div (i * windowWidth) (pred n))

        let options = zipWith reorder [ 0 .. ] <$> permutations (toList u)

        let reordered = minimumBy (compare `on` hpwlDelta top) (toList u : options)

        sequence_ [ write v (pos + i) g | (i, g) <- zip [ 0 .. ] reordered ]

    unsafeFreeze v




globalSwap :: NetGraph -> LSC NetGraph
globalSwap = liftIO . stToIO . swapRoutine True


verticalSwap :: NetGraph -> LSC NetGraph
verticalSwap = liftIO . stToIO . swapRoutine False



type Layout = IntMap Segment

type Segment = IntMap (Either Area Gate)

type Area = Component Layer Int


type SegmentIterator = Segment -> Int -> [Maybe (Int, Either Area Gate)]


occupied :: Segment -> Int -> Maybe (Int, Either Area Gate)
occupied segment x = fmap snd $ find fst $ catMaybes
    [ liftA2 (,) (lower . snd) id <$> lookupLE x segment
    , liftA2 (,) (upper . snd) id <$> lookupGE x segment
    ] where lower g = either (view r) (view (space . r)) g + either width gateWidth g `div` 2 >= x
            upper g = either (view r) (view (space . r)) g - either width gateWidth g `div` 2 <= x


leftNext, rightNext :: SegmentIterator
leftNext  segment = tail . iterate (flip lookupLT segment . fst =<<) . pure . (, undefined)
rightNext segment = tail . iterate (flip lookupGT segment . fst =<<) . pure . (, undefined)


buildLayout :: Foldable f => f Gate -> Layout
buildLayout
    = fmap intersperseSpace
    . foldl' (\ a g -> alter (pure . insert (g ^. space . to centerX) (Right g) . foldMap id)
                             (g ^. space . to centerY) a) mempty

intersperseSpace :: Segment -> Segment
intersperseSpace segment = gs <> fromAscList
    [ (centerX area, Left area)
    | (u, v) <- xs `zip` tail xs
    , let area = u ^. space & l .~ view (space . r) u & r .~ view (space . l) v
    ] where xs = rights $ snd <$> toAscList gs
            gs = IntMap.filter isRight segment


cutLayout :: (Int, Int) -> IntMap a -> IntMap a
cutLayout (lower, upper) zs
    = foldMap (singleton lower) y <> xs <> foldMap (singleton upper) x
    where (_, y, ys) = splitLookup lower zs
          (xs, x, _) = splitLookup upper ys



cutSegment :: (Int, Int) -> Segment -> Segment
cutSegment (lower, upper) zs
    | null $ cutLayout (lower, upper) zs
    = foldMap (uncurry singleton) (occupied zs lower) <> foldMap (uncurry singleton) (occupied zs upper)
cutSegment (lower, upper) zs
    = cutLayout (lower, upper) zs




swapRoutine :: Bool -> NetGraph -> ST s NetGraph
swapRoutine _ top
    | views gates length top < 3
    = pure top
swapRoutine global top = do

    assert "swapRoutine: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    assert "swapRoutine: gates have different heights"
        $ all (== gateHeight (view gates top ! 0)) $ gateHeight <$> view gates top

    v <- Vector.thaw $ view gates top

    layout <- newSTRef $ views gates buildLayout top

    taint <- T.replicate (views gates length top) False

    for_ (view gates top) $ \ g -> do

      let adjacentG = adjacentByPin top g

      taintG <- T.read taint $ g ^. number

      swaps <- findSwaps global g (optimalRegion adjacentG) <$> readSTRef layout

      let benefits = uncurry (benefit top g) <$> swaps

      unless (taintG || g ^. fixed || all null adjacentG)
        $ do

          taints <- forM swaps $ either (pure . const False) (T.read taint . view number) . snd

          case maximumBy (compare `on` view _1) $ zip3 (0 : benefits) (True : taints) (Right g : map snd swaps) of

              (w, taintH,  _) | w <= 0 || taintH -> pure ()
              (_, _, Right h) | h ^. fixed -> pure ()

              (_, _, Right h) -> do

                  let adjacentH = adjacentByPin top h

                  let i = g & space %~ relocateX (views space centerX h) . relocateB (h ^. space . b)
                      j = h & space %~ relocateX (views space centerX g) . relocateB (g ^. space . b)

                  write v (g ^. number) i
                  write v (h ^. number) j

                  for_ (foldMap id adjacentG <> pure h <> foldMap id adjacentH)
                    $ T.modify taint (const True) . view number

                  modifySTRef layout
                    $ adjust (insert (views space centerX i) (Right i)) (views space centerY i)
                    . adjust (insert (views space centerX j) (Right j)) (views space centerY j)
                    . adjust (delete (views space centerX g)) (views space centerY g)
                    . adjust (delete (views space centerX h)) (views space centerY h)

                  for_ (unstableUnique $ views space centerY <$> [g, h, i, j])
                    $ modifySTRef layout . adjust intersperseSpace

              (_, _, Left a) -> do

                  let i = g & space %~ relocateX (centerX a) . relocateB (a ^. b)

                  write v (g ^. number) i

                  for_ (foldMap id adjacentG)
                    $ T.modify taint (const True) . view number

                  modifySTRef layout
                    $ adjust (insert (views space centerX i) (Right i)) (views space centerY i)
                    . adjust (delete (views space centerX g)) (views space centerY g)

                  for_ (unstableUnique $ views space centerY <$> [g, i])
                    $ modifySTRef layout . adjust intersperseSpace


    gs <- unsafeFreeze v

    pure $ top &~ do
        gates .= gs



type Penalty = Rational

type Benefit = Rational


benefit :: NetGraph -> Gate -> Penalty -> Either Area Gate -> Benefit
benefit top g penalty (Right h)
    = subtract penalty
    $ fromIntegral
    $ negate
    $ hpwlDelta top [g & space .~ view space h, h & space .~ view space g]
benefit top g penalty (Left a)
    = subtract penalty
    $ fromIntegral
    $ negate
    $ hpwlDelta top [g & space .~ a]



findSwaps :: Bool -> Gate -> Area -> Layout -> [(Penalty, Either Area Gate)]
findSwaps True g area layout =
    [ (max 0 penalty, h)
    | (cut, segmentH) <- globalSwapSegments area layout
    , (_, h) <- toAscList cut
    , let segmentG = foldMap id $ layout ^? ix (g ^. space . to centerY)
          penalty = either (penaltyForSpace g segmentH) (penaltyForCell segmentG g segmentH) h
    ]
findSwaps _ g _ layout =
    [ (max 0 penalty, h)
    | (cut, segmentH) <- verticalSwapSegments g layout
    , (_, h) <- toAscList cut
    , let segmentG = foldMap id $ layout ^? ix (g ^. space . to centerY)
          penalty = either (penaltyForSpace g segmentH) (penaltyForCell segmentG g segmentH) h
    ]


verticalSwapSegments :: Gate -> Layout -> [(Segment, Segment)]
verticalSwapSegments g layout
    = toList
    $ fmap (liftA2 (,) (cutSegment (g ^. space . l, g ^. space . r)) id)
    $ foldMap (uncurry singleton) (lookupLT (g ^. space . to centerY) layout)
      <> foldMap (uncurry singleton) (lookupGT (g ^. space . to centerY) layout)


globalSwapSegments :: Area -> Layout -> [(Segment, Segment)]
globalSwapSegments area layout
    = toList
    $ fmap (liftA2 (,) (cutSegment (area ^. l, area ^. r)) id)
    $ cutLayout (area ^. b, area ^. t) layout



spaces :: SegmentIterator -> Int -> Segment -> Area -> [Area]
spaces it n segment = lefts . map snd . catMaybes . take n . it segment . centerX


penaltyForSpace :: Gate -> Segment -> Area -> Penalty
penaltyForSpace i segment s
    = fromIntegral (gateWidth i - width s) * wt1
    + fromIntegral (gateWidth i - (s1 + width s + s2)) * wt2
    where s1 = maybe 0 width $ listToMaybe $ spaces  leftNext 2 segment s
          s2 = maybe 0 width $ listToMaybe $ spaces rightNext 2 segment s 


penaltyForCell :: Segment -> Gate -> Segment -> Gate -> Penalty
penaltyForCell _ i segmentJ j
    | gateWidth i >= gateWidth j
    = fromIntegral ((gateWidth i - gateWidth j) - (s2 + s3)) * wt1
    + fromIntegral ((gateWidth i - gateWidth j) - (s1 + s2 + s3 + s4)) * wt2
    where ls = spaces  leftNext 3 segmentJ (j ^. space)
          rs = spaces rightNext 3 segmentJ (j ^. space)
          s1 = maybe 0 width $ listToMaybe $ drop 1 ls
          s2 = maybe 0 width $ listToMaybe ls
          s3 = maybe 0 width $ listToMaybe rs
          s4 = maybe 0 width $ listToMaybe $ drop 1 rs
penaltyForCell segmentI i segmentJ j
    = penaltyForCell segmentJ j segmentI i




type Cluster = [Gate]


singleSegmentClustering :: NetGraph -> LSC NetGraph
singleSegmentClustering top = do

    assert "singleSegmentClustering: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    segments <- liftIO . stToIO
        $ sequence
        $ clusterSegment top <$> views gates getSegments top

    pure $ top &~ do
        gates %= flip update (liftA2 (,) (view number) id <$> Vector.concat segments)


clusterSegment :: NetGraph -> Vector Gate -> ST s (Vector Gate)
clusterSegment _ segment
    | length segment < 2
    = pure segment
clusterSegment top segment = do

    assert "clusterSegment: segment is unsorted!"
        $ all (\ (g, h) -> g ^. space . l <= h ^. space . l) $ Vector.zip segment (Vector.tail segment)

    let n = length segment

    let area = coarseBoundingBox $ view space <$> segment

    let ordering i
            = HashMap.lookup i
            $ foldMap' (\ (k, g) -> HashMap.singleton (g ^. number) k) (Vector.indexed segment)


    let clusterIndex :: Cluster -> Int
        clusterIndex c = fromJust $ ordering . view number =<< listToMaybe c

    mX <- new n
    let getX c = read mX (clusterIndex c)
    let setX c x
          = write mX (clusterIndex c)
          $ min (area ^. r - div (sum $ gateWidth <$> c) 2)
          $ max (area ^. l + div (sum $ gateWidth <$> c) 2)
          $ x


    sequence_
        $ Vector.zipWith setX (pure <$> segment)
        $ median . generateBoundsList top segment ordering . pure <$> segment

    oldCount <- newSTRef n
    oldCluster <- unsafeThaw $ pure <$> segment

    newCount <- newSTRef 0
    newCluster <- new n

    void
      $ do

        write newCluster 0 =<< read oldCluster 0
        writeSTRef newCount 1

        it <- readSTRef oldCount

        for_ [ 1 .. pred it ] $ \ j -> do -- NEW

            k <- readSTRef newCount

            x <- read newCluster (pred k)
            y <- read oldCluster j

            overlap <- clusterOverlap <$> fmap (, x) (getX x) <*> fmap (, y) (getX y)

            if overlap
            then do
                let merged = x <> y
                write newCluster (pred k) merged
                setX merged $ median $ generateBoundsList top segment ordering merged
            else do
                write newCluster k y -- NEW
                modifySTRef newCount succ

        i <- readSTRef newCount
        copy (slice 0 i oldCluster) (slice 0 i newCluster)
        writeSTRef oldCount i

        `untilM_` do
            cs <- Vector.freeze . flip (slice 0) oldCluster =<< readSTRef oldCount
            xs <- mapM getX cs
            pure $ not $ or
                 $ Vector.zipWith clusterOverlap (Vector.zip xs cs) (Vector.tail (Vector.zip xs cs))

    cs <- Vector.freeze . flip (slice 0) oldCluster =<< readSTRef oldCount
    xs <- mapM getX cs

    pure $ fromListN n
      [ centerCluster (pos, c) g
      | (pos, c) <- toList $ Vector.zip xs cs
      , g <- toList c
      ]



-- assumes gates in clusters in left-to-right order
--
centerCluster :: (Int, Cluster) -> Gate -> Gate
centerCluster (pos, c) g = g & space %~ relocateL (pos - div w 2 + x)
    where
        w = sum $ gateWidth <$> c
        x = sum $ gateWidth <$> takeWhile (g /=) c


-- assumes clusters in left-to-right order
--
clusterOverlap :: (Int, Cluster) -> (Int, Cluster) -> Bool
clusterOverlap (c, x) (d, y)
    = d - div (sum $ gateWidth <$> y) 2
    < c + div (sum $ gateWidth <$> x) 2 



generateBoundsList :: NetGraph -> Vector Gate -> (Int -> Maybe Int) -> Cluster -> [Int]
generateBoundsList _ _ _ c
    | null c
    = error "generateBoundsList: empty cluster"
generateBoundsList _ gs _ _
    | null gs
    = error "generateBoundsList: empty segment"
generateBoundsList top _ _ c
    | null $ hyperedges top c
    = error "generateBoundsList: cluster is not connected"
generateBoundsList top segment ordering c
    = sort
    $ foldMap (\ box -> [box ^. l, box ^. r])
    $ selfContained
    $ filter (/= mempty)
      [ boundingBox
        [ case compare <$> ordering i <*> ordering (head c ^. number) of
            Just LT -> Vector.head segment ^. space
            Just  _ -> Vector.last segment ^. space
            Nothing -> view gates top ! i ^. space
        | i <- HashMap.keys $ net ^. contacts
        , i >= 0
        , not $ elem i $ view number <$> c
        ]
      | net <- hyperedges top c
      ]

    where

      selfContained [] = view space <$> c
      selfContained xs = xs

