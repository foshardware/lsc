-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.FastDP where

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Either
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap
    ( IntMap
    , lookupLE, lookupLT, lookupGT
    , split, splitLookup
    , insert, alter
    , adjust, delete
    , singleton
    , fromAscList, toAscList
    , toDescList
    )
import qualified Data.IntMap as IntMap
import Data.List (sort, permutations)
import Data.Maybe
import Data.STRef
import Data.Vector (Vector, (!), unsafeFreeze, unsafeThaw, fromListN, update)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (new, read, write, copy)
import qualified Data.Vector.Unboxed.Mutable as T
import Data.Vector.Mutable (slice)
import Prelude hiding (read, lookup)

import LSC.Component
import LSC.NetGraph
import LSC.Types



-- | `wt1` and `wt2` penalize overlaps resulting from swapping cells.
-- wt1: Penalty on shifting the closest two cells to resolve overlaps
-- wt2: Penalty on shifting cells other than the closest two cells
--
wt1, wt2 :: Penalty
wt1 = 1
wt2 = 5


-- | Choose the first swap that yields some benefit or the best possible
-- swap when `sufficientBenefit` or the former is Nothing
--
sufficientBenefit :: Maybe Double
sufficientBenefit = Nothing



globalSwap :: NetGraph -> LSC NetGraph
globalSwap top = do
    info ["Global swap"]
    sc <- view scaleFactor <$> technology
    realWorldST . swapRoutine sc False $ top


verticalSwap :: NetGraph -> LSC NetGraph
verticalSwap top = do 
    info ["Vertical swap"]
    sc <- view scaleFactor <$> technology
    realWorldST . swapRoutine sc True $ top




type Layout = IntMap Segment

type Segment = IntMap (Either Area Gate)

type Slot = (Int, Either Area Gate)

type Area = Component Layer Int


type SegmentIterator = Segment -> Int -> [Slot]


leftNext, rightNext :: SegmentIterator
leftNext  segment k = toDescList $ delete k $ fst $ split k segment
rightNext segment k = toAscList  $ delete k $ snd $ split k segment


spaces :: SegmentIterator -> Int -> Segment -> Area -> [Area]
spaces it n segment = lefts . map snd . take n . it segment . centerX



buildLayout :: Foldable f => f Gate -> Layout
buildLayout
    = fmap intersperseSpace
    . foldl' (\ a g -> alter (pure . insertGate g . fold) (rowIndex g) a) mempty
{-# INLINABLE buildLayout #-}


rowIndex :: Gate -> Int
rowIndex = centerY . view space


insertGate :: Gate -> Segment -> Segment
insertGate = liftA2 insert (centerX . view space) Right


removeGate :: Gate -> Segment -> Segment
removeGate = delete . centerX . view space


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
    | null $ cutLayout (lower, upper) zs -- TODO: not obvious what to return when the
    = fold                               --       optimal region's width has degraded
      [ singleton pos g                  --       or a vertical swap is performed
      | (pos, g) <- toList (lookupLE lower zs)
      , lower <= either (view r) (view (space . r)) g + either width gateWidth g `div` 2
      ]
cutSegment (lower, upper) zs
    = cutLayout (lower, upper) zs




swapRoutine :: Double -> Bool -> NetGraph -> ST s NetGraph
swapRoutine _ _ top
    | views gates length top < 3
    = pure top
swapRoutine sc vertical top = do

    assume "swapRoutine: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    assume "swapRoutine: gates have different heights"
        $ all (== gateHeight (view gates top ! 0)) $ gateHeight <$> view gates top

    v <- Vector.thaw $ view gates top

    layout <- newSTRef $ views gates buildLayout top

    taint <- T.replicate (views gates length top) False

    for_ (view gates top) $ \ g -> do

      let adjacentG = adjacentByPin top g

      taintG <- T.read taint $ g ^. number

      swaps <- findSwaps vertical g (optimalRegion adjacentG) <$> readSTRef layout

      let benefits = uncurry (benefit top g) <$> swaps

      unless (taintG || g ^. fixed || all null adjacentG)
        $ do

          taints <- forM swaps $ either (pure . const False) (T.read taint . view number) . snd

          case decide sufficientBenefit $ zip3 (0 : benefits) (True : taints) (Right g : map snd swaps) of

              (w, taintH,  _) | w <= 0 || taintH -> pure ()
              (_, _, Right h) | h ^. fixed || eqNumber g h -> pure ()

              (_, _, Right h) -> do

                  let adjacentH = adjacentByPin top h

                  let i = g & space %~ relocateX (h ^. space . to centerX) . relocateB (h ^. space . b)
                      j = h & space %~ relocateX (g ^. space . to centerX) . relocateB (g ^. space . b)

                  write v (g ^. number) i
                  write v (h ^. number) j

                  for_ (fold adjacentG <> pure h <> fold adjacentH)
                    $ T.modify taint (const True) . view number

                  modifySTRef layout
                    $ adjust (intersperseSpace . insertGate i . removeGate h) (rowIndex i)
                    . adjust (intersperseSpace . insertGate j . removeGate g) (rowIndex g)

              (_, _, Left a) -> do

                  let i = g & space %~ relocateX (centerX a) . relocateB (a ^. b)

                  write v (g ^. number) i

                  for_ (fold adjacentG)
                    $ T.modify taint (const True) . view number

                  modifySTRef layout
                    $ adjust (intersperseSpace . insertGate i) (rowIndex i)
                    . adjust (intersperseSpace . removeGate g) (rowIndex g)

    gs <- unsafeFreeze v

    pure $ top &~ do
        gates .= gs

    where byMaximum = maximumBy . on compare $ view _1
          bySufficient s = find $ (>= sc * s) . fromIntegral . view _1

          decide = maybe byMaximum $ liftA2 fromMaybe byMaximum . bySufficient




type Penalty = Int

type Benefit = Int


benefit :: NetGraph -> Gate -> Penalty -> Either Area Gate -> Benefit
benefit top g penalty (Right h)
    = negate . (+ penalty)
    $ hpwlDelta top [g & space .~ view space h, h & space .~ view space g]
benefit top g penalty (Left a)
    = negate . (+ penalty)
    $ hpwlDelta top [g & space .~ a]



findSwaps :: Bool -> Gate -> Area -> Layout -> [(Penalty, Either Area Gate)]
findSwaps vertical i a layout =
    [ (max 0 penalty, j)
    | (cut, segj) <- if vertical
                     then verticalSwapSegments i layout
                     else globalSwapSegments a layout
    , j <- toList cut
    , let penalty = either (penaltyForSpace i segj) (penaltyForCell segi i segj) j
    ] where segi = fold $ layout ^? ix (rowIndex i)


verticalSwapSegments :: Gate -> Layout -> [(Segment, Segment)]
verticalSwapSegments g
    = map (liftA2 (,) (cutSegment (g ^. space . l, g ^. space . r)) id)
    . fmap snd
    . liftA2 (++) (toList . lookupLT (rowIndex g)) (toList . lookupGT (rowIndex g))


globalSwapSegments :: Area -> Layout -> [(Segment, Segment)]
globalSwapSegments a
    = map (liftA2 (,) (cutSegment (a ^. l, a ^. r)) id)
    . toList
    . cutLayout (a ^. b, a ^. t)



penaltyForSpace :: Gate -> Segment -> Area -> Penalty
penaltyForSpace i segj s
    = fromIntegral (gateWidth i - width s) * wt1
    + fromIntegral (gateWidth i - (s1 + width s + s2)) * wt2
    where s1 = maybe 0 width $ listToMaybe $ spaces  leftNext 2 segj s
          s2 = maybe 0 width $ listToMaybe $ spaces rightNext 2 segj s


penaltyForCell :: Segment -> Gate -> Segment -> Gate -> Penalty
penaltyForCell _ i segj j
    | gateWidth i >= gateWidth j
    = fromIntegral ((gateWidth i - gateWidth j) - (s2 + s3)) * wt1
    + fromIntegral ((gateWidth i - gateWidth j) - (s1 + s2 + s3 + s4)) * wt2
    where ls = spaces  leftNext 3 segj (j ^. space)
          rs = spaces rightNext 3 segj (j ^. space)
          s1 = maybe 0 width $ listToMaybe $ drop 1 ls
          s2 = maybe 0 width $ listToMaybe ls
          s3 = maybe 0 width $ listToMaybe rs
          s4 = maybe 0 width $ listToMaybe $ drop 1 rs
penaltyForCell segi i segj j
    = penaltyForCell segj j segi i




localReordering :: NetGraph -> LSC NetGraph
localReordering top = do

    info ["Local reordering"]

    assume "localReorder: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    segments <- realWorldST
        $ sequence
        $ localReorderSegment 3 top <$> views gates getSegments top

    pure $ top &~ do
        gates %= flip update (liftA2 (,) (view number) id <$> fold segments)



localReorderSegment :: Int -> NetGraph -> Vector Gate -> ST s (Vector Gate)
localReorderSegment m top segment = do

    let n = m `min` length segment

    assume "localReorderSegment: segment is unsorted!"
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




singleSegmentClustering :: NetGraph -> LSC NetGraph
singleSegmentClustering top = do

    info ["Single segment clustering"]

    assume "singleSegmentClustering: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    segments <- realWorldST
        $ sequence
        $ clusterSegment top <$> views gates getSegments top

    pure $ top &~ do
        gates %= flip update (liftA2 (,) (view number) id <$> Vector.concat segments)



clusterSegment :: NetGraph -> Vector Gate -> ST s (Vector Gate)
clusterSegment _ segment
    | length segment < 2
    = pure segment
clusterSegment top segment = do

    assume "clusterSegment: segment is unsorted!"
        $ all (\ (g, h) -> g ^. space . l <= h ^. space . l) $ Vector.zip segment (Vector.tail segment)

    let n = length segment

    let area = coarseBoundingBox $ view space <$> segment

    let order i
            = preview (ix i)
            $ ifoldMap (\ k g -> HashMap.singleton (g ^. number) k) segment

    mX <- T.new n
    let getX = T.read mX . clusterIndex order
    let setX c
          = T.write mX (clusterIndex order c)
          . min (area ^. r - div (sum $ gateWidth <$> c) 2)
          . max (area ^. l + div (sum $ gateWidth <$> c) 2)


    sequence_
        $ Vector.zipWith setX (pure <$> segment)
        $ median . generateBoundsList top segment order . pure <$> segment

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
                setX merged $ median $ generateBoundsList top segment order merged
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



type Cluster = [Gate]

type Order = Int -> Maybe Int


clusterIndex :: Order -> Cluster -> Int
clusterIndex order c
    | Just n <- order . view number =<< listToMaybe c
    = n
clusterIndex _ _
    = error "clusterIndex: not found in order"


-- assumes gates in clusters in left-to-right order
--
centerCluster :: (Int, Cluster) -> Gate -> Gate
centerCluster (pos, c) g = g & space %~ relocateL (pos - div w 2 + x)
    where
        w = sum $ gateWidth <$> c
        x = sum $ gateWidth <$> takeWhile (not . eqNumber g) c


-- assumes clusters in left-to-right order
--
clusterOverlap :: (Int, Cluster) -> (Int, Cluster) -> Bool
clusterOverlap (c, x) (d, y)
    = d - div (sum $ gateWidth <$> y) 2
    < c + div (sum $ gateWidth <$> x) 2 



generateBoundsList :: NetGraph -> Vector Gate -> Order -> Cluster -> [Int]
generateBoundsList _ _ _ c
    | null c
    = error "generateBoundsList: empty cluster"
generateBoundsList _ gs _ _
    | null gs
    = error "generateBoundsList: empty segment"
generateBoundsList top _ _ c
    | null $ hyperedges top c
    = error "generateBoundsList: cluster is not connected"
generateBoundsList top segment order c
    = sort
    $ foldMap (\ box -> [box ^. l, box ^. r])
    $ selfContained
    $ filter (/= mempty)
      [ boundingBox
        [ case compare <$> order i <*> order (head c ^. number) of
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

