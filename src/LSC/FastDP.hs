-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TupleSections #-}

module LSC.FastDP where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Either
import Data.Function
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (adjust, lookupLT, lookupGT)
import Data.List (sort, permutations)
import Data.Maybe
import Data.Scientific
import Data.STRef
import Data.Vector (Vector, (!), unsafeFreeze, unsafeThaw, fromListN, update)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (new, read, write, copy, slice)
import qualified Data.Vector.Unboxed.Mutable as T
import Prelude hiding (read, lookup)

import LSC.BinarySearch
import LSC.Cartesian
import LSC.Component
import LSC.HigherOrder
import LSC.Layout
import LSC.Model
import LSC.NetGraph
import LSC.Transformer



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
sufficientBenefit :: Maybe Scientific
sufficientBenefit = Nothing



globalSwap :: NetGraph -> LSC IO NetGraph
globalSwap top = do
    info ["Global swap"]
    sc <- view scaleFactor <$> technology
    determinate $ swapRoutine sc False top


verticalSwap :: NetGraph -> LSC IO NetGraph
verticalSwap top = do 
    info ["Vertical swap"]
    sc <- view scaleFactor <$> technology
    determinate $ swapRoutine sc True top



swapRoutine :: Scientific -> Bool -> NetGraph -> ST s NetGraph
swapRoutine _ _ top
  | views gates length top < 3
  = pure top
swapRoutine sc vertical top
  = do

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
              (_, _, Right h) | h ^. fixed || g ^. number == h ^. number -> pure ()

              (_, _, Right h) -> do

                  let adjacentH = adjacentByPin top h

                  let i = g & geometry %~ relocateX (h ^. geometry . to centerX) . relocateB (h ^. geometry . b)
                      j = h & geometry %~ relocateX (g ^. geometry . to centerX) . relocateB (g ^. geometry . b)

                  write v (g ^. number) i
                  write v (h ^. number) j

                  for_ (fold adjacentG <> pure h <> fold adjacentH)
                    $ T.modify taint (const True) . view number

                  modifySTRef layout
                    $ adjust (intersperseSpace . insertGate i . removeGate h) (ordinate i)
                    . adjust (intersperseSpace . insertGate j . removeGate g) (ordinate g)

              (_, _, Left a) -> do

                  let i = g & geometry %~ relocateX (centerX a) . relocateB (a ^. b)

                  write v (g ^. number) i

                  for_ (fold adjacentG)
                    $ T.modify taint (const True) . view number

                  modifySTRef layout
                    $ adjust (intersperseSpace . insertGate i) (ordinate i)
                    . adjust (intersperseSpace . removeGate g) (ordinate g)

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
  $ hpwlDelta top [g & geometry .~ view geometry h, h & geometry .~ view geometry g]
benefit top g penalty (Left a)
  = negate . (+ penalty)
  $ hpwlDelta top [g & geometry .~ a]



findSwaps :: Bool -> Gate -> Area -> Layout -> [(Penalty, Either Area Gate)]
findSwaps vertical i a layout =
  [ (max 0 penalty, j)
  | (cut, segj) <- if vertical
                   then verticalSwapSegments i layout
                   else globalSwapSegments a layout
  , j <- toList cut
  , let penalty = either (penaltyForSpace i segj) (penaltyForCell segi i segj) j
  ] where segi = fold $ layout ^? ix (ordinate i)


verticalSwapSegments :: Gate -> Layout -> [(Segment, Segment)]
verticalSwapSegments g
  = map (liftA2 (,) (cutSegment (g ^. geometry . l, g ^. geometry . r)) id)
  . fmap snd
  . liftA2 (++) (toList . lookupLT (ordinate g)) (toList . lookupGT (ordinate g))


globalSwapSegments :: Area -> Layout -> [(Segment, Segment)]
globalSwapSegments a
  = map (liftA2 (,) (cutSegment (a ^. l, a ^. r)) id)
  . toList
  . cutLayout (a ^. b, a ^. t)



penaltyForSpace :: Gate -> Segment -> Area -> Penalty
penaltyForSpace i segj s
  = fromIntegral (gateWidth i - width s) * wt1
  + fromIntegral (gateWidth i - (s1 + width s + s2)) * wt2
  where
    s1 = maybe 0 width $ listToMaybe $ spaces  leftNext 2 segj s
    s2 = maybe 0 width $ listToMaybe $ spaces rightNext 2 segj s


penaltyForCell :: Segment -> Gate -> Segment -> Gate -> Penalty
penaltyForCell _ i segj j
  | gateWidth i >= gateWidth j
  = fromIntegral ((gateWidth i - gateWidth j) - (s2 + s3)) * wt1
  + fromIntegral ((gateWidth i - gateWidth j) - (s1 + s2 + s3 + s4)) * wt2
  where
    ls = spaces  leftNext 3 segj (j ^. geometry)
    rs = spaces rightNext 3 segj (j ^. geometry)
    s1 = maybe 0 width $ listToMaybe $ drop 1 ls
    s2 = maybe 0 width $ listToMaybe ls
    s3 = maybe 0 width $ listToMaybe rs
    s4 = maybe 0 width $ listToMaybe $ drop 1 rs
penaltyForCell segi i segj j
  = penaltyForCell segj j segi i




localReordering :: NetGraph -> LSC IO NetGraph
localReordering top = do

    info ["Local reordering"]

    assume "localReorder: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    segments <- determinate
        $ sequence
        $ localReorderSegment 3 top <$> views gates getSegments top

    pure $ top &~ do
        gates %= flip update (liftA2 (,) (view number) id <$> fold segments)



localReorderSegment :: Int -> NetGraph -> Vector Gate -> ST s (Vector Gate)
localReorderSegment m top segment = do

    let n = m `min` length segment

    assume "localReorderSegment: segment is unsorted!"
        $ all (\ (g, h) -> g ^. geometry . l <= h ^. geometry . l) $ Vector.zip segment (Vector.tail segment)

    v <- Vector.thaw segment

    for_ [ 0 .. length segment - n ] $ \ pos -> do

        u <- Vector.freeze $ slice pos n v

        let leftBoundary  = Vector.head u ^. geometry . l
            rightBoundary = Vector.last u ^. geometry . r
            windowWidth = rightBoundary - leftBoundary

        let reorder :: Int -> Gate -> Gate
            reorder i | i == pred n = geometry %~ relocateR rightBoundary
            reorder 0 = geometry %~ relocateL leftBoundary
            reorder i = geometry %~ relocateX (leftBoundary + div (i * windowWidth) (pred n))

        let options = zipWith reorder [ 0 .. ] <$> permutations (toList u)

        let reordered = minimumBy (compare `on` hpwlDelta top) (toList u : options)

        sequence_ [ write v (pos + i) g | (i, g) <- zip [ 0 .. ] reordered ]

    unsafeFreeze v




singleSegmentClustering :: NetGraph -> LSC IO NetGraph
singleSegmentClustering top = do

    info ["Single segment clustering"]

    assume "singleSegmentClustering: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    segments <- determinate
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
        $ all (\ (g, h) -> g ^. geometry . l <= h ^. geometry . l) $ Vector.zip segment (Vector.tail segment)

    let n = length segment

    let area = coarseBoundingBox $ view geometry <$> segment

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
centerCluster (pos, c) g = g & geometry %~ relocateL (pos - div w 2 + x)
    where
        w = sum $ gateWidth <$> c
        x = sum $ gateWidth <$> takeWhile (\ h -> g ^. number /= h ^. number) c


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
    $ foldMap abscissae
    $ selfContained
    $ filter (/= mempty)
      [ foldMap' implode
        [ case compare <$> order i <*> order (head c ^. number) of
            Just LT -> Vector.head segment ^. geometry
            Just  _ -> Vector.last segment ^. geometry
            Nothing -> view gates top ! i ^. geometry
        | i <- views contacts HashMap.keys net
        , i >= 0
        , notElem i $ view number <$> c
        ]
      | net <- hyperedges top c
      ]

    where

      selfContained [] = BoundingBox . view geometry <$> c
      selfContained xs = xs

