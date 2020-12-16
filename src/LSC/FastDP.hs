-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module LSC.FastDP where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap, lookupGE, lookupLE, lookupLT, lookupGT, insert, alter)
import Data.List (sort, permutations)
import Data.Maybe
import Data.Ratio
import Data.STRef
import Data.Vector (Vector, (!), unsafeFreeze, unsafeThaw, fromListN, update)
import Data.Vector.Mutable (new, read, write, modify, copy)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (slice)
import Prelude hiding (read)

import LSC.NetGraph
import LSC.Types



-- | wt1 and wt2 penalize overlaps resulting from swapping cells.
-- wt1: Penalty on shifting the closest two cells to resolve overlaps
-- wt2: Penalty on shifting cells other than the closest two cells
--
wt1, wt2 :: Rational
wt1 = 1 % 2
wt2 = 1


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


type Netlist = IntMap Segment

type Segment = IntMap Gate


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
    tainted <- unsafeThaw $ False <$ view gates top

    let byCoords = foldl' (\ a g -> alter (pure . insert (g ^. space . to centerX) g . foldMap id)
                                          (g ^. space . to centerY) a)
                          mempty $ top ^. gates

    for_ (view gates top) $ \ g -> do

      taintG <- read tainted $ g ^. number

      let ns = adjacentByPin top g

      unless (taintG || g ^. fixed || null ns) $ do

        area <- optimalRegion <$> sequence (mapM (read v . view number) <$> ns)
        let x = centerX area
            y = centerY area

        let (y1, segment) = if global
              then findGlobalSwapSegment byCoords y g
              else findVerticalSwapSegment byCoords y g
        let (_, swapCell) = if global
              then findSwapCell segment x g
              else findSwapCell segment (g ^. space . to centerX) g

        taintH <- maybe (pure False) (read tainted . view number) swapCell

        case swapCell of

          Just h | taintH || h ^. fixed || g == h -> pure ()

          Nothing
              | delta <- hpwlDelta top [g & space %~ relocateX x . relocateY y1]
              , (s1, s, s2) <- findSwapSpaces segment x
              , p1 <- fromIntegral (gateWidth g - s) * wt1
              , p2 <- fromIntegral (gateWidth g - (s1 + s + s2)) * wt2
              , fromIntegral (negate delta) - p1 - p2 > 0 
              ->  do

                  modify v (space %~ relocateX x . relocateY y1) (g ^. number)

                  sequence_ $ mapM_ (flip (write tainted) True . view number) <$> ns

          Just h
              | delta <- hpwlDelta top
                [ g & space %~ relocateX x . relocateY y1
                , h & space %~ relocateX (g ^. space . to centerX) . relocateB (g ^. space . b)
                ]
              , i <- minimumBy (compare `on` gateWidth) [g, h]
              , j <- maximumBy (compare `on` gateWidth) [g, h]
              , (s1, s, s2) <- findSwapSpaces segment x -- TODO: this should be in the segment of the smaller cell
              , p1 <- fromIntegral (gateWidth j - gateWidth i - (s - gateWidth i)) * wt1
              , p2 <- fromIntegral (gateWidth j - gateWidth i - (s1 + s - gateWidth i + s2)) * wt2
              , fromIntegral (negate delta) - p1 - p2 > 0
              ->  do

                  modify v (space %~ relocateX x . relocateY y1) (g ^. number)
                  modify v (space %~ relocateX (g ^. space . to centerX) . relocateB (g ^. space . b)) (h ^. number)

                  sequence_ $ mapM_ (flip (write tainted) True . view number) <$> ns

          _ -> pure ()
 
    gs <- unsafeFreeze v

    pure $ top &~ do
        gates .= gs



findGlobalSwapSegment :: Netlist -> Int -> Gate -> (Int, Segment)
findGlobalSwapSegment byCoords y _
    = minimumBy (compare `on` \ (y', _) -> abs $ y' - y) $ catMaybes [lookupLE y byCoords, lookupGE y byCoords]


findVerticalSwapSegment :: Netlist -> Int -> Gate -> (Int, Segment)
findVerticalSwapSegment byCoords y g
    = minimumBy (compare `on` \ (y', _) -> abs $ y' - y) $ catMaybes [lookupLT p byCoords, lookupGT p byCoords]
    where p = g ^. space . to centerY


findSwapCell :: Segment -> Int -> Gate -> (Int, Maybe Gate)
findSwapCell s x g
     | x' == x
    || gateWidth g < gateWidth h && x' < x && hr >= gr
    || gateWidth g < gateWidth h && x' > x && hl <= gl
    || gateWidth g > gateWidth h && x' < x && hr <= gr
    || gateWidth g > gateWidth h && x' > x && hl >= gl
    = (x, Just h)
    where

      (x', h) = minimumBy (compare `on` f) $ catMaybes [lookupLE x s, lookupGE x s]
      f (x1, i) | x1 > x = x1 - x + gateWidth i `div` 2
      f (x1, i) | x1 < x = x - x1 - gateWidth i `div` 2
      f _ = 0

      gl = x  - gateWidth g `div` 2
      hl = x' - gateWidth h `div` 2

      gr = x  + gateWidth g `div` 2
      hr = x' + gateWidth h `div` 2

findSwapCell _ x _ = (x, Nothing)


findSwapSpaces :: Segment -> Int -> (Int, Int, Int)
findSwapSpaces s x
    | Just (x1, i1) <- lookupLE x s
    , Just (x2, i2) <- lookupGE x s
    , Just (_, i0) <- lookupLE x1 s
    , Just (_, i3) <- lookupGE x2 s
    = ( i1 ^. space . l - i0 ^. space . r
      , i2 ^. space . l - i1 ^. space . r
      , i3 ^. space . l - i2 ^. space . r
      )
findSwapSpaces _ _ = (0, 0, 0)



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
             i <- readSTRef oldCount
             cs <- Vector.freeze $ slice 0 i oldCluster
             xs <- mapM getX cs
             pure $ not $ or
                  $ Vector.zipWith clusterOverlap (Vector.zip xs cs) (Vector.tail (Vector.zip xs cs))

    i <- readSTRef oldCount
    cs <- unsafeFreeze $ slice 0 i oldCluster
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



generateBoundsList :: Ord n => NetGraph -> Vector Gate -> (Int -> Maybe n) -> Cluster -> [Int]
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

