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
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.IntMap (IntMap, lookupGE, lookupLE, lookupLT, lookupGT, singleton, fromListWith)
import Data.List (sortOn, permutations)
import Data.Maybe
import Data.Ratio
import Data.STRef
import Data.Vector (Vector, (!), (//), unsafeFreeze, unsafeThaw, fromListN)
import Data.Vector.Mutable (STVector, new, read, write, modify)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (slice)
import Prelude hiding (read, filter)

import LSC.NetGraph
import LSC.Types


type HashTable s k v = C.HashTable s k v


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

    groups <- liftIO . stToIO
        $ sequence
        $ localReorderSegment3 top <$> getSegments (top ^. gates)

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap toList groups ])


localReorderSegment3 :: NetGraph -> Vector Gate -> ST s (Vector Gate)
localReorderSegment3 top gs = do

    v <- Vector.thaw gs

    let slidingWindow = [ (i, permutations [ i .. i + 2 ]) | i <- [ 0 .. length gs - 3 ] ]

    for_ slidingWindow $ \ (pos, perms) -> do

        u <- Vector.freeze $ slice pos 3 v

        let leftBoundary  = u!0 ^. space . l
            rightBoundary = u!2 ^. space . r
            midpoint = div (u!0 ^. space . l + u!2 ^. space . r) 2

        let reorder (0, i) = u ! (i - pos) & space %~ relocateL leftBoundary
            reorder (2, i) = u ! (i - pos) & space %~ relocateR rightBoundary
            reorder (_, i) = u ! (i - pos) & space %~ relocateX midpoint
            reorder :: (Int, Int) -> Gate

        let options = fmap reorder . zip [0..] <$> perms
        let reordered = minimumBy (compare `on` hpwlDelta top) (toList u : options)

        sequence_ [ write v (pos + i) g | (i, g) <- zip [0 ..] reordered ]

    unsafeFreeze v



globalSwap :: NetGraph -> LSC NetGraph
globalSwap = liftIO . stToIO . swapST True


verticalSwap :: NetGraph -> LSC NetGraph
verticalSwap = liftIO . stToIO . swapST False


type Netlist = IntMap Segment

type Segment = IntMap Gate


swapST :: Bool -> NetGraph -> ST s NetGraph
swapST global top = do

    assert "globalSwapST: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    v <- Vector.thaw $ view gates top

    let byCoords = fromListWith (<>)
            [ (g ^. space . to centerY, singleton (g ^. space . to centerX) g)
            | g <- toList $ view gates top
            ]

    for_ [ 0 .. top ^. gates . to length - 1 ] $ \ k -> do

      g <- read v k
      ns <- sequence $ sequence . fmap (read v) <$> connectedIndices top g

      unless (g ^. fixed || null ns) $ do

        let (x, y) = optimalRegionCenter ns

        let (y1, segment) = if global
              then findGlobalSwapSegment byCoords y g
              else findVerticalSwapSegment byCoords y g
        let (_, swapCell) = if global
              then findSwapCell segment x g
              else findSwapCell segment (g ^. space . to centerX) g

        case swapCell of

            Just h
              | h ^. fixed || g == h -> pure ()

            Nothing
              | delta <- hpwlDelta top [g & space %~ relocateX x . relocateY y1]
              , (s1, s, s2) <- findSwapSpaces segment x
              , p1 <- fromIntegral (gateWidth g - s) * wt1
              , p2 <- fromIntegral (gateWidth g - (s1 + s + s2)) * wt2
              , fromIntegral (negate delta) - p1 - p2 > 0 
              -> do

                 modify v (space %~ relocateX x . relocateY y1) (g ^. number)

            Just h
              | delta <- hpwlDelta top
                [ g & space %~ relocateX x . relocateY y1
                , h & space %~ relocateX (g ^. space . to centerX) . relocateY (g ^. space . to centerY)
                ]
              , i <- minimumBy (compare `on` gateWidth) [g, h]
              , j <- maximumBy (compare `on` gateWidth) [g, h]
              , (s1, s, s2) <- findSwapSpaces segment x -- TODO: this should be in the segment of the smaller cell
              , p1 <- fromIntegral (gateWidth j - gateWidth i - (s - gateWidth i)) * wt1
              , p2 <- fromIntegral (gateWidth j - gateWidth i - (s1 + s - gateWidth i + s2)) * wt2
              , fromIntegral (negate delta) - p1 - p2 > 0
              -> do

                 modify v (space %~ relocateX x . relocateY y1) (g ^. number)
                 modify v (space %~ relocateX (g ^. space . to centerX) . relocateY (g ^. space . to centerY)) (h ^. number)

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
    |  x' == x
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

    groups <- liftIO . stToIO
        $ sequence
        $ clustering top <$> getSegments (top ^. gates)

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap toList groups ])


clustering :: NetGraph -> Vector Gate -> ST s (Vector Gate)
clustering top segment = do

    let n = length segment

    let area = coarseBoundingBox $ view space <$> segment

    (getX, setX, mX) <- newX (area ^. l, area ^. r) n

    numOldCluster <- newSTRef n
    oldCluster <- unsafeThaw $ pure <$> segment

    when (n > 0)
      $ flip untilM_ (not <$> segmentOverlaps getX numOldCluster oldCluster) $ do

        numOld <- readSTRef numOldCluster
        old <- Vector.freeze (slice 0 numOld oldCluster)
        sequence_ [ setX c $ median $ boundsListSSC segment top c | c <- toList old ]

        newcount <- newSTRef 1
        newCluster <- new n
        write newCluster 0 =<< read oldCluster 0

        j <- newSTRef 1

        whileM_ ((<) <$> readSTRef j <*> readSTRef numOldCluster) $ do

            k <- readSTRef newcount
            i <- readSTRef j
            x <- read newCluster (pred k)
            y <- read oldCluster i

            mapM_ (setX y) =<< max <$> getX x <*> getX y -- NEW

            overlap <- clusterOverlaps getX x y

            if overlap
            then do
                let merged = x <> y
                write newCluster (pred k) merged
                setX merged $ median $ boundsListSSC segment top merged
            else do
                write newCluster k y -- NEW
                modifySTRef' newcount succ
            modifySTRef' j succ -- NEW

        writeSTRef numOldCluster =<< readSTRef newcount
        i <- readSTRef newcount
        sequence_ [ write oldCluster x =<< read newCluster x | x <- [ 0.. pred i ] ]

    clusters <- sortOn (negate . length . fst) <$> H.toList mX

    pure $ fromListN n
      [ centerCluster pos (fmap (view gates top !) c) (view gates top ! k)
      | (k, (c, pos)) <- uniqueBy (compare `on` fst)
        [ (k, entry) | entry <- clusters, k <- fst entry ]
      ]


centerCluster :: Int -> Cluster -> Gate -> Gate
centerCluster pos c g = g & space %~ relocateL (pos - div w 2 + x)
    where
        w = sum $ gateWidth <$> c
        x = sum $ gateWidth <$> takeWhile (g /=) c


clusterOverlaps :: (Cluster -> ST s (Maybe Int)) -> Cluster -> Cluster -> ST s Bool
clusterOverlaps _ [] _ = pure False
clusterOverlaps _ _ [] = pure False
clusterOverlaps getX x y = do

    cm <- getX x
    dm <- getX y

    let cw = sum $ gateWidth <$> x
    let dw = sum $ gateWidth <$> y

    case (,) <$> cm <*> dm of

        Nothing -> pure False

        Just (c, d) -> do

            let (c1, c2) = (c - div cw 2, c + div cw 2)
                (d1, d2) = (d - div dw 2, d + div dw 2)

            pure $ c1 == d1 || c2 == d2 || c1 < d1 && d1 < c2 || d1 < c1 && c1 < d2


segmentOverlaps :: (Cluster -> ST s (Maybe Int)) -> STRef s Int -> STVector s Cluster -> ST s Bool
segmentOverlaps getX numOldCluster oldCluster = do
    n <- readSTRef numOldCluster
    v <- Vector.freeze (slice 0 n oldCluster)
    -- overlaps <- sequence $ uncurry (clusterOverlaps getX) <$> Vector.zip v (Vector.drop 1 v)
    overlaps <- sequence [ clusterOverlaps getX x y | (x, y) <- distinctPairs $ toList v ]
    pure $ or overlaps



boundsListSSC :: Vector Gate -> NetGraph -> Cluster -> [Int]
boundsListSSC segment top c = join [ [box ^. l, box ^. r] | box <- boxes ]

    where

      leftEnd  = Vector.head segment ^. space
      rightEnd = Vector.last segment ^. space

      boxes =
        [ boundingBox cs
        | k <- uniqueBy compare
          [ k
          | ks <- view wires <$> c
          , k <- toList ks
          , not $ control k
          ]
        , cs <- pure $
          [ view gates top ! i ^. space
          | i <- maybe [] HashMap.keys $ top ^. nets ^? ix k . contacts
          , i >= 0
          , not $ elem i $ view number <$> segment
          ] ++
          [ if x < y then leftEnd else rightEnd
          | i <- maybe [] HashMap.keys $ top ^. nets ^? ix k . contacts
          , i >= 0
          , not $ elem i $ view number <$> c
          , elem i $ view number <$> segment
          , let x = Vector.findIndex (== i) (view number <$> segment)
          , let y = Vector.findIndex (== view number (head c)) (view number <$> segment)
          ]
        , not $ null cs
        ]


newX
  :: (Int, Int) -> Int
  -> ST s (Cluster -> ST s (Maybe Int), Cluster -> Int -> ST s (), HashTable s [Int] Int)
newX bounds n = do
    mX <- H.newSized $ n + div (n*n) 2
    let getX c = H.lookup mX (view number <$> c)
    let setX c x = H.insert mX (view number <$> c)
                 $ min (snd bounds - div (sum $ gateWidth <$> c) 2)
                 $ max (fst bounds + div (sum $ gateWidth <$> c) 2)
                 $ x
    pure (getX, setX, mX)

