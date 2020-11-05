{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module LSC.FastDP where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import Data.Foldable
import Data.Function
import qualified Data.HashSet as HashSet
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.IntMap (IntMap, lookupGE, lookupLE)
import qualified Data.IntMap as IntMap
import Data.List (sort, sortOn, permutations)
import Data.List.Split (wordsBy)
import Data.Map (keys)
import Data.Maybe
import Data.STRef
import Data.Vector (Vector, (!), (//), unsafeFreeze, unsafeThaw, generate)
import Data.Vector.Mutable (STVector, new, read, write)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (slice)
import Prelude hiding (read, filter)

import LSC.NetGraph
import LSC.Types


type HashTable s k v = C.HashTable s k v


wt1, wt2 :: Integer
wt1 = 1
wt2 = 1


localReordering :: NetGraph -> LSC NetGraph
localReordering top = do 

    assert "localReordering: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    groups <- liftIO . stToIO
        $ sequence
        $ sequence
        . fmap (localReorderSegment3 top)
        . wordsBy (view fixed)
        . sortOn (view space) <$> IntMap.fromListWith (++)
        [ (fromIntegral $ g ^. space . b, [ g ])
        | g <- toList $ view gates top
        ]

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap join groups ])


localReorderSegment3 :: NetGraph -> [Gate] -> ST s [Gate]
localReorderSegment3 top gs
  | segment <- Vector.fromList gs
  , length segment >= 3
  = do

    v <- unsafeThaw segment

    let slidingWindow = [ (i, permutations [ i .. i + 2 ]) | i <- [ 0 .. length segment - 3 ] ]

    for_ slidingWindow $ \ (pos, perms) -> do

        u <- Vector.freeze $ slice pos 3 v

        let leftBoundary  = u!0 ^. space . l
            rightBoundary = u!2 ^. space . r
            midpoint = div (u!1 ^. space . l + u!1 ^. space . r) 2

        let reorder (0, i) = u ! (i - pos) & space %~ relocateL leftBoundary
            reorder (2, i) = u ! (i - pos) & space %~ relocateR rightBoundary
            reorder (_, i) = u ! (i - pos) & space %~ relocateX midpoint
            reorder :: (Int, Int) -> Gate

        let reordered = minimumBy (compare `on` hpwlDelta top) $ toList u : (fmap reorder . zip [0..] <$> perms)

        sequence_ [ write v (pos + i) g | (i, g) <- zip [0 ..] reordered ]

    toList <$> unsafeFreeze v

localReorderSegment3 _ gs = pure gs



globalSwap :: NetGraph -> LSC NetGraph
globalSwap = liftIO . stToIO . swapST True


verticalSwap :: NetGraph -> LSC NetGraph
verticalSwap = liftIO . stToIO . swapST False


swapST :: Bool -> NetGraph -> ST s NetGraph
swapST global top = do

    assert "globalSwapST: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    v <- Vector.thaw $ view gates top

    byCoords <- pure $ IntMap.fromListWith (<>)
        [ (fromIntegral $ g ^. space . to centerY, IntMap.singleton (fromIntegral $ g ^. space . to centerX) g)
        | g <- toList $ view gates top
        ]

    let vector = generate (top ^. gates . to length) id

    for_ vector $ \ k -> do

      g <- read v k
      unless (g ^. fixed)
        do

        (x, y) <- optimalRegionCenterST top v g

        let (y1, segment) = if global then findGlobalSwapSegment byCoords y else findVerticalSwapSegment byCoords y g
        let (_, swapCell) = if global then findSwapCell segment x g else findSwapCell segment (g ^. space . to centerX) g

        case swapCell of

            Just h
              | h ^. fixed -> pure ()

            Nothing
              | delta <- hpwlDelta top [g & space %~ relocateX x . relocateY y1]
              , (s1, s, s2) <- findSwapSpaces segment x
              , p1 <- (g ^. space . to width - s) * wt1
              , p2 <- (g ^. space . to width - (s1 + s + s2)) * wt2
              , negate delta - p1 - p2 > 0 
              -> do

                 write v (g ^. number) $ g & space %~ relocateX x . relocateY y1

            Just h
              | delta <- hpwlDelta top
                [ g & space %~ relocateX x . relocateY y1
                , h & space %~ relocateX (g ^. space . to centerX) . relocateY (g ^. space . to centerY)
                ]
              , i <- minimumBy (compare `on` gateWidth) [g, h]
              , j <- maximumBy (compare `on` gateWidth) [g, h]
              , (s1, s, s2) <- findSwapSpaces segment x
              , p1 <- (j ^. space . to width - i ^. space . to width - (s - i ^. space . to width)) * wt1
              , p2 <- (j ^. space . to width - i ^. space . to width - (s1 + s - i ^. space . to width + s2)) * wt2
              , negate delta - p1 - p2 > 0
              -> do

                 write v (g ^. number) $ g & space %~ relocateX x . relocateY y1
                 write v (h ^. number) $ h & space %~ relocateX (g ^. space . to centerX) . relocateY (g ^. space . to centerY)

            _ -> pure ()
 
    gateVector <- unsafeFreeze v

    pure $ top &~ do
        gates .= gateVector



findGlobalSwapSegment :: Integral a => IntMap (IntMap Gate) -> a -> (a, IntMap Gate)
findGlobalSwapSegment byCoords pos = (fromIntegral y1, s)
    where
      y = fromIntegral pos
      (y1, s) = minimumBy (compare `on` \ (y', _) -> abs $ y' - y) $ catMaybes [lookupLE y byCoords, lookupGE y byCoords]


findVerticalSwapSegment :: Integral a => IntMap (IntMap Gate) -> a -> Gate -> (a, IntMap Gate)
findVerticalSwapSegment byCoords pos g = (fromIntegral y1, s)
    where
      y  = fromIntegral pos
      yo = fromIntegral $ g ^. space . to centerY
      (y1, s) = minimumBy (compare `on` \ (y', _) -> abs $ y' - y) $ catMaybes [lookupLE yo byCoords, lookupGE yo byCoords]


findSwapCell :: Integral a => IntMap Gate -> a -> Gate -> (a, Maybe Gate)
findSwapCell s pos g
    |  x' == x
    || gateWidth g < gateWidth h && x' < x && hr >= gr
    || gateWidth g < gateWidth h && x' > x && hl <= gl
    || gateWidth g > gateWidth h && x' < x && hr <= gr
    || gateWidth g > gateWidth h && x' > x && hl >= gl
    = (pos, Just h)
    where
      x = fromIntegral pos

      (x', h) = minimumBy (compare `on` f) $ catMaybes [lookupLE x s, lookupGE x s]
      f (x1, i) | x1 > x = x1 - x + fromIntegral (gateWidth i `div` 2)
      f (x1, i) | x1 < x = x - x1 - fromIntegral (gateWidth i `div` 2)
      f _ = 0

      gl = x  - fromIntegral (gateWidth g `div` 2)
      hl = x' - fromIntegral (gateWidth h `div` 2)

      gr = x  + fromIntegral (gateWidth g `div` 2)
      hr = x' + fromIntegral (gateWidth h `div` 2)

findSwapCell _ pos _ = (pos, Nothing)


findSwapSpaces :: Integral a => IntMap Gate -> a -> (a, a, a)
findSwapSpaces  s pos
    | x <- fromIntegral pos
    , Just (x1, i1) <- lookupLE x s
    , Just (x2, i2) <- lookupGE x s
    , Just (_, i0) <- lookupLE x1 s
    , Just (_, i3) <- lookupGE x2 s
    = ( fromIntegral $ i1 ^. space . l - i0 ^. space . r
      , fromIntegral $ i2 ^. space . l - i1 ^. space . r
      , fromIntegral $ i3 ^. space . l - i2 ^. space . r
      )
findSwapSpaces  _ _ = (0, 0, 0)



optimalRegionCenterST :: NetGraph -> STVector s Gate -> Gate -> ST s (Integer, Integer)
optimalRegionCenterST top v g = do
    boxes <-  sequence
      [ boundingBox <$> sequence cs
      | net <- catMaybes [ top ^. nets ^? ix x . contacts . to keys | x <- toList $ g ^. wires, x /= "clk", x /= "CLK" ]
      , let cs = [ view space <$> read v i | i <- net, i /= g ^. number, i >= 0, i < top ^. gates . to length ]
      , not $ null cs
      ]
    let xs = sort $ join [ [box ^. l, box ^. r] | box <- boxes ]
    let ys = sort $ join [ [box ^. b, box ^. t] | box <- boxes ]
    pure (median xs, median ys)



type Cluster = [Gate]


singleSegmentClustering :: NetGraph -> LSC NetGraph
singleSegmentClustering top = do

    assert "singleSegmentClustering: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    groups <- liftIO . stToIO
        $ sequence
        $ sequence
        . fmap (clustering top)
        . wordsBy (view fixed) 
        . sortOn (view space) <$> IntMap.fromListWith (++)
        [ (fromIntegral $ g ^. space . b, [ g ])
        | g <- toList $ view gates top
        ]

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap join groups ])


clustering :: NetGraph -> [Gate] -> ST s [Gate]
clustering top gs = do

    let segment = Vector.fromList gs
    let n = length segment

    let area = coarseBoundingBox $ view space <$> segment

    (getX, setX, mX) <- newX (area ^. l, area ^. r) n

    numOldCluster <- newSTRef n
    oldCluster <- unsafeThaw $ pure <$> segment

    when (n > 0)
      $ flip untilM_ (not <$> segmentOverlaps getX numOldCluster oldCluster) $ do

        numOld <- readSTRef numOldCluster
        old <- Vector.freeze (slice 0 numOld oldCluster)
        sequence_ [ setX c $ optimalRegionCenterIn segment top c | c <- toList old ]

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
                setX merged $ optimalRegionCenterIn segment top merged
            else do
                write newCluster k y -- NEW
                modifySTRef' newcount succ
            modifySTRef' j succ -- NEW

        writeSTRef numOldCluster =<< readSTRef newcount
        i <- readSTRef newcount
        sequence_ [ write oldCluster x =<< read newCluster x | x <- [ 0.. pred i ] ]

    clusters <- H.toList mX
    let byGateNumber = IntMap.fromList [ (k, entry) | entry <- sortOn (length . fst) clusters, k <- fst entry ]

    pure
      [ centerCluster pos (fmap (view gates top !) c) g
      | g <- toList segment
      , (c, pos) <- toList $ byGateNumber ^? ix (view number g)
      ]


centerCluster :: Integer -> Cluster -> Gate -> Gate
centerCluster pos c g = g & space %~ relocateL (pos - div w 2 + x)
    where
        w = sum $ gateWidth <$> c
        x = sum $ gateWidth <$> takeWhile (g /=) c


clusterOverlaps :: (Cluster -> ST s (Maybe Integer)) -> Cluster -> Cluster -> ST s Bool
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


segmentOverlaps :: (Cluster -> ST s (Maybe Integer)) -> STRef s Int -> STVector s Cluster -> ST s Bool
segmentOverlaps getX numOldCluster oldCluster = do
    n <- readSTRef numOldCluster
    v <- Vector.freeze (slice 0 n oldCluster)
    -- overlaps <- sequence $ uncurry (clusterOverlaps getX) <$> Vector.zip v (Vector.drop 1 v)
    overlaps <- sequence [ clusterOverlaps getX x y | (x, y) <- distinctPairs $ toList v ]
    pure $ or overlaps


optimalRegionCenterIn :: Vector Gate -> NetGraph -> Cluster -> Integer
optimalRegionCenterIn segment top c = median boundsList

    where

      boundsList = sort $ join [ [box ^. l, box ^. r] | box <- boxes ]

      leftEnd  = Vector.head segment ^. space
      rightEnd = Vector.last segment ^. space

      boxes =
        [ boundingBox cs
        | net <- toList $ HashSet.fromList $ catMaybes
            [ top ^. nets ^? ix x . contacts . to keys
            | m <- view wires <$> c
            , x <- toList m
            , x /= "clk", x /= "CLK", not $ x `elem` power
            ]
        , cs <- pure $ catMaybes $
            [ top ^. gates ^? ix i . space
            | i <- net
            , not $ elem i $ view number <$> segment
            ] ++
            [ Just $ if x < y then leftEnd else rightEnd
            | i <- net
            , elem i $ view number <$> segment
            , not $ elem i $ view number <$> c
            , let x = Vector.findIndex (== i) (view number <$> segment)
            , let y = Vector.findIndex (== view number (head c)) (view number <$> segment)
            ]
        , not $ null cs
        ]


newX
  :: (Integer, Integer) -> Int
  -> ST s (Cluster -> ST s (Maybe Integer), Cluster -> Integer -> ST s (), HashTable s [Int] Integer)
newX bounds n = do
    mX <- H.newSized $ n + div (n*n) 2
    let getX c = H.lookup mX (view number <$> c)
    let setX c x = H.insert mX (view number <$> c)
                 $ min (snd bounds - div (sum $ gateWidth <$> c) 2)
                 $ max (fst bounds + div (sum $ gateWidth <$> c) 2)
                 $ x
    pure (getX, setX, mX)

