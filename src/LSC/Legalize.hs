
module LSC.Legalize where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import qualified Data.Array as G
import Data.Default
import Data.Foldable
import Data.Function
import Data.Graph
import qualified Data.IntMap as IntMap
import Data.List (sortOn, groupBy)
import Data.Maybe
import Data.STRef
import Data.Vector ((!), (//), unsafeFreeze, unsafeThaw)
import Data.Vector.Mutable (read, modify)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed.Mutable as ST
import Prelude hiding (read, lookup)
import Text.Printf

import LSC.NetGraph
import LSC.Types



legalizeRows :: NetGraph -> LSC NetGraph
legalizeRows top = do

    assert "legalizeRows: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    let resolution = minimum $ top ^. supercell . rows <&> views granularity fromIntegral

    groups <- liftIO . stToIO
        $ sequence
        $ rowLegalization resolution top
        . sortOn (view space) <$> IntMap.fromListWith (++)
        [ (fromIntegral $ g ^. space . b, [ g ])
        | g <- toList $ view gates top
        ]

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap id groups ])


rowLegalization :: Int -> NetGraph -> [Gate] -> ST s [Gate]
rowLegalization res top gs = do

    let gateVector = Vector.fromList gs

    let bySites f g = g ^. space . to ((`div` res) . fromIntegral . f)
        bySites :: (Component Layer Integer -> Integer) -> Gate -> Int

    let n = maybe 0 (`div` res) $ listToMaybe $ top ^. supercell . geometry <&> width <&> fromIntegral
        m = length gateVector

    let w = bySites    width <$> gateVector
        x = bySites (view l) <$> gateVector

    let count = succ m * succ n

    let vertex j k = k + j * succ n
        site i = divMod i $ succ n

    let graph = buildG (0, count - 1) $
          [ (vertex j (pred k), vertex j k)
          | j <- [0 .. m]
          , k <- [1 .. n]
          ]
          ++
          [ (vertex (pred j) k, vertex j (k + w ! pred j))
          | j <- [1 .. m]
          , k <- [1 .. n - w ! pred j]
          ]

    let minPERB (u, v) | (i, _) <- site u, (j, _) <- site v, i == j = 0
        minPERB (u, _) | (j, k) <- site u, x ! j /= k, gateVector ! j ^. fixed = div maxBound 2
        minPERB (u, _) | (j, k) <- site u = abs $ x ! j - k

    shortestPath <- topologicalShortestPath minPERB count graph

    let leftMost = fmap (* res) $ fmap snd $ fmap last $ groupBy (on (==) fst) $ fmap site shortestPath

    assert "cannot legalize row: try reducing the row capacity, e. g. lsc --row-capacity=0.5"
        $ length leftMost >= length gateVector

    pure
      [ g & space %~ relocateL (fromIntegral u)
      | (u, g) <- leftMost `zip` toList gateVector
      ]


topologicalShortestPath :: (Edge -> Int) -> Int -> Graph -> ST s [Int]
topologicalShortestPath weight count graph = do

    d <- ST.replicate count (div maxBound 2)
    ST.write d 0 (0 :: Int)

    p <- ST.new count
    ST.write p 0 (0 :: Int)

    for_ (topSort graph) $ \ u -> do
      for_ (graph G.! u) $ \ v -> do

        let w = weight (u, v)

        dv <- ST.read d v
        du <- ST.read d u

        when (dv > du + w) $ do
          ST.write d v $ du + w
          ST.write p v $ u

    shortestPath <- newSTRef [pred count]
    whileM_ ((0 <) . head <$> readSTRef shortestPath)
      $ do
        modifySTRef shortestPath . (:) =<< ST.read p . head =<< readSTRef shortestPath

    readSTRef shortestPath



juggleCells :: NetGraph -> LSC NetGraph
juggleCells top = do

    debugRowCapacities top

    rc <- view rowCapacity <$> environment

    result <- liftIO $ stToIO $ rowJuggling rc top

    debugRowCapacities result

    pure result


rowJuggling :: Double -> NetGraph -> ST s NetGraph
rowJuggling rc top = do

    assert "rowJuggling: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    let rs = top ^. supercell . rows
        gs = top ^. gates

    let groups = IntMap.fromListWith (++) [ (fromIntegral $ g ^. space . b, [ g ]) | g <- toList gs ]

    let widths = rs <&> (*) <$> view granularity <*> view cardinality
    let table  = rs <&> maybe mempty id . flip preview groups . ix . views b fromIntegral

    surplus <- unsafeThaw $ uncurry (-) <$> Vector.zip
        (sum . fmap gateWidth <$> table)
        (ceiling . (rc *) . fromIntegral <$> widths)

    sorted <- unsafeThaw . Vector.indexed =<< Vector.freeze surplus
    Intro.sortBy (flip compare `on` snd) sorted
    ranked <- unsafeFreeze sorted

    matrix <- Vector.thaw table

    for_ (Vector.takeWhile ( \ (_, s) -> s > 0) ranked) $ \ (i, _) -> do

      whileM_ ((0 <) <$> read surplus i)
        $ do

          bestHpwl <- newSTRef (maxBound :: Int)
          bestRow  <- newSTRef (-1)
          bestCell <- newSTRef def

          cs <- read matrix i

          for_ cs $ \ c -> do
            for_ [ 0 .. length table - 1 ] $ \ k -> do

              let g = c & space %~ relocateB (rs!k ^. b)
              let delta = fromIntegral $ hpwlDelta top [g]

              s <- read surplus k
              d <- readSTRef bestHpwl

              when (s < 0 && abs s > gateWidth c && views fixed not c && delta < d)
                $ do
                  writeSTRef bestHpwl delta
                  writeSTRef bestRow  k
                  writeSTRef bestCell g

          k <- readSTRef bestRow
          c <- readSTRef bestCell

          assert "no space left to juggle - increase row capacity!" $ k >= 0

          modify matrix (filter (/= c)) i
          modify matrix (c : ) k

          modify surplus (\ s -> s - gateWidth c) i
          modify surplus (\ s -> s + gateWidth c) k

    grouped <- unsafeFreeze matrix

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap id grouped ])



debugRowCapacities :: NetGraph -> LSC ()
debugRowCapacities top = do

    let rs = top ^. supercell . rows

    rc <- view rowCapacity <$> environment

    let groups = IntMap.fromListWith (++) [ (fromIntegral $ g ^. space . b, [ g ]) | g <- toList $ top ^. gates ]

    let widths = rs <&> (*) <$> view granularity <*> view cardinality
    let table  = rs <&> maybe mempty id . flip preview groups . ix . views b fromIntegral

    debug $ "Row capacities:" :
        [ printf "Row %d: %d (%.2f%%)" (i :: Int) (w :: Integer) (f :: Double)
          <> if f <= rc * 100 then mempty else printf " exceeds maximum! (%.2f%%)" (rc * 100)
        | (i, rw, gs) <- zip3 [1..] (toList widths) (toList table)
        , let w = sum $ gateWidth <$> gs
        , let f = 100 * fromIntegral w / fromIntegral rw
        , f > rc * 10
        , rw > (0 :: Integer)
        ]

