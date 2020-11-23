
module LSC.Legalize where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import Control.Monad.Writer
import qualified Data.Array as G
import Data.Default
import Data.Foldable
import Data.Function
import Data.Graph
import Data.List (groupBy)
import Data.STRef
import Data.Vector (Vector, (!), (//), unsafeFreeze, unsafeThaw, fromListN)
import Data.Vector.Mutable (read, modify)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as ST
import Prelude hiding (read, lookup)
import Text.Printf

import LSC.NetGraph
import LSC.Types



legalizeRows :: NetGraph -> LSC NetGraph
legalizeRows top = do

    assert "legalizeRows: gate vector indices do not match gate numbers"
        $ and $ imap (==) $ view number <$> view gates top

    let resolution = minimum $ top ^. supercell . rows <&> view granularity

    groups <- liftIO . stToIO
        $ sequence
        $ rowLegalization resolution top <$> getRows (top ^. gates)

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap toList groups ])


rowLegalization :: Int -> NetGraph -> Vector Gate -> ST s (Vector Gate)
rowLegalization   _   _ gs | length gs < 2 = pure gs
rowLegalization   _   _ gs | all (view fixed) gs = pure gs
rowLegalization res top gs = do

    let n = head (top ^. supercell . geometry <&> width) `div` res
        m = length gs

    let w = (`div` res) .  width . view space <$> gs
        x = (`div` res) . view l . view space <$> gs

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
        minPERB (u, _) | (j, k) <- site u, x ! j /= k, gs ! j ^. fixed = count
        minPERB (u, _) | (j, k) <- site u = abs $ x ! j - k

    shortestPath <- topologicalShortestPath minPERB graph $ pred count

    let leftMost = fmap head $ groupBy (on (==) fst) $ fmap site shortestPath

    assert "cannot legalize row!" $ tail leftMost /= [(0, 0)]

    pure $ gs //
      [ (g, gs ! g & space %~ relocateL (u * res))
      | (g, u) <- tail leftMost
      ]


topologicalShortestPath :: (Edge -> Int) -> Graph -> Int -> ST s [Int]
topologicalShortestPath weight graph target = do

    let count = length graph

    d <- ST.replicate count $ div maxBound 2
    ST.write d 0 0

    p <- ST.new count
    ST.write p 0 0

    for_ (topSort graph) $ \ u -> do
      for_ (graph G.! u) $ \ v -> do

        let w = weight (u, v)

        dv <- ST.read d v
        du <- ST.read d u

        when (dv > du + w) $ do
          ST.write d v $ du + w
          ST.write p v $ u

    execWriter . traversePath target <$> Unbox.unsafeFreeze p


traversePath :: Int -> Unbox.Vector Int -> Writer [Int] ()
traversePath 0 _ = tell [0]
traversePath x g = tell [x] >> traversePath (Unbox.unsafeIndex g x) g



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

    let table = fromListN (length rs) $ toList <$> getRows (top ^. gates)

    let rowWidth p = ceiling $ rc * fromIntegral (view l p + view cardinality p * view granularity p)
    let w = fmap (maybe 0 rowWidth . flip preview rs . ix . view (space . b) . head) table

    surplus <- unsafeThaw $ uncurry (-) <$> Vector.zip (sum . fmap gateWidth <$> table) w

    sorted <- unsafeThaw . Vector.indexed =<< Vector.freeze surplus
    Intro.sortBy (flip compare `on` snd) sorted
    ranked <- unsafeFreeze sorted

    matrix <- Vector.thaw table

    for_ (Vector.takeWhile ( \ (_, s) -> s > 0) ranked) $ \ (i, _) -> do

      whileM_ ((0 <) <$> read surplus i)
        $ do

          bestHpwl <- newSTRef maxBound
          bestRow  <- newSTRef (-1)
          bestCell <- newSTRef def

          cs <- read matrix i

          for_ cs $ \ c -> do
            for_ [ 0 .. length table - 1 ] $ \ k -> do

              let g = c & space %~ relocateB (head (table ! k) ^. space . b)
              let delta = hpwlDelta top [g]

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

    rc <- view rowCapacity <$> environment

    let rs = top ^. supercell . rows
    let table = top ^. gates . to getRows
    let rowWidth p = ceiling $ rc * fromIntegral (view l p + view cardinality p * view granularity p)
    let caps = fmap (maybe 0 rowWidth . flip preview rs . ix . view (space . b) . Vector.head) table

    debug $ "Row capacities:" :
        [ printf "Row %d: %d (%.2f%%)" (i :: Int) (cap :: Int) (f :: Double)
          <> if f <= rc * 100 then mempty else printf " exceeds maximum! (%.2f%%)" (rc * 100)
        | (i, gs, cap) <- zip3 [1..] table caps
        , let gw = sum $ gateWidth <$> gs
        , let f = 100 * fromIntegral gw / fromIntegral cap
        , f > rc * 10
        , gw > (0 :: Int)
        ]

