-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.Legalize where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import qualified Data.Array as G
import Data.Default
import Data.Foldable
import Data.Function
import Data.Graph
import Data.HashMap.Lazy (insert, delete)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import Data.STRef
import Data.Vector (Vector, (!), unsafeFreeze, unsafeThaw, update, unsafeUpd)
import Data.Vector.Mutable (read, modify)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as ST
import Prelude hiding (read, lookup)
import Text.Printf

import LSC.BinarySearch
import LSC.Cartesian
import LSC.Component
import LSC.Model
import LSC.NetGraph
import LSC.Transformer



legalizeRows :: NetGraph -> LSC NetGraph
legalizeRows top = do

    info ["Row legalization"]

    assume "legalizeRows: gate vector indices do not match gate numbers"
      $ and $ imap (==) $ view number <$> view gates top

    segments <- liftST
      $ sequence
      $ rowLegalization top <$> getRows (top ^. gates)

    pure
      $ top &~ do
        gates %= flip update (liftA2 (,) (view number) id <$> fold segments)


rowLegalization :: NetGraph -> Vector Gate -> ST s (Vector Gate)
rowLegalization _ gs
  | length gs <= 1
  = pure gs
rowLegalization _ gs
  | all (view fixed) gs
  = pure gs
rowLegalization _ gs
  | all (\ (g, h) -> g ^. space . l <= h ^. space . l) $ Vector.zip gs (Vector.tail gs)
  , not $ or $ Vector.zipWith gateOverlap gs (Vector.tail gs)
  = pure gs
rowLegalization top gs
  = do

    assume "rowLegalization: row is unsorted!"
      $ all (\ (g, h) -> g ^. space . l <= h ^. space . l) $ Vector.zip gs (Vector.tail gs)

    let y = Vector.head gs ^. space . b

    assume "rowLegalization: row is not aligned!" $ all (\ g -> g ^. space . b == y) gs

    let row = top ^. supercell . rows ^? ix y

    assume "rowLegalization: no corresponding row found!" $ isJust row

    let res = maybe 1 (view granularity) row
        off = maybe 0 (subtract res . view l) row `div` res

    let w = (`div` res) . width . view space <$> gs
        x = subtract off . (`div` res) . view (space . l) <$> gs

    let n = maybe 1 (succ . view cardinality) row
        m = length gs

    let count = succ n * succ m

    let target = pred count

    let vertex j k = k + j * succ n
        site i = divMod i $ succ n

    d <- ST.replicate count count
    ST.write d 0 0

    p <- ST.new count
    ST.write p 0 0

    for_ [0 .. m] $ \ j -> do
      for_ [1 .. n] $ \ k -> do

        void
          $ do

            let u = vertex j (pred k)
            let v = vertex j k

            dv <- ST.read d v
            du <- ST.read d u

            when (dv > du)
              $ do
                ST.write d v du
                ST.write p v u

        unless (j < 1)
          $ unless (k > n - w ! pred j)
          $ unless (gs ! pred j ^. fixed && x ! pred j /= k)
          $ do

            let u = vertex (pred j) k
            let v = vertex j (k + w ! pred j)

            let weight = abs $ x ! pred j - k

            dv <- ST.read d v
            du <- ST.read d u

            when (dv > du + weight)
              $ do
                ST.write d v $ du + weight
                ST.write p v u

    predecessor <- Unbox.unsafeFreeze p

    let shortestPath = takeWhile (> 0) (iterate (Unbox.unsafeIndex predecessor) target)

    let diagonal = tail . map head . groupOn fst . map site $ shortestPath

    assume "rowLegalization: cannot legalize row!" $ not $ null diagonal

    pure $ unsafeUpd gs
      [ (i, gs ! i & space %~ relocateL ((pos + off) * res))
      | (i, pos) <- diagonal
      ]


-- | This is here to facilitate the writing of reference implementations
--
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

        when (dv > du + w)
          $ do
            ST.write d v $ du + w
            ST.write p v $ u

    predecessor <- Unbox.unsafeFreeze p

    pure $ takeWhile (> 0) (iterate (Unbox.unsafeIndex predecessor) target)




sortVectorOn :: Ord b => (a -> b) -> Vector a -> ST s (Vector a)
sortVectorOn f v = do
    u <- Vector.thaw v
    Intro.sortBy (on compare f) u
    unsafeFreeze u



juggleCells :: NetGraph -> LSC NetGraph
juggleCells top = do

    info ["Row juggling"]

    assume "juggleCells: gate vector indices do not match gate numbers"
      $ and $ imap (==) $ view number <$> view gates top

    debugRowCapacities top

    rc <- view rowCapacity <$> environment

    result <- liftST . rowJuggling rc $ top

    debugRowCapacities result

    pure result


rowJuggling :: Double -> NetGraph -> ST s NetGraph
rowJuggling _ top
  | views gates null top
  = pure top
rowJuggling rc top
  = do

    let rs = top ^. supercell . rows

    let table
          = Vector.fromList
          [ HashMap.fromList [ (g ^. number, g) | g <- toList row ]
          | row <- top ^. gates . to getRows
          ]

    let rowWidth p = ceiling $ rc * fromIntegral (view cardinality p * view granularity p)

    let w = maybe 0 rowWidth . (rs ^?) . ix . minimum . fmap (minY . view space) <$> table
        y = minimum . fmap (minX . view space) <$> table

    surplus <- unsafeThaw $ Vector.zipWith subtract w $ sum . fmap gateWidth <$> table

    matrix <- Vector.thaw table

    loaded <- sortVectorOn (negate . snd) . Vector.filter ((0 <) . snd) . Vector.indexed
          <=< Vector.freeze $ surplus

    for_ loaded $ \ (i, _) -> do

      whileM_ ((0 <) <$> read surplus i)
        $ do

          bestHpwl <- newSTRef maxBound
          bestRow  <- newSTRef minBound
          bestCell <- newSTRef def

          cs <- read matrix i

          for_ cs $ \ c -> do
            for_ [ 0 .. length table - 1 ] $ \ k -> do

              let g = c & space %~ relocateB (y ! k)
                  delta = hpwlDelta top [g]

              s <- read surplus k
              d <- readSTRef bestHpwl

              unless (c ^. fixed || c ^. feedthrough)
                $ when (s < 0 && abs s > gateWidth c && delta < d)
                $ do
                  writeSTRef bestHpwl delta
                  writeSTRef bestRow  k
                  writeSTRef bestCell g

          k <- readSTRef bestRow
          c <- readSTRef bestCell

          assume "rowJuggling: no space left to juggle - increase row capacity!"
            $ k > minBound

          modify matrix (views number delete c) i
          modify matrix (views number insert c c) k

          modify surplus (subtract $ gateWidth c) i
          modify surplus (+ gateWidth c) k

    grouped <- unsafeFreeze matrix

    pure
      $ top &~ do
        gates %= flip unsafeUpd (foldMap HashMap.toList grouped)



debugRowCapacities :: NetGraph -> LSC ()
debugRowCapacities top = do

    rc <- view rowCapacity <$> environment

    let rs = top ^. supercell . rows
    let table = top ^. gates . to getRows
    let rowWidth p = fromIntegral (view cardinality p * view granularity p)
    let caps = fmap (maybe 0 rowWidth . (rs ^?) . ix . view (space . b) . Vector.head) table

    debug $ "Row capacities:" :
        [ printf "Row %d: %d (%.2f%%)" (i :: Int) (cap :: Int) (f :: Double)
          <> if f <= rc * 100 then mempty else printf " exceeds maximum! (%.2f%%)" (rc * 100)
        | (i, gs, cap) <- zip3 [1..] table caps
        , let gw = sum $ gateWidth <$> gs
        , let f = 100 * fromIntegral gw / fromIntegral cap
        , f > rc * 10
        , gw > (0 :: Int)
        ]

