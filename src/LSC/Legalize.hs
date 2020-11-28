
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

    rc <- view rowCapacity <$> environment

    groups <- liftIO . stToIO
        $ sequence
        $ rowLegalization rc top <$> getRows (top ^. gates)

    pure $ top &~ do
        gates %= (// [ (g ^. number, g) | g <- foldMap toList groups ])


rowLegalization :: Double -> NetGraph -> Vector Gate -> ST s (Vector Gate)
rowLegalization  _   _ gs | length gs < 2 = pure gs
rowLegalization  _   _ gs | all (view fixed) gs = pure gs
rowLegalization rc top gs = do

    let row = top ^. supercell . rows ^? ix (gs ! 0 ^. space . b)

    let res = maybe 1 (view granularity) row
        off = div (maybe 0 (view l) row) res - 1

    let w = (`div` res) .  width . view space <$> gs
        x = (flip (-) off) . (`div` res) . view l . view space <$> gs

    -- let n = head (top ^. supercell . geometry <&> width) `div` res
    let n = maybe 0 (view cardinality) row + maximum w
        m = length gs

    -- let clearArea = ceiling $ fromIntegral n * (1 - rc) * 0.5

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
      -- for_ [ 1 + sum (Vector.take j w) .. n] $ \ k -> do

        let u1 = vertex j (pred k)
        let v1 = vertex j k

        dv1 <- ST.read d v1
        du1 <- ST.read d u1

        when (dv1 > du1)
          $ do
            ST.write d v1 du1
            ST.write p v1 u1

        unless (j < 1)
          $ unless (k > n - w ! pred j)
          $ unless (gs ! pred j ^. fixed && x ! pred j /= k)
          -- $ when (gs ! pred j ^. fixed || k > clearArea && k < n - clearArea)
          $ do

            let u2 = vertex (pred j) k
            let v2 = vertex j (k + w ! pred j)

            let w2 = abs $ x ! pred j - k

            dv2 <- ST.read d v2
            du2 <- ST.read d u2

            when (dv2 > du2 + w2)
              $ do
                ST.write d v2 $ du2 + w2
                ST.write p v2 u2

    shortestPath <- execWriter . traversePath target <$> Unbox.unsafeFreeze p

    let leftMost = fmap head $ groupBy (on (==) fst) $ site <$> shortestPath

    assert "cannot legalize row!" $ tail leftMost /= [(0, 0)]

    pure $ gs //
      [ (g, gs ! g & space %~ relocateL ((u + off) * res))
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

    let rowWidth p = ceiling $ rc * fromIntegral (view cardinality p * view granularity p)
    let w = fmap (maybe 0 rowWidth . flip preview rs . ix . view (space . b) . head) table
    let y = fmap (view (space . b) . head) table

    surplus <- unsafeThaw $ uncurry (-) <$> Vector.zip (sum . fmap gateWidth <$> table) w

    matrix <- Vector.thaw table

    ranked <- Vector.indexed <$> Vector.freeze surplus

    for_ (Vector.filter (\ (_, s) -> s > 0) ranked) $ \ (i, _) -> do

      whileM_ ((0 <) <$> read surplus i)
        $ do

          bestHpwl <- newSTRef maxBound
          bestRow  <- newSTRef (-1)
          bestCell <- newSTRef def

          cs <- read matrix i

          for_ cs $ \ c -> do
            for_ [ 0 .. length table - 1 ] $ \ k -> do

              let g = c & space %~ relocateB (y ! k)
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
    let rowWidth p = fromIntegral (view cardinality p * view granularity p)
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

