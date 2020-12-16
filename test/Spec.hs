-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Category
import Control.Arrow
import Control.Arrow.Select
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import Data.Bits
import Data.Default
import Data.FileEmbed
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (fromList, fromAscList, size)
import Data.List (sort, isPrefixOf)
import Data.Ratio
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (replicate, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Prelude hiding (id, replicate, (.))

import System.Environment

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC
import LSC.BLIF
import LSC.Entropy
import LSC.FIFO (enqueue, deq)
import LSC.Types hiding (assert)
import LSC.UnionFind
import LSC.FM as FM
import LSC.KGGGP as KGGGP hiding (V, E)



main :: IO ()
main = do
  a <- getArgs
  defaultMain $ lsc a


lsc :: [String] -> TestTree
lsc args = testGroup "LSC" $
  [ medians
  , fifo
  , unionFind
  , entropy
  , gggp
  , fm
  ] ++ [ concurrency | isPrefixOf "-j" `any` args ]



medians :: TestTree
medians = testGroup "Types"
  [ testCase "Median random input" $ do
      rng <- create
      med <- generate $ choose (1, 100000)
      assertEqual "even median" (med - 1) . median . sort . toList =<< randomPermutation (2 * med) rng
      assertEqual "odd median" med . median . sort . toList =<< randomPermutation (2 * med + 1) rng
  , testCase "Median large input" $ assertEqual "is median" 5000000 $ median [0..10000000 :: Int]
  ]



fifo :: TestTree
fifo = testGroup "FIFO"
  [ testCase "Enqueue" $ do
      let q = enqueue 3 $ enqueue 2 $ enqueue 1 $ mempty
      assertEqual "is first element" (1 :: Int) $ fst $ deq q
      assertEqual "is sum" 6 $ sum q
      assertEqual "is maximum" 3 $ maximum q
      assertEqual "is length" 3 $ length q
  ]



unionFind :: TestTree
unionFind = testGroup "UnionFind"
  [ testCase "Simple union" $ do
      (p1, p2) <- stToIO $ do
        disjoint <- newDisjointSet
        union disjoint (1 :: Int) 2
        (,) <$> equivalent disjoint 1 2 <*> equivalent disjoint 1 3
      assertEqual "" True $ p1 && not p2
  , testCase "Complex union" $ do
      (p1, p2, p3, p4) <- stToIO $ do
        disjoint <- newDisjointSet
        union disjoint (1 :: Int) 2
        union disjoint 1 4
        union disjoint 2 6
        union disjoint 2 5
        union disjoint 7 9
        union disjoint 7 10
        union disjoint 7 2
        _ <- equivalent disjoint 7 10
        (,,,)
          <$> equivalent disjoint 2 4
          <*> equivalent disjoint 3 4
          <*> equivalent disjoint 30 31
          <*> equivalent disjoint 35 35
      assertEqual "" True $ p1 && not p2 && not p3 && p4
  ]



entropy :: TestTree
entropy = testGroup "Entropy"
  [ testCase "Random permutation" testRandomPermutation
  ]



gggp :: TestTree
gggp = testGroup "KGGGP"
  [ testCase "Insert Gain" kgggpInsertGain
  , testCase "Remove Gain" kgggpRemoveGain
  , testCase "Modify Gain" kgggpModifyGain
  , testCase "Empty Gains" kgggpEmptyGains
  ]



fm :: TestTree
fm = testGroup "FM"
  [ testCase "Input routine" fmInputRoutine
  , testCase "Deterministic" fmDeterministic
  , testCase "Balance criterion" fmBalanceCriterion
  , fmML
  , fmLocks
  ]



fmML :: TestTree
fmML = testGroup "Multi level"
  [ testCase "Match" fmMatch
  , testCase "Rebalance" $ sequence_ $ replicate 100 $ fmRebalance
  , fmRealWorld
  ]

fmRealWorld :: TestTree
fmRealWorld = testGroup "Real world instances"
  [ testCase "queue_1.blif"     $ fmMulti   7 =<< stToIO queue_1Hypergraph
  ]


fmLocks :: TestTree
fmLocks = testGroup "Locks"
  [ testCase "Even bipartition" fmEvenPartition
  , testCase "Random bipartition" fmRandomPartition
  ]



concurrency :: TestTree
concurrency = testGroup "Concurrency" $
  [ testCase "Dual core" $ dualCore n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Quad core" $ quadCore n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Stream processor" $ stream n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Remote procedure" $ remoteProcess n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Race condition" $ raceCondition n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ]



testRandomPermutation :: IO ()
testRandomPermutation = do
  n <- generate $ choose (1000, 10000)
  v <- pure $ V.generate n id
  u <- randomPermutation n =<< create
  a <- V.thaw u
  V.sort a
  w <- V.freeze a

  assertBool "permutation" $ u /= v
  assertBool "sorted"      $ w == v



-- | KGGGP
-- 
kgggpInsertGain :: IO ()
kgggpInsertGain = do

    x <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 5 3 4 bs
      maximumGain bs
    assertEqual "maximum gain" x (5, 3)



kgggpRemoveGain :: IO ()
kgggpRemoveGain = do

    x <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 2 3 4 bs
      insertGain 5 3 4 bs
      _ <- KGGGP.removeGain 5 3 bs
      maximumGain bs
    assertEqual "maximum gain" x (2, 3)



kgggpModifyGain :: IO ()
kgggpModifyGain = do

    (x, g) <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      insertGain 3 3 5 bs
      insertGain 2 3 4 bs
      insertGain 1 2 2 bs
      KGGGP.modifyGain 2 3 succ bs
      (,) <$> maximumGain bs <*> KGGGP.removeGain 2 3 bs
    assertEqual "gain" g (Just 5)
    assertEqual "maximum gain" x (2, 3)



kgggpEmptyGains :: IO ()
kgggpEmptyGains = do

    (x, y) <- stToIO $ do
      (v, _) <- queue_1Hypergraph
      bs <- newGains v "bs" 4
      KGGGP.insertGain 3 3 5 bs
      _ <- KGGGP.removeGain 3 3 bs
      x <- emptyGains bs
      KGGGP.insertGain 3 3 5 bs
      y <- emptyGains bs
      pure (x, y)

    assertBool "empty" x
    assertBool "not empty" $ not y



-- | FM
-- 

fmEvenPartition :: IO ()
fmEvenPartition = do

    (v, e) <- arbitraryHypergraph 1000

    xs <- generate $ vectorOf 20 $ choose (0, length v - 1)
    let Bisect p q = bipartitionEven (v, e) (Lock (fromList xs) mempty)
    assertEqual "duplicates" (size p + size q) (length v)

    ys <- generate $ vectorOf 20 $ choose (0, length v - 1)
    let Bisect p1 q1 = bipartitionEven (v, e) (Lock mempty (fromList ys))
    assertEqual "duplicates" (size p1 + size q1) (length v)




fmRandomPartition :: IO ()
fmRandomPartition = do

    (v, e) <- arbitraryHypergraph 1000

    xs <- generate $ vectorOf 20 $ choose (0, length v - 1)
    Bisect p q <- nonDeterministic $ runFMWithGen $ bipartitionRandom (v, e) (Lock (fromList xs) mempty)
    assertEqual "duplicates" (size p + size q) (length v)

    ys <- generate $ vectorOf 20 $ choose (0, length v - 1)
    Bisect p1 q1 <- nonDeterministic $ runFMWithGen $ bipartitionRandom (v, e) (Lock mempty (fromList ys))
    assertEqual "duplicates" (size p1 + size q1) (length v)




fmMulti :: Int -> (V, E) -> IO ()
fmMulti cut h = do
  let predicate p = cutSize h p <= cut
  p <- iterateUntil predicate $ nonDeterministic $ runFMWithGen
    $ fmMultiLevel h mempty coarseningThreshold matchingRatio
  assertBool "unexpected cut size" $ cutSize h p <= cut



fmMatch :: IO ()
fmMatch = do
  (v, e) <- arbitraryHypergraph 10000
  (clustering, _) <- nonDeterministic $ runFMWithGen $ do
      u <- FM.st . randomPermutation (length v) =<< prng
      FM.st $ match (v, e) mempty matchingRatio u

  assertEqual "length does not match" (length v) (sum $ size <$> clustering)
  assertBool "elements do not match" $ foldMap id clustering == fromAscList [0 .. length v - 1]
  assertBool "clustering" $ length clustering <= length v



fmRebalance :: IO ()
fmRebalance = do
  let v = 10000
  pivot <- generate $ choose (0, v)
  (p, q) <- nonDeterministic $ runFMWithGen $ do
      u <- FM.st . randomPermutation v =<< prng
      let p = Bisect
            (fromList [u!i | i <- [0 .. pivot-1]])
            (fromList [u!i | i <- [pivot .. v-1]])
      (p, ) <$> rebalance p
  let it = show (bisectBalance p, bisectBalance q)
  assertBool it $ bisectBalance q % v < 5%8



fmInputRoutine :: IO ()
fmInputRoutine = void $ arbitraryHypergraph 10000



fmBalanceCriterion :: IO ()
fmBalanceCriterion = do

  assertBool "t1" $ balanced 420 0 $ bisect [1 .. 210] [211 .. 420]
  assertBool "t2" $ not $ balanced 420 0 $ bisect [1 .. 105] [106 .. 420]
  assertBool "t3" $ not $ balanced 420 0 $ bisect [1 .. 319] [320 .. 420]

  assertBool "t4" $ balanced 42 0 $ bisect [1 .. 20] [21 .. 42]
  assertBool "t5" $ not $ balanced 42 0 $ bisect [1 .. 10] [11 .. 42]
  assertBool "t6" $ not $ balanced 42 0 $ bisect [1 .. 31] [32 .. 42]

  where

    balanced v = flip $ balanceCriterion v
    bisect p q = Bisect (fromAscList p) (fromAscList q)



fmDeterministic :: IO ()
fmDeterministic = do
  h <- stToIO queue_1Hypergraph
  p <- stToIO $ evalFM $ bipartition h mempty $ bipartitionEven h mempty
  assertEqual "cut size" 9 $ cutSize h p



blifHypergraph :: BLIF -> ST s (V, E)
blifHypergraph netlist = FM.inputRoutine
    (top ^. nets . to length)
    (top ^. gates . to length)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList $ top ^. nets
    , c <- w ^. contacts . to HashMap.keys
    ] where top = fromBLIF netlist



arbitraryHypergraph :: Int -> IO (V, E)
arbitraryHypergraph n = do
  k <- generate $ choose (1, n)
  y <- generate $ choose (k, k+n)
  config <- generate $ vectorOf (4 * k) $ (,) <$> choose (0, pred y) <*> choose (0, pred k)
  stToIO $ FM.inputRoutine y k config


queue_1Hypergraph :: ST s (V, E)
queue_1Hypergraph = either (fail . show) blifHypergraph $ parseBLIF queue_1Blif


queue_1Blif :: Text
queue_1Blif = decodeUtf8 $(embedFile "sample/queue_1.blif")



-- | Concurrency
--
dualCore :: Int -> IO ()
dualCore n = do
  counter <- newMVar 0
  ws <- createWorkers 2
  let inc = local_ $ incrementWithDelay (2*n) counter
      act = fst ^<< inc &&& inc &&& inc &&& inc &&& inc &&& inc
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay (3*n)
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  result1 @?= 2
  result2 @?= 4


quadCore :: Int -> IO ()
quadCore n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = local_ $ incrementWithDelay (2*n) counter
      act = fst . snd ^<< (inc &&& inc) &&& inc &&& (inc &&& inc &&& inc)
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay (3*n)
  result1 <- readMVar counter
  threadDelay (5*n)
  result2 <- readMVar counter
  result1 @?= 4
  result2 @?= 6


stream :: Int -> IO ()
stream n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = local_ $ incrementWithDelay (2*n) counter
      act = select inc
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ () <$ compiler act (replicate 8 ())
  threadDelay (3*n)
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  result1 @?= 4
  result2 @?= 8


remoteProcess :: Int -> IO ()
remoteProcess n = do
  counter1 <- newMVar 0
  counter2 <- newMVar 0
  ws <- createWorkers 2
  let inc1 = local_ $ incrementWithDelay (2*n) counter1
      inc2 = remote_ $ incrementWithDelay (2*n) counter2
      act = fst ^<< inc1 &&& inc1 &&& inc2 &&& inc1 &&& inc2 &&& inc1 &&& inc1 &&& inc1 &&& inc1 &&& inc1 &&& inc1
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ () <$ compiler act mempty
  threadDelay (7*n)
  result1 <- readMVar counter1
  result2 <- readMVar counter2
  result1 @?= 6
  result2 @?= 2


raceCondition :: Int -> IO ()
raceCondition n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc k = local_ $ incrementWithDelay (2*k*n) counter
      act = inc 1 \\\ inc 2 \\\ inc 3 \\\ inc 4 \\\ inc 5 \\\ inc 6
      tech = thaw def
      opts = def & workers .~ ws
  runLSC opts tech $ compiler act mempty
  result1 <- readMVar counter
  threadDelay (8*n)
  result2 <- readMVar counter
  result1 @?= 1
  result1 @?= result2


incrementWithDelay :: Int -> MVar Int -> LSC ()
incrementWithDelay n counter = liftIO $ do
  threadDelay n
  modifyMVar_ counter $ pure . succ


