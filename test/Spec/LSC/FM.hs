-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Spec.LSC.FM
    ( fm
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.FileEmbed
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (fromList, fromAscList, size)
import Data.Ratio
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (replicate, (!))
import Prelude hiding (replicate)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.BLIF
import LSC.Entropy
import LSC.Types
import LSC.FM




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
      u <- st . randomPermutation (length v) =<< prng
      st $ match (v, e) mempty matchingRatio u

  assertEqual "length does not match" (length v) (sum $ size <$> clustering)
  assertBool "elements do not match" $ foldMap id clustering == fromAscList [0 .. length v - 1]
  assertBool "clustering" $ length clustering <= length v



fmRebalance :: IO ()
fmRebalance = do
  let v = 10000
  pivot <- generate $ choose (0, v)
  (p, q) <- nonDeterministic $ runFMWithGen $ do
      u <- st . randomPermutation v =<< prng
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
blifHypergraph netlist = inputRoutine
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
  stToIO $ inputRoutine y k config


queue_1Hypergraph :: ST s (V, E)
queue_1Hypergraph = either (fail . show) blifHypergraph $ parseBLIF queue_1Blif


queue_1Blif :: Text
queue_1Blif = decodeUtf8 $(embedFile "sample/queue_1.blif")


